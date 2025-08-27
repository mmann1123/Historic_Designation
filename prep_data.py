# %%
import geopandas as gpd
import pandas as pd

# Path to geopackage
gpkg_path = "./data/dc_census_tracts_complete.gpkg"

# Layers to process (before 2020)
layers = [
    "decennial_2000",
    "acs_2009",
    "acs_2010",
    "acs_2011",
    "acs_2012",
    "acs_2013",
    "acs_2014",
    "acs_2015",
    "acs_2016",
    "acs_2017",
    "acs_2018",
    "acs_2019",
    "acs_2020",
    "acs_2021",
    "acs_2022",
    "acs_2023",
]

# Read 2023 boundaries
tracts_2023 = gpd.read_file(gpkg_path, layer="acs_2023")
tracts_2023.explore(column="median_rentE")

# %% remove water and public lands etc
water = gpd.read_file("./data/Waterbodies.geojson").to_crs("EPSG:32618")
national_parks = gpd.read_file("./data/National_Parks.geojson").to_crs("EPSG:32618")
dc_parks = gpd.read_file("./data/Parks_and_Recreation_Areas.geojson").to_crs(
    "EPSG:32618"
)

for layer in layers:
    gdf = gpd.read_file(gpkg_path, layer=layer).to_crs("EPSG:32618")
    # Remove water and public lands from 2023 tracts
    gdf = gpd.overlay(gdf, water, how="difference")
    gdf = gpd.overlay(gdf, national_parks, how="difference")
    gdf = gpd.overlay(gdf, dc_parks, how="difference")
    gdf.to_file(f"cleaned_census_tracts.gpkg", layer=layer, driver="GPKG")

# %%
tracts_2023 = gpd.read_file("cleaned_census_tracts.gpkg", layer="acs_2023")
tracts_2023.explore()
# %%


# Function to drop empty/outlier tracts
def clean_tracts(gdf, value_col):
    gdf = gdf[gdf[value_col].notnull() & (gdf[value_col] > 0)]
    q1 = gdf[value_col].quantile(0.25)
    q3 = gdf[value_col].quantile(0.75)
    iqr = q3 - q1
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    gdf = gdf[(gdf[value_col] >= lower) & (gdf[value_col] <= upper)]
    return gdf


# Dasymetric reallocation for sum and mean
def dasymetric_reallocate(old_gdf, new_gdf, value_col, agg_method):
    old_gdf["old_area"] = old_gdf.geometry.area
    intersected = gpd.overlay(old_gdf, new_gdf, how="intersection")
    intersected["intersect_area"] = intersected.geometry.area
    # For sum, allocate by area proportion
    if agg_method == "sum":
        intersected["reallocated_value"] = intersected[value_col] * (
            intersected["intersect_area"] / intersected["old_area"]
        )
        result = (
            intersected.groupby(intersected.index_right)["reallocated_value"]
            .sum()
            .reset_index()
        )
        result.rename(
            columns={"reallocated_value": value_col, "index_right": "index_right"},
            inplace=True,
        )
    elif agg_method == "mean":
        intersected["weighted_value"] = (
            intersected[value_col] * intersected["intersect_area"]
        )
        grouped = (
            intersected.groupby(intersected.index_right)
            .agg({"weighted_value": "sum", "intersect_area": "sum"})
            .reset_index()
        )
        grouped["reallocated_value"] = (
            grouped["weighted_value"] / grouped["intersect_area"]
        )
        result = grouped[["index_right", "reallocated_value"]]
        result.rename(columns={"reallocated_value": value_col}, inplace=True)
    else:
        raise ValueError(f"Unknown aggregation method: {agg_method}")
    return result


# Variables and aggregation methods
variables = {
    "total_pop": "sum",
    "median_income": "mean",
    "white_alone": "sum",
    "black_alone": "sum",
    "asian_alone": "sum",
    "hispanic": "sum",
    "median_home_value": "mean",
    "low_quart_rent": "mean",
    "median_rent": "mean",
    "upper_quart_rent": "mean",
    "total_housing_units": "sum",
    "owner_occupied": "sum",
    "renter_occupied": "sum",
}

# Main loop for all layers and variables
for layer in layers:
    old_gdf = gpd.read_file(gpkg_path, layer=layer)
    output = tracts_2023.copy()
    for var, agg in variables.items():
        if var in old_gdf.columns:
            cleaned = clean_tracts(old_gdf, var)
            reallocated = dasymetric_reallocate(cleaned, tracts_2023, var, agg)
            output = output.merge(
                reallocated, left_index=True, right_on="index_right", how="left"
            )
    output.to_file(f"reallocated_{layer}_to_2023.gpkg", layer=layer, driver="GPKG")
