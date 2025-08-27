# %%
import geopandas as gpd
from tobler.area_weighted import area_interpolate

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

# %% remove water and public lands etc
water = gpd.read_file("./data/Waterbodies.geojson").to_crs("EPSG:32618")
national_parks = gpd.read_file("./data/National_Parks.geojson").to_crs("EPSG:32618")
dc_parks = gpd.read_file("./data/Parks_and_Recreation_Areas.geojson").to_crs(
    "EPSG:32618"
)

for layer in layers:
    print(f"Processing layer: {layer}")
    gdf = gpd.read_file(gpkg_path, layer=layer).to_crs("EPSG:32618")
    # Remove water and public lands from 2023 tracts
    gdf = gpd.overlay(gdf, water, how="difference")
    gdf = gpd.overlay(gdf, national_parks, how="difference")
    gdf = gpd.overlay(gdf, dc_parks, how="difference")
    gdf.to_file(f"cleaned_census_tracts.gpkg", layer=layer, driver="GPKG")
    # %%
    source_df = gpd.read_file(gpkg_path, layer="decennial_2000")
source_df
# %%
gpkg_path = "./data/cleaned_census_tracts.gpkg"


results = area_interpolate(
    source_df=gpd.read_file(gpkg_path, layer="decennial_2000"),
    target_df=gpd.read_file(gpkg_path, layer="acs_2023"),
    intensive_variables=[
        "median_income",
        "median_home_value",
        "low_quart_rent",
        "median_rent",
        "upper_quart_rent",
    ],
    extensive_variables=[
        "total_pop",
        "white_alone",
        "black_alone",
        "asian_alone",
        "hispanic",
        "total_housing_units",
        "owner_occupied",
    ],
)
# %%
results.explore("median_income")
# %%
gpd.read_file(gpkg_path, layer="decennial_2000").explore("median_income")
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
    old_gdf["old_GEOID"] = old_gdf["GEOID"]
    new_gdf["new_GEOID"] = new_gdf["GEOID"]
    intersected = gpd.overlay(old_gdf, new_gdf, how="intersection", make_valid=True)
    intersected["intersect_area"] = intersected.geometry.area
    # For sum, allocate by area proportion
    if agg_method == "sum":
        intersected["reallocated_value"] = intersected[value_col] * (
            intersected["intersect_area"] / intersected["old_area"]
        )
        result = (
            intersected.groupby(intersected.new_GEOID)["reallocated_value"]
            .sum()
            .reset_index()
        )
        result.rename(
            columns={
                "reallocated_value": value_col,
                "new_GEOID": "GEOID",
            },
            inplace=True,
        )
        result.drop(
            columns=["new_GEOID", "old_GEOID", "old_area"],
            inplace=True,
            errors="ignore",
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
        result.drop(
            columns=["new_GEOID", "old_GEOID", "old_area"],
            inplace=True,
            errors="ignore",
        )
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
