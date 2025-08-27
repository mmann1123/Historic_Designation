# %% env tobler
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

rename_dict = {
    "total_popE": "total_pop",  # Total population
    "median_incomeE": "median_income",  # Median household income
    "white_aloneE": "white_alone",  # White alone
    "black_aloneE": "black_alone",  # Black alone
    "asian_aloneE": "asian_alone",  # Asian alone
    "hispanicE": "hispanic",  # Hispanic or Latino
    "median_home_valueE": "median_home_value",  # Median value (owner-occupied housing units)
    "low_quart_rentE": "low_quart_rent",  # Low quartile rent
    "median_rentE": "median_rent",  # Median rent
    "upper_quart_rentE": "upper_quart_rent",  # Upper quartile rent
    "total_housing_unitsE": "total_housing_units",  # Total housing units
    "owner_occupiedE": "owner_occupied",  # Owner-occupied housing units
    "renter_occupiedE": "renter_occupied",  # Renter-occupied housing units
}

for layer in layers:
    print(f"Processing layer: {layer}")
    gdf = gpd.read_file(gpkg_path, layer=layer).to_crs("EPSG:32618")
    # Remove water and public lands from 2023 tracts
    gdf = gpd.overlay(gdf, water, how="difference")
    gdf = gpd.overlay(gdf, national_parks, how="difference")
    gdf = gpd.overlay(gdf, dc_parks, how="difference")
    # drop slivers
    gdf = gdf.explode(index_parts=False)
    gdf.rename(columns=rename_dict, inplace=True, errors="ignore")
    gdf = gdf[gdf.area > 1000]
    gdf = gdf[gdf["median_income"] > 0]
    gdf = gdf.dissolve(by="GEOID", as_index=False)

    gdf.to_file(f"./data/cleaned_census_tracts.gpkg", layer=layer, driver="GPKG")
# %%
gpkg_path = "./data/cleaned_census_tracts.gpkg"

source_df = gpd.read_file(gpkg_path, layer="decennial_2000")
source_df.explore("total_pop")
# %%
gpkg_path = "./data/cleaned_census_tracts.gpkg"

for layer in layers:
    print(f"Processing layer: {layer}")
    results = area_interpolate(
        source_df=gpd.read_file(gpkg_path, layer=layer),
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
    gdf.to_file(f"./data/final_census_tracts_2003.gpkg", layer=layer, driver="GPKG")

# %%
results.explore("median_income")
# %%
gpd.read_file(gpkg_path, layer="decennial_2000").explore("median_income")
# %%
results.explode().area.describe()
# %%
