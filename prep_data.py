# %% env tobler
import geopandas as gpd
from tobler.area_weighted import area_interpolate

# Path to geopackage
gpkg_path = "./data/dc_census_tracts_complete.gpkg"

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
    results.to_file(f"./data/final_census_tracts_2003.gpkg", layer=layer, driver="GPKG")

# %%
results = gpd.read_file(f"./data/final_census_tracts_2003.gpkg", layer="decennial_2000")
results.explore("median_income")

# %% get historic designation data

# TO DO there are only 54 matches out of 69 bounds

hd_bounds = gpd.read_file("./data/Historic_Districts.geojson").to_crs("EPSG:32618")
hd_years = pd.read_csv("./data/hd_boundary_data.csv")
hd = hd_bounds.merge(hd_years[["UNIQUEID", "YEAR"]], on="UNIQUEID", how="left")

hd.loc[hd["UNIQUEID"] == "D_080", "YEAR"] = 2016
hd.loc[hd["UNIQUEID"] == "D_082", "YEAR"] = 2021
hd.loc[hd["UNIQUEID"] == "D_031", "YEAR"] = 1973
hd.loc[hd["UNIQUEID"] == "D_043", "YEAR"] = 1981
hd.loc[hd["UNIQUEID"] == "D_044", "YEAR"] = 1973
hd.loc[hd["UNIQUEID"] == "D_056", "YEAR"] = 1973
hd.loc[hd["UNIQUEID"] == "D_057", "YEAR"] = 1964
hd.loc[hd["UNIQUEID"] == "D_060", "YEAR"] = 1964
hd.loc[hd["UNIQUEID"] == "D_063", "YEAR"] = 1964
hd.loc[hd["UNIQUEID"] == "D_016", "YEAR"] = 1964


hd.to_crs("EPSG:32618").to_file("./data/hd_with_years.geojson", driver="GeoJSON")
display(hd.head())
hd.shape
# %%
