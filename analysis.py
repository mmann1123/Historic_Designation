# %%
import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.regression.linear_model import OLS
from statsmodels.tools.tools import add_constant
import statsmodels.formula.api as smf
from scipy import stats
import os
from tqdm import tqdm

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
variables = [
    "total_pop",  # Total population
    "median_income",  # Median household income
    "white_alone",  # White alone
    "black_alone",  # Black alone
    "asian_alone",  # Asian alone
    "hispanic",  # Hispanic or Latino
    "median_home_value",  # Median value (owner-occupied housing units)
    "low_quart_rent",  # Low quartile rent
    "median_rent",  # Median rent
    "upper_quart_rent",  # Upper quartile rent
    "total_housing_units",  # Total housing units
    "owner_occupied",  # Owner-occupied housing units
    "renter_occupied",  # Renter-occupied housing units
]

# Path to data
gpkg_path = "./data/final_census_tracts_2003.gpkg"
hd_path = "./data/hd_with_years.geojson"

# Define variables of interest
rent_vars = ["median_rent", "low_quart_rent", "upper_quart_rent"]
ethnic_vars = ["white_alone", "black_alone", "asian_alone", "hispanic", "total_pop"]
# %%
