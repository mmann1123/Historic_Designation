# Analysis of Historic Designation Effects on Rent and Ethnic Composition
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

# Path to data
gpkg_path = "./data/final_census_tracts_2003.gpkg"
hd_path = "./data/hd_with_years.geojson"

# Define variables of interest
rent_vars = ["median_rent", "low_quart_rent", "upper_quart_rent"]
ethnic_vars = ["white_alone", "black_alone", "asian_alone", "hispanic", "total_pop"]
census_years = [
    2000,
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017,
    2018,
    2019,
    2020,
    2021,
    2022,
    2023,
]

# %%
# Load historic district data
print("Loading historic district data...")
hd = gpd.read_file(hd_path)
# Filter to districts designated between 2000 and 2023
hd = hd[(hd["YEAR"] >= 2000) & (hd["YEAR"] <= 2023)]
print(f"Number of historic districts implemented between 2000-2023: {len(hd)}")

# %%
# Load all census years
print("Loading census data for all years...")
census_data = {}


def load_census_year(year):
    if year == 2000:
        layer_name = "decennial_2000"
    else:
        layer_name = f"acs_{year}"

    try:
        df = gpd.read_file(gpkg_path, layer=layer_name)
        df["year"] = year
        return df
    except Exception as e:
        print(f"Error loading {layer_name}: {e}")
        return None


for year in census_years:
    census_data[year] = load_census_year(year)
    if census_data[year] is not None:
        print(f"Loaded {year} with {len(census_data[year])} tracts")


# %%
# Function to determine if a tract is in a historic district by a given year
def identify_hd_tracts():
    print("Identifying historic district tracts for each year...")
    # Start with the latest census data for the geometry
    base_tracts = census_data[max(census_years)].copy()

    # Create a dictionary to store HD status by year
    hd_status = {}

    # Initialize all tracts as non-HD for all years
    for year in census_years:
        hd_status[year] = pd.Series(False, index=base_tracts.index)

    # For each historic district, identify which tracts it intersects
    for idx, hd_row in tqdm(hd.iterrows(), total=len(hd)):
        hd_year = hd_row["YEAR"]
        hd_geom = hd_row.geometry

        # Find intersecting tracts
        for i, tract in base_tracts.iterrows():
            if tract.geometry.intersects(hd_geom):
                # Calculate intersection area as proportion of tract area
                intersection = tract.geometry.intersection(hd_geom)
                proportion = intersection.area / tract.geometry.area

                # If a significant portion (>10%) of the tract is in the HD
                if proportion > 0.1:
                    # Mark this tract as HD for years after designation
                    for year in census_years:
                        if year >= hd_year:
                            hd_status[year][i] = True
        print(f"Total number found in year {hd_year}: { sum(hd_status[year]) }")

    return hd_status


# %%
# Get HD status for all tracts by year
hd_status = identify_hd_tracts()

# %%
# Prepare panel data for difference-in-differences analysis
print("Preparing panel data for analysis...")
panel_data = []

for year in census_years:
    if census_data[year] is not None:
        df = census_data[year].copy()
        df["hd"] = hd_status[year]
        df["year"] = year
        df["fid"] = df.index

        # Calculate non-white/non-asian percentage
        if all(
            col in df.columns for col in ["total_pop", "white_alone", "asian_alone"]
        ):
            df["nonwhite_nonasian_pct"] = (
                100
                * (df["total_pop"] - df["white_alone"] - df["asian_alone"])
                / df["total_pop"]
            )

        panel_data.append(df)

# Combine into a single dataframe
panel_df = pd.concat(panel_data, ignore_index=True)

# Create lag variables to analyze changes
print("Creating variables for analysis...")
# Create unique tract ID
panel_df["tract_id"] = panel_df["fid"]

# %%
panel_df.drop(columns=["geometry"]).to_csv("./data/panel_data.csv", index=False)

# %%
# Difference-in-Differences Analysis
print("\n===== Difference-in-Differences Analysis =====")


# Function to run DiD for a variable
def run_did_analysis(var, pre_period=2010, min_post_period=2016):
    print(f"\nRunning DiD for {var}...")

    # Create panel data in long format for analysis
    panel_long = panel_df[["tract_id", "year", "hd", var]].dropna()

    # Create treatment and post period indicators
    panel_long["treated"] = panel_long["hd"]
    panel_long["post"] = panel_long["year"] > pre_period
    panel_long["treated_post"] = panel_long["treated"] * panel_long["post"]

    # Filter to relevant years
    panel_long = panel_long[
        (panel_long["year"] == pre_period) | (panel_long["year"] >= min_post_period)
    ]

    # Run regression
    formula = f"{var} ~ treated + post + treated_post"
    model = smf.ols(formula, data=panel_long).fit(
        cov_type="cluster", cov_kwds={"groups": panel_long["tract_id"]}
    )

    print(model.summary().tables[1])
    return model


# Run DiD for key variables
for var in rent_vars + ["nonwhite_nonasian_pct"]:
    if var in panel_df.columns:
        model = run_did_analysis(var)

        # Interpretation of the DiD coefficient
        coef = model.params["treated_post[T.True]"]
        pvalue = model.pvalues["treated_post[T.True]"]
        sig = "significant" if pvalue < 0.05 else "not significant"

        print(f"\nInterpretation for {var}:")
        print(f"The DiD coefficient is {coef:.2f} (p={pvalue:.4f}), which is {sig}")
        if var == "nonwhite_nonasian_pct":
            if coef > 0:
                print(
                    "Historic designation is associated with an increase in non-white/non-Asian population percentage"
                )
            else:
                print(
                    "Historic designation is associated with a decrease in non-white/non-Asian population percentage"
                )
        else:  # Rent variables
            if coef > 0:
                print(
                    "Historic designation is associated with an increase in rent prices"
                )
            else:
                print(
                    "Historic designation is associated with a decrease in rent prices"
                )

# %%
# Visualizations
print("\n===== Creating Visualizations =====")

# Create a directory for output figures
os.makedirs("./figures", exist_ok=True)


# Plot trend lines for treated vs control groups
def plot_trends(var, title):
    plt.figure(figsize=(12, 6))

    # Calculate average by year and HD status
    avg_by_group = panel_df.groupby(["year", "hd"])[var].mean().reset_index()

    # Plot
    sns.lineplot(data=avg_by_group, x="year", y=var, hue="hd", marker="o", linewidth=2)

    plt.title(f"{title} Over Time by Historic District Status")
    plt.xlabel("Year")
    plt.ylabel(title)
    plt.legend(["Non-HD Areas", "Historic Districts"])
    plt.grid(True, alpha=0.3)
    plt.tight_layout()

    # Save figure
    plt.savefig(f"./figures/{var}_trends.png", dpi=300)
    plt.close()
    print(f"Saved {var} trend plot to ./figures/{var}_trends.png")


# %%
# Plot trends for key variables
for var, title in zip(
    rent_vars + ["nonwhite_nonasian_pct"],
    [
        "Median Rent",
        "Lower Quartile Rent",
        "Upper Quartile Rent",
        "Non-White/Non-Asian Population (%)",
    ],
):
    if var in panel_df.columns:
        plot_trends(var, title)

# %%
# Structural Break Analysis
print("\n===== Structural Break Analysis =====")

# Identify tracts that transitioned to HD during our time period
transitioning_tracts = []
for tract_id in panel_df["tract_id"].unique():
    # Get HD status for this tract across years
    hd_status_by_year = (
        panel_df[panel_df["tract_id"] == tract_id].sort_values("year")["hd"].values
    )
    # Check if it transitions from False to True
    if (
        len(hd_status_by_year) > 0
        and not all(hd_status_by_year)
        and any(hd_status_by_year)
    ):
        transitioning_tracts.append(tract_id)

print(
    f"Found {len(transitioning_tracts)} tracts that transitioned to HD status during 2000-2023"
)


# %%
# Function to test for structural breaks around HD designation
def test_structural_breaks():
    results = {}

    for tract_id in transitioning_tracts:
        # Get data for this tract across all years
        tract_data = panel_df[panel_df["tract_id"] == tract_id].sort_values("year")

        # Find the year this tract became an HD
        hd_years = tract_data[tract_data["hd"]]["year"].values
        if len(hd_years) == 0:
            continue

        hd_year = min(hd_years)

        # For each variable, test if there's a change in trend after HD designation
        for var in rent_vars + ["nonwhite_nonasian_pct"]:
            if var not in tract_data.columns:
                continue

            # We need enough data points before and after
            pre_hd = tract_data[tract_data["year"] < hd_year][var].dropna().values
            post_hd = tract_data[tract_data["year"] >= hd_year][var].dropna().values

            if len(pre_hd) >= 3 and len(post_hd) >= 3:
                # Test for difference in means
                t_stat, p_value = stats.ttest_ind(pre_hd, post_hd, equal_var=False)

                # Record result
                if tract_id not in results:
                    results[tract_id] = {}
                results[tract_id][var] = {
                    "hd_year": hd_year,
                    "t_stat": t_stat,
                    "p_value": p_value,
                    "pre_mean": np.mean(pre_hd),
                    "post_mean": np.mean(post_hd),
                    "percent_change": 100
                    * (np.mean(post_hd) - np.mean(pre_hd))
                    / np.mean(pre_hd),
                }

    return results


# %%
# Run structural break analysis
break_results = test_structural_breaks()

# Summarize results
print("\nStructural Break Analysis Results:")
significant_breaks = {var: 0 for var in rent_vars + ["nonwhite_nonasian_pct"]}
total_tests = {var: 0 for var in rent_vars + ["nonwhite_nonasian_pct"]}

for tract_id, vars_results in break_results.items():
    for var, result in vars_results.items():
        total_tests[var] += 1
        if result["p_value"] < 0.05:
            significant_breaks[var] += 1
            print(
                f"Tract {tract_id}, {var}: Significant break at HD year {result['hd_year']}, "
                f"p={result['p_value']:.4f}, change={result['percent_change']:.1f}%"
            )

# %%
print("\nSummary of significant structural breaks:")
for var in significant_breaks:
    if total_tests[var] > 0:
        print(
            f"{var}: {significant_breaks[var]}/{total_tests[var]} tracts "
            f"({100*significant_breaks[var]/total_tests[var]:.1f}%) show significant changes after HD designation"
        )

# Save results to CSV
results_df = pd.DataFrame(
    [
        {
            "tract_id": tract_id,
            "variable": var,
            "hd_year": result["hd_year"],
            "p_value": result["p_value"],
            "pre_mean": result["pre_mean"],
            "post_mean": result["post_mean"],
            "percent_change": result["percent_change"],
        }
        for tract_id, vars_results in break_results.items()
        for var, result in vars_results.items()
    ]
)

if len(results_df) > 0:
    results_df.to_csv("./structural_break_results.csv", index=False)
    print("\nSaved detailed structural break results to structural_break_results.csv")

print("\nAnalysis complete!")


# %%
