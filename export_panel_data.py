# Export panel data to CSV for R analysis
import pandas as pd
import geopandas as gpd
import os

# Function to export panel data from Python script for R analysis
def export_panel_data():
    # Check if panel_df exists in the global scope
    try:
        # Get panel_df from historic_designation_analysis.py script
        # This assumes you've already run the script up to the panel data creation
        from historic_designation_analysis import panel_df
        
        # Select relevant columns
        export_cols = ['tract_id', 'GEOID', 'year', 'hd', 'median_rent', 
                       'low_quart_rent', 'upper_quart_rent', 'nonwhite_nonasian_pct',
                       'white_alone', 'black_alone', 'asian_alone', 'hispanic', 'total_pop']
        
        # Create a DataFrame with only the columns that exist
        export_df = panel_df[[col for col in export_cols if col in panel_df.columns]]
        
        # Export to CSV
        export_df.to_csv('panel_data.csv', index=False)
        print(f"Exported panel data to panel_data.csv with {len(export_df)} rows")
        
    except Exception as e:
        print(f"Error exporting panel data: {e}")
        print("Manual export required - run historic_designation_analysis.py first")

if __name__ == "__main__":
    export_panel_data()
