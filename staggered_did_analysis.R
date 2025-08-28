# Modern Difference-in-Differences Analysis with Staggered Adoption
# Implements Callaway & Sant'Anna (2021) and Sun & Abraham (2021) approaches

# Install required packages if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # for data manipulation
  did,          # Callaway & Sant'Anna (2021) estimator
  fixest,       # Sun & Abraham (2021) style estimators
  ggplot2,      # for plotting
  patchwork,    # for combining plots
  haven,        # for reading data files
  here          # for file paths
)

# Read in panel data
panel_data <- read.csv("data/panel_data.csv")

# drop year < 2000
panel_data <- panel_data %>%
  filter(year > 2001)

# Data preparation
# Make sure tract_id is numeric for did package
panel_data$tract_id_numeric <- as.numeric(as.factor(panel_data$tract_id))
panel_data$GEOID <- as.factor(panel_data$GEOID)

# Create cohort variable (first year of treatment for each tract)
cohorts <- panel_data %>%
  # Convert hd to logical first
  mutate(hd_logical = (hd == "TRUE" | hd == "true" | hd == "True")) %>%
  filter(hd_logical == TRUE) %>%
  group_by(tract_id_numeric) %>%
  summarize(first_treated = min(year)) %>%
  ungroup()

# Join cohort info back to the main data
panel_data <- panel_data %>%
  left_join(cohorts, by = "tract_id_numeric") %>%
  mutate(
    # For never-treated units, set first_treated to Inf or some value beyond study period
    first_treated = if_else(is.na(first_treated), Inf, first_treated),
    # Create a never_treated flag
    never_treated = is.infinite(first_treated),
    # Create proper logical hd variable for models
    hd_logical = (hd == "TRUE" | hd == "True" | hd == "1")
  )
summary(panel_data)


# Define variables to analyze
rent_vars <- c("median_rent", "low_quart_rent", "upper_quart_rent")
ethnic_vars <- c("nonwhite_nonasian_pct")

# Function to run Callaway & Sant'Anna estimator
run_cs_did <- function(outcome_var) {
  cat("\n\n===== Callaway & Sant'Anna DiD for", outcome_var, "=====\n")
  
  # Create a temporary dataset for this variable
  temp_data <- panel_data %>%
    select(tract_id_numeric, year, hd, first_treated, !!sym(outcome_var)) %>%
    # Remove rows with NA in outcome variable
    filter(!is.na(!!sym(outcome_var)))
  
  # Ensure data is sorted
  temp_data <- temp_data %>% 
    arrange(tract_id_numeric, year)
  
  # Run the CS estimator
  cs_results <- try(
    att_gt(
      yname = outcome_var,
      tname = "year",
      idname = "tract_id_numeric",
      gname = "first_treated",
      data = temp_data,
      control_group = "notyettreated",
      anticipation = 0,  # No anticipation effects
      bstrap = TRUE,     # Use bootstrap for inference
      cband = TRUE,      # Compute uniform confidence bands
      clustervars = "tract_id_numeric", # Cluster at the tract level
      print_details = FALSE
    )
  )
  
  if (inherits(cs_results, "try-error")) {
    cat("Error in CS estimator for", outcome_var, "\n")
    return(NULL)
  }
  
  # Print summary of results
  summary(cs_results)
  
  # Aggregate treatment effects
  agg_results <- aggte(cs_results, type = "dynamic")
  summary(agg_results)
  
  # Generate event study plot
  es_plot <- ggdid(agg_results) +
    ggtitle(paste("Event Study:", outcome_var)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  # Save plot
  ggsave(
    paste0("figures/event_study_cs_", outcome_var, ".png"),
    es_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  return(list(cs_results = cs_results, agg_results = agg_results, plot = es_plot))
}

# Function to run Sun & Abraham style estimator using fixest
run_sa_did <- function(outcome_var) {
  cat("\n\n===== Sun & Abraham style DiD for", outcome_var, "=====\n")
  
  # Create a temporary dataset for this variable
  temp_data <- panel_data %>%
    select(tract_id_numeric, year, hd, first_treated, !!sym(outcome_var)) %>%
    filter(!is.na(!!sym(outcome_var)))
  
  # Create relative time variable (event time)
  temp_data <- temp_data %>%
    mutate(rel_time = year - first_treated) %>%
    # For never-treated units, set rel_time to some large negative value
    mutate(rel_time = if_else(is.infinite(first_treated), -1000, rel_time))
  
  # Create cohort-specific indicators
  temp_data <- temp_data %>%
    # We'll define cohorts as groups of 2-3 years to avoid too many small cohorts
    mutate(
      cohort_group = case_when(
        first_treated <= 2005 ~ "2000-2005",
        first_treated <= 2010 ~ "2006-2010",
        first_treated <= 2015 ~ "2011-2015",
        first_treated <= 2020 ~ "2016-2020",
        first_treated <= 2023 ~ "2021-2023",
        TRUE ~ "never_treated"
      ),
      cohort_group = factor(cohort_group)
    )
  
  # Check if there's a rel_time = -1 available to use as reference
  if(!(-1 %in% temp_data$rel_time)) {
    # If not, choose a different reference period
    ref_period <- ifelse(-2 %in% temp_data$rel_time, -2, min(temp_data$rel_time[temp_data$rel_time > -1000]))
    cat("Using reference period rel_time =", ref_period, "instead of -1\n")
  } else {
    ref_period <- -1
  }
  
  # Run interaction-weighted estimator
  # We'll use rel_time bins to avoid sparse periods
  model_sa <- try(
    feols(
      as.formula(paste0(outcome_var, " ~ i(rel_time, cohort_group, ref = ", ref_period, ") | tract_id_numeric + year")),
      data = temp_data %>% 
        # Restrict to reasonable event time window
        filter(rel_time >= -5 & rel_time <= 5 | rel_time == -1000),
      cluster = "tract_id_numeric"
    )
  )
  
  if (inherits(model_sa, "try-error")) {
    cat("Error in SA estimator for", outcome_var, "\n")
    return(NULL)
  }
  
  # Print results
  summary(model_sa)
  
  # Create event study plot
  sa_plot <- try(
    iplot(
      model_sa,
      main = paste("Event Study:", outcome_var),
      xlab = "Years Relative to Historic Designation",
      ylab = paste("Effect on", outcome_var)
    )
  )
  
  # Save plot
  if (!inherits(sa_plot, "try-error")) {
    ggsave(
      paste0("figures/event_study_sa_", outcome_var, ".png"),
      sa_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
  return(list(model = model_sa, plot = sa_plot))
}

# Create directory for figures if it doesn't exist
dir.create("figures", showWarnings = FALSE)

# Run analysis for all variables
results_cs <- list()
results_sa <- list()

for (var in c(rent_vars, ethnic_vars)) {
  # Check if variable exists in the data
  if (var %in% colnames(panel_data)) {
    # Run Callaway & Sant'Anna estimator
    results_cs[[var]] <- run_cs_did(var)
    
    # Run Sun & Abraham style estimator
    results_sa[[var]] <- run_sa_did(var)
    
    cat("\nCompleted analysis for", var, "\n")
  } else {
    cat("\nVariable", var, "not found in the data\n")
  }
}

# Save results
# save(results_cs, results_sa, file = "staggered_did_results.RData")

cat("\n\n===== Analysis Complete =====\n")
cat("Results saved to staggered_did_results.RData\n")
cat("Plots saved to the figures/ directory\n")
results_sa$median_rent
