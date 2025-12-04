# ===============================================
# HEATPUMP Reproduction Script
# ===============================================
# This script sets up the environment, loads data,
# runs the main analysis and produces outputs for
# the HEATPUMP project.
# ===============================================

# set dictionary
setFixest_dict(c(consumption_hh = "Consumption in kWh per half hour",
                 total_consumption = "Weekly Energy Consumption in kWh", 
                 daily_consumption = "Consumption in kWh per day",
                 share_consumption = "Share of daily consumption",
                 weekly_consumption = "Weekly Gas Consumption in kWh",
                 is_hp_installed = "OnCosy",
                 hdd = "HDD",
                 daily_avg_heating_degree = "HDD",
                 avg_heating_degree = "HDD",
                 date = "Day", 
                 settlement_week = "Week",
                 account_id = "Household", 
                 corrected_consumption = "Monthly Gas Consumption in kWh",
                 account_closed = "Account Closed",
                 tariff_gsp_group_id = "GSP", 
                 rate_period = "Rate period",
                 is_variable = "Variable Tariff",
                 energy_efficiency = "Energy Efficiency",
                 estimated_annual_consumption = "EAC",
                 eac_mwh = "EAC in mWh",
                 is_hp_installed = "Is HP Installed",
                 predicted_is_installed = "Is HP Installed",
                 elec_consumption = "Weekly Electricity Consumption (kWh)",
                 gas_consumption = "Weekly Gas Consumption (kWh)",
                 month_date = "Month",
                 is_winter = "Winter",
                 previous_is_variable = "Prev Is Variable",
                 epc_letter = "EPC",
                 latest_survey_heat_loss = "Heatloss (W)",
                 urban = "Urban",
                 hp_survey_is_solar_present = "Has Solar PV",
                 floor_area = "Floor area",
                 property_value = "Property value",
                 months_since_hp_rec = "Months Since HP Installation",
                 ev_charging = "EV Charging",
                 has_ev = "EV User"))


## Define most used functions

### Add pre-treatment average statistics to tables
CleanPreAverage <- function(file_path) {
  
  # Read the generated LaTeX file
  file_content <- readLines(file_path)
  
  # Find the lines with the pre-treatment average and remove them
  if (length(grep("Yearly Consumption", file_content))==1) {
    pre_avg_line_index <- grep("Yearly Consumption", file_content)
  } else {
    pre_avg_line_index <- grep("Yearly Consumption", file_content)[2]
  }
  
  # Find the lines with the pre-treatment average and remove them
  pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index)]
  file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index)]
  
  # Find the position just after the coefficients
  coeff_end_index <- grep("Is HP Installed", file_content)[length(grep("Is HP Installed", file_content))] + 2
  
  # Insert the pre-treatment average row after the coefficients
  file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
  file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
  
  # Add a \midrule after the pre-treatment average
  file_content <- append(file_content, "\\midrule", after = coeff_end_index + length(pre_avg_lines)+1)
  
  # Modify the label for "Size of the 'effective' sample" to "Number of Households"
  sample_line <- grep("Size of the 'effective' sample", file_content)
  file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
  
  # Write the modified content back to the LaTeX file
  writeLines(file_content, file_path)
}

# Function to format numbers
format_number <- function(number) {
  return(format(round(number), big.mark = ",", scientific = FALSE))
}

# Function to format numbers with four decimal places
format_decimal <- function(number, digits = 2) {
  return(format(round(number, digits), nsmall = digits, big.mark = ",", scientific = FALSE))
}

# Register the pre-treatment average fit statistic
fitstat_register("pre_avg", function(x) {
  
  # Directly use the model's data
  model_data <- eval(x$call$data, envir = x$call_env)
  
  # Extract the outcome variable from the formula
  outcome_variable <- all.vars(x$fml_all$linear)[1]
  
  # Get the logical vector of observations used in the model
  obs_used <- obs(x)
  
  # Ensure `obs_used` correctly subsets the data
  if (is.logical(obs_used) && length(obs_used) == nrow(model_data)) {
    data_used <- model_data[obs_used, ]
  } else if (is.numeric(obs_used) && all(obs_used <= nrow(model_data))) {
    data_used <- model_data[obs_used, ]
  } else {
    stop("Unable to correctly subset data. Check the obs_used vector.")
  }
  
  # Extract the outcome values
  outcome_values <- data_used[[outcome_variable]]
  
  # Create pre-avg for non-HP installed group
  pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Yearly Consumption")

# Add number of time periods
fitstat_register("t_obs", function(x) {
  
  # time variable
  t_var <- x$fixef_vars[3]
  
  # nbr of unique val for t var
  t_obs <- x$fixef_sizes[t_var]
  
  return(format_number(t_obs))
}, "Number of Time Periods")

# Load data
source("scripts/01_01_load_data.R")

# List objects in the environment
list_env <- c(ls(), "list_env")

# Summary statistics 
source("scripts/01_02_summary_graphs.R")

# Remove all objects in the environment except for those in list_env
rm(list = setdiff(ls(), list_env))


source("scripts/01_03_balance_table.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_04_ev_ownership.R")
rm(list = setdiff(ls(), list_env))


source("scripts/01_05_switch_to_smart_tariff.R")
rm(list = setdiff(ls(), list_env))



## Heterogeneity analysis
source("scripts/01_06_heterogeneity_analysis.R")
rm(list = setdiff(ls(), list_env))
gc()


source("scripts/01_07_cop_analysis.R")
rm(list = setdiff(ls(), list_env))
gc()

source("scripts/01_08_engineer_variance_analysis.R")
rm(list = setdiff(ls(), list_env))


source("scripts/01_09_solar_PV_analysis.R")
rm(list = setdiff(ls(), list_env))


source("scripts/01_10_data_availability.R")
rm(list = setdiff(ls(), list_env))



source("scripts/01_11_event_study.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_12_DiD_analysis.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_13_DiD_analysis_outputs.R")
rm(list = setdiff(ls(), list_env))



