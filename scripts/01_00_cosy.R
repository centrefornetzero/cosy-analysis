# ===============================================
# COSY Reproduction Script
# ===============================================
# This script sets up the environment, loads data,
# runs the main analysis and produces outputs for
# the COSY project.
# ===============================================



# -----------------------------
# Variable Labels for Output
# -----------------------------

# set dictionary
setFixest_dict(c(total_consumption = "Consumption in kWh per period", 
                 daily_consumption = "Consumption in kWh per day",
                 consumption_hh = "Half Hourly Consumption in kWh",
                 share_consumption = "Share of daily consumption",
                 weekly_consumption = "Gas Consumption per Week in kWh",
                 cosy_contract_active = "Contract Active",
                 daily_avg_heating_degree = "HDD",
                 avg_heating_degree = "HDD",
                 hdd = "HDD",
                 settlement_week = "Week",
                 date = "Day", 
                 hashed_mpan = "Household", 
                 tariff_gsp_group_id = "GSP", 
                 rate_period = "Rate period",
                 is_variable = "Variable Tariff",
                 energy_efficiency = "Energy Efficiency",
                 estimated_annual_consumption = "EAC",
                 eac_mwh = "EAC in mWh",
                 is_hp_installed = "Is HP Installed",
                 predicted_is_installed = "Is HP Installed",
                 elec_consumption = "Monthly Electricity Consumption (kWh)",
                 gas_consumption = "Monthly Gas Consumption (kWh)",
                 month_date = "Month",
                 is_winter = "Winter",
                 previous_is_variable = "Prev Is Variable",
                 epc_letter = "EPC",
                 predicted_heatloss_watts = "Heatloss (W)",
                 total_floor_area = "Floor Area (m sq)",
                 urban = "Urban",
                 floor_area = "Floor area",
                 property_value = "Property value",
                 account_id = "Household",
                 ev_charging = "EV Charging",
                 has_ev = "EV User"))


# -----------------------------
#  Define main functions
# -----------------------------

# Function to format numbers
format_number <- function(number) {
  return(format(round(number), big.mark = ",", scientific = FALSE))
}

# Function to format numbers with four decimal places
format_decimal <- function(number, decimals = 4) {
  return(format(round(number, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE))
}


# Register the pre-treatment average fit statistic
fitstat_register("pre_avg", function(x) {
  
  # Function to format numbers with four decimal places
  format_decimal <- function(number, decimals = 4) {
    return(format(round(number, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE))
  }

  # Extract the formula
  formula <- x$fml_all$linear
  
  # Extract the outcome variable from the formula
  outcome_variable <- all.vars(formula)[1]
  
  # Extract the call object and evaluate the data argument
  call_object <- x$call
  data_expr <- call_object$data
  data <- eval(data_expr)
  
  # Get the logical vector of observations used in the model
  obs_used <- obs(x)
  
  # Subset the original dataset using this logical vector
  data_used <- data[obs_used, ]
  
  # Ensure the outcome variable is treated as a column name
  outcome_values <- data_used[[outcome_variable]]
  
  # Create pre-avg for non-HP installed group
  pre_avg <- mean(outcome_values[data_used$cosy_contract_active == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Half Hourly Consumption")

# Add number of time periods
fitstat_register("t_obs", function(x) {
  
  # Function to format numbers
  format_number <- function(number) {
    return(format(round(number), big.mark = ",", scientific = FALSE))
  }
  
  # time variable
  t_var <- x$fixef_vars[3]
  
  # nbr of unique val for t var
  t_obs <- x$fixef_sizes[t_var]
  
  return(format_number(t_obs))
}, "Number of Time Periods")

CleanPreAverage <- function(file_path) {
  
  # Read the generated LaTeX file
  raw_file <- readLines(file_path)
  
  # Find the lines with the pre-treatment average and remove them
  if (length(grep("Half Hourly Consumption", raw_file))==1) {
    pre_avg_line_index <- grep("Half Hourly Consumption", raw_file)
  } else {
    pre_avg_line_index <- grep("Half Hourly Consumption", raw_file)[2]
  }
  
  pre_avg_lines <- raw_file[pre_avg_line_index:(pre_avg_line_index)]
  file_content <- raw_file[-c(pre_avg_line_index, pre_avg_line_index)]
      
  # Find the position just after the coefficients
  coeff_end_index <- grep("Fixed-effects", file_content) -2
  
  # Insert the pre-treatment average row after the coefficients
  file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
  file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
    
  # Add a \midrule after the pre-treatment average
  file_content <- append(file_content, "\\midrule", after = coeff_end_index)
  
  # Modify the label for "Size of the 'effective' sample" to "Number of Households"
  sample_line <- grep("Size of the 'effective' sample", file_content)
  file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
  

  print(file_content)
  # Write the modified content back to the LaTeX file
  writeLines(file_content, file_path)
}

# fix dates import
base_as_date <- base::as.Date  # save original function

as.Date <- function(x, ...) {
  if (is.numeric(x)) {
    base_as_date(x, origin = "1970-01-01", ...)
  } else {
    base_as_date(x, ...)
  }
}

# # -----------------------------
# # Data processing
# # -----------------------------

source("scripts/01_01_load_data.R")
print("loaded data")

# List objects in the environment
list_env <- c(ls(), "list_env")

source("scripts/01_02_rate_graphs.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_03_summary_graphs.R")
list_env <- c(list_env, "contract_analysis")
rm(list = setdiff(ls(), list_env))

source("scripts/01_04_data_availability.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_05_balance_table.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_06_lct_ownership_and_leavers.R")
rm(list = setdiff(ls(), list_env))

source("scripts/01_07_heterogeneity_analysis.R")
rm(list = setdiff(ls(), list_env))
gc()   

source("scripts/01_08_cosy_and_hp_coadoption.R")
rm(list = setdiff(ls(), list_env))
gc()  

source("scripts/01_09_structural_winner.R")
rm(list = setdiff(ls(), list_env))
gc()  

source("scripts/01_10_DiD_analysis.R")
rm(list = setdiff(ls(), list_env))
gc()  
