### Table A.5: HP Installation and Solar PV on Electricity Consumption
# Register the pre-treatment average fit statistic
fitstat_register("pre_avg_solar", function(x) {
  
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
  pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0 & data_used$hp_survey_is_solar_present == TRUE], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Yearly Consumption Has Solar PV")



# Register the pre-treatment average fit statistic
fitstat_register("pre_avg_rest", function(x) {
  
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
  pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0 & data_used$hp_survey_is_solar_present == FALSE], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Yearly Consumption No Solar PV")


# Fit the model
m_solar <- feols(total_consumption ~ i(is_hp_installed) + i(is_hp_installed, hp_survey_is_solar_present, ref = 0) |
                   hdd + account_id + date,
                 data = hp_installed %>% filter(treated == 1) %>% ungroup()  %>%
                   mutate(total_consumption=365.25*total_consumption,   rate_period = factor(rate_period, levels = c("Morning Cosy",                                                                                                             "Afternoon Cosy","Peak Rate","Other", "Overall"))),
                 cluster = ~account_id,
                 split = ~ rate_period)

# Generate the initial LaTeX table
etable(m_solar,  tex = TRUE, title = "HP Installation and Solar PV on Electricity Consumption ",
       fitstat = ~ N + g + pre_avg_rest + pre_avg_solar +t_obs + r2,
       dict = c("total_consumption" = "Yearly Consumption in kWh"),
       file = "tables/hp_did_solar.tex", replace = TRUE, label = "tab:hp-did-solar")

# Read the generated LaTeX file
file_content <- readLines("tables/hp_did_solar.tex")

# Find the lines with the pre-treatment average and remove them
pre_avg_line_index <- grep("Yearly Consumption No Solar PV", file_content)
pre_avg_line_index2 <- grep("Yearly Consumption Has Solar PV", file_content)
pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index2)]
file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]

# Find the position just after the coefficients
coeff_end_index <- grep("Is HP Installed", file_content)[2] + 2
if (length(coeff_end_index) > 1) {
  coeff_end_index <- coeff_end_index[-1]
}

# Insert the pre-treatment average row after the coefficients
file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)

# Add a \midrule after the pre-treatment average
file_content <- append(file_content, "\\midrule", after = coeff_end_index + 3)

# Modify the label for "Size of the 'effective' sample" to "Number of Households"
sample_line <- grep("Size of the 'effective' sample", file_content)
file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])

# Write the modified content back to the LaTeX file
writeLines(file_content, "tables/hp_did_solar.tex")
