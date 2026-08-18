### Figure A.21: Main Results Coefficients With and Without Controlling for Heat Pump
# Register the pre-treatment average fit statistic
fitstat_register("pre_avg3", function(x) {
  
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
  pre_avg <- mean(outcome_values[data_used$is_hp_installed == 1 & data_used$cosy_contract_active == 1], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Half Hourly Consumption Pre-Tariff-Adoption")


# Register the pre-treatment average fit statistic
fitstat_register("pre_avg2", function(x) {
  
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
  pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Half Hourly Consumption Pre-Heatpump")


# get the subsample with HP installation by OE
# get install date
hp_install_date <- fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>%
  select(account_id, installed_at) %>%
  mutate(installed_at = as.Date(installed_at)) %>%
  distinct(account_id, .keep_all = TRUE)

# get people from the survey responders
survey_responses <- fread(file.path(datapath, "input/responses.csv")) %>%
  select(-Other) %>% 
  rename(account_number = kid) %>%
  inner_join(fread(file.path(datapath, "input/survey_ids.csv"))) %>%
  mutate(installed_at_2 = as.Date(`When was your heat pump installed?`),
         `Electric vehicle(s)` = as.numeric(`Electric vehicle(s)`=="Electric vehicle(s)")) %>%
  distinct(account_id, .keep_all = TRUE)

aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS")) %>%
  left_join(hp_install_date)  %>%
  left_join(survey_responses)  %>%
  mutate(is_hp_installed = ifelse(!is.na(installed_at), 
                                  as.numeric(installed_at <= date),
                                  ifelse(!is.na(installed_at_2),  as.numeric(installed_at_2 <= date),
                                         NA)))


m_hp_cosy <- feols(consumption_hh ~ i(is_hp_installed) + i(cosy_contract_active) | hdd + account_id + date, 
                   data = aggregated_data, 
                   cluster = ~account_id, 
                   split = ~ rate_period)

etable(m_hp_cosy, tex=TRUE, title = "HP and Cosy Adoption",
       fitstat = ~ N + g + pre_avg2 + pre_avg3  +t_obs + r2, 
       file = "tables/did_hp_install.tex", replace = TRUE, label="tab:did-hp-install")



# Read the generated LaTeX file
file_content <- readLines("tables/did_hp_install.tex")

# Find the lines with the pre-treatment average and remove them
pre_avg_line_index <- grep("Half Hourly Consumption Pre-Heatpump", file_content)
pre_avg_line_index2 <- grep("Half Hourly Consumption Pre-Tariff-Adoption", file_content)
pre_avg_lines <- file_content[pre_avg_line_index]
pre_avg_lines2 <- file_content[pre_avg_line_index2]

file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]

# Find the position just after the coefficients
coeff_end_index <- grep("Cosy Contract Active", file_content) + 2
if (length(coeff_end_index) > 1) {
  coeff_end_index <- coeff_end_index[-1]
}

# Insert the pre-treatment average row after the coefficients
file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
file_content <- append(file_content, pre_avg_lines2, after = coeff_end_index)
file_content <- append(file_content,"\\emph{Pre-Treatment Average}\\", after = coeff_end_index)
# Add a \midrule after the pre-treatment average
file_content <- append(file_content, "\\midrule", after = coeff_end_index + length(pre_avg_lines))

# Modify the label for "Size of the 'effective' sample" to "Number of Households"
sample_line <- grep("Size of the 'effective' sample", file_content)
file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])


# Write the modified content back to the LaTeX file
writeLines(file_content, "tables/did_hp_install.tex")

rm(list = ls(pattern = "^m_"))
gc()


# Write the modified content back to the LaTeX file
# plot the coefficients with and without controls

m_without_control <- feols(consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date, 
                           data = aggregated_data %>% filter(!is.na(installed_at_2)), 
                           split = ~ rate_period,
                           cluster = ~account_id)
m_with_control <- feols(consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date + is_hp_installed, 
                        data = aggregated_data %>% filter(!is.na(installed_at_2)), 
                        split = ~ rate_period,
                        cluster = ~account_id) 

# Extract coefficients and standard errors for total_consumption
coefs_total <- rbind(coeftable(m_without_control)  %>%
                       data.frame() %>%
                       mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
                              upper_ci = Estimate + 1.96 * `Std..Error`,
                              model = "Without HP Installation Date"),
                     coeftable(m_with_control)  %>%
                       data.frame() %>%
                       mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
                              upper_ci = Estimate + 1.96 * `Std..Error`,
                              model = "With HP Installation Date")) %>%
  mutate(rate_period = factor(sample, levels = c("Morning Cosy",
                                                 "Afternoon Cosy",
                                                 "Peak Rate",
                                                 "Other", 
                                                 "Overall")))

# Define custom colors
custom_colors <- c("Without HP Installation Date" = "#E8F5FF", "With HP Installation Date" = cosy_color)

# Create the ggplot
ggplot(coefs_total, aes(x = factor(rate_period), y = Estimate, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), show.legend = TRUE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 1), alpha=0.6) +
  scale_fill_manual(values = custom_colors) +
  labs(x = "Rate Period",
       y = "Coefficient Estimate",
       fill = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the plot
ggsave(paste0("graphs/did_controlling_hp_installation.png"),
       width = 16, height = 8, units = "cm")


etable(m_with_control, m_without_control, tex=TRUE, 
       title = "HP and Cosy Adoption",
       fitstat = ~ N + g  + pre_avg2 + pre_avg3 +t_obs + r2, 
       file = "tables/did_hp_install_overall.tex", replace = TRUE, label="tab:did-hp-install-overall")


# Read the generated LaTeX file
file_content <- readLines("tables/did_hp_install_overall.tex")

# Find the lines with the pre-treatment average and remove them
pre_avg_line_index <- grep("Half Hourly Consumption Pre-Heatpump", file_content)
pre_avg_line_index2 <- grep("Half Hourly Consumption Pre-Tariff-Adoption", file_content)
pre_avg_lines <- file_content[pre_avg_line_index]
pre_avg_lines2 <- file_content[pre_avg_line_index2]

file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]

# Find the position just after the coefficients
coeff_end_index <- grep("Cosy Contract Active", file_content) + 2
if (length(coeff_end_index) > 1) {
  coeff_end_index <- coeff_end_index[-1]
}

# Insert the pre-treatment average row after the coefficients
file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
file_content <- append(file_content, pre_avg_lines2, after = coeff_end_index)
file_content <- append(file_content,"\\emph{Pre-Treatment Average}\\", after = coeff_end_index)
# Add a \midrule after the pre-treatment average
file_content <- append(file_content, "\\midrule", after = coeff_end_index + length(pre_avg_lines))

# Modify the label for "Size of the 'effective' sample" to "Number of Households"
sample_line <- grep("Size of the 'effective' sample", file_content)
file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])

# Write the modified content back to the LaTeX file
writeLines(file_content, "tables/did_hp_install_overall.tex")