# Load data 
elec_color <- hp_color 
gas_color <- not_hp_color

# get consumption by period
hp_installed <- fread("data/input/cosy_-_hp_aggregated_up_2024_06_18.csv") %>%
  group_by(account_id, settlement_date) %>%
  summarise(total_consumption = sum(total_read_value)) %>%
  mutate(consumption_hh = total_consumption / 48) %>%
  inner_join(fread("data/input/cosy_-_hp_details_2024_06_25.csv") %>%
               distinct(account_id, .keep_all = TRUE),
             by=c("account_id")) %>%
  mutate(date = as.Date(settlement_date),
         is_hp_installed = as.numeric(installed_at <= date))%>%
  group_by(account_id) %>%
  mutate(treated = max(is_hp_installed))  # identify treated versus not yet treated


# Check number of accounts
hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()

# Load and preprocess gas consumption data
# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread("data/input/cosy_-_hp_users_gas_2024_06_13.csv") %>%
  group_by(account_id) %>%
  mutate(is_hp_installed = as.numeric(installed_at <= settlement_week),
         treated = max(is_hp_installed),
         min_settlement_week = min(settlement_week)) %>%
  distinct(account_id, settlement_week, .keep_all = TRUE)

# Create a sequence of weeks
min_date <- min(cosy_hp_install_gas_consumption$settlement_week)
max_date <- max(cosy_hp_install_gas_consumption$settlement_week)
all_weeks <- seq(min_date, max_date, by = "week")

# Create a data frame with all combinations of account_id and settlement_week
all_combinations <- expand.grid(
  account_id = unique(cosy_hp_install_gas_consumption$account_id),
  settlement_week = all_weeks
)

# Merge with original data
merged_data <- all_combinations %>%
  left_join(cosy_hp_install_gas_consumption %>% 
              distinct(account_id, settlement_week, weekly_consumption, 
                       min_settlement_week, installed_at)) %>%
  filter(min_settlement_week < settlement_week) %>%
  mutate(
    is_hp_installed = as.numeric(installed_at < settlement_week),
    weekly_consumption = ifelse(is.na(weekly_consumption), 0, weekly_consumption)
  ) 

# Define overall_weekly by merging with electricity data
overall_weekly <- hp_installed %>%
  mutate(settlement_week = floor_date(date, "week") + 1,
         is_hp_installed = as.numeric(installed_at <= settlement_week)) %>%
  group_by(account_id, hashed_mpan, tariff_gsp_group_id, settlement_week, treated, installed_at, is_hp_installed) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  left_join(merged_data %>%
              select(account_id, settlement_week, weekly_consumption) %>%
              rename(gas_consumption = weekly_consumption)) %>%
  mutate(
    gas_consumption = 52.25 * gas_consumption,
    elec_consumption = 52.25 * elec_consumption,
    total_consumption = gas_consumption + elec_consumption
  ) 


# add weather 
weather_weekly <- fread("data/input/cosy_-_weather_weekly_2024_06_13.csv") %>%
  mutate(settlement_week = as.Date(week_date)) %>%
  distinct(gsp_group_id, settlement_week, .keep_all = TRUE) %>%
  select(gsp_group_id, settlement_week, avg_heating_degree, avg_air_temperature_celsius) %>%
  rename(tariff_gsp_group_id = gsp_group_id)

# merge with consumption data
overall_weekly <- overall_weekly %>%
  inner_join(weather_weekly) %>%
  rename(hdd = avg_heating_degree) %>% 
  mutate(temp_degree = factor(
    case_when(
      avg_air_temperature_celsius < 0 ~ 0,
      avg_air_temperature_celsius < 25.5 ~ round(avg_air_temperature_celsius),
      TRUE ~ 25
    )))


# Create CS main results 
start_date <- min(overall_weekly$settlement_week)
did_data <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption) %>%
  filter(week <= 129, firstweek <= 129) 


# Function to format numbers with thousands separator
format_decimal <- function(x, digits) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

format_number <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

# Function to check if the confidence interval contains zero
confidence_star <- function(coefficient, se, alpha) {
  ci_lower <- coefficient - qnorm(1 - alpha / 2) * se
  ci_upper <- coefficient + qnorm(1 - alpha / 2) * se
  if (ci_lower > 0 | ci_upper < 0) {
    return("***")
  } else {
    return("")
  }
}

# Function to create LaTeX table
create_latex_table <- function(models, headers, title, file, label, pre_treatment_averages, note) {
  # Extract data from models
  coefficients <- sapply(models, function(model) {
    coef <- model$overall.att
    se <- model$overall.se
    alpha <- model$DIDparams$alp
    paste0(format_decimal(coef, 1), confidence_star(coef, se, alpha))
  })
  standard_errors <- sapply(models, function(model) paste0("(", format_decimal(model$overall.se, 1), ")"))
  observations <- sapply(models, function(model) format_number(model$DIDparams$n))
  nG <- sapply(models, function(model) format_number(model$DIDparams$nG))
  nT <- sapply(models, function(model) format_number(model$DIDparams$nT))
  alpha <- models[[1]]$DIDparams$alp
  conf_level <- (1 - alpha) * 100
  
  # Pre-treatment averages
  pre_treatment_averages <- pre_treatment_averages %>%
    summarise_all(mean)
  pre_treatment_values <- c(
    format_decimal(pre_treatment_averages$elec_consumption, 1),
    format_decimal(pre_treatment_averages$gas_consumption, 1),
    format_decimal(pre_treatment_averages$total_consumption, 1))
  
  # Begin LaTeX table
  latex_table <- "\\begin{table}[htbp]\n"
  latex_table <- paste0(latex_table, "   \\caption{\\label{", label, "} ", title, "}\n")
  latex_table <- paste0(latex_table, "   \\floatfoot{\\justifying \\footnotesize \\upshape \\textbf{Note:}", note, "}\n")            
  latex_table <- paste0(latex_table, "   \\centering\n")
  latex_table <- paste0(latex_table, "   \\begin{tabular}{l", paste(rep("c", length(headers)), collapse = ""), "}\n")
  latex_table <- paste0(latex_table, "      \\tabularnewline \\midrule \\midrule\n")
  latex_table <- paste0(latex_table, "                                     & ", paste(headers, collapse = "     & "), " \\\\   \n")
  latex_table <- paste0(latex_table, "      Model:                         & ", paste(paste0("(", 1:length(headers), ")"), collapse = "              & "), "\\\\  \n")
  latex_table <- paste0(latex_table, "      \\midrule\n")
  latex_table <- paste0(latex_table, "      \\emph{Variable}\\\\\n")
  latex_table <- paste0(latex_table, "      Is HP Installed $=$ 1          & ", paste(coefficients, collapse = " & "), "\\\\   \n")
  latex_table <- paste0(latex_table, "                                     & ", paste(standard_errors, collapse = "         & "), "\\\\   \n")
  latex_table <- paste0(latex_table, "      \\midrule\n")
  latex_table <- paste0(latex_table, "      \\emph{Pre-treatment Average}\\\\\n")
  latex_table <- paste0(latex_table, "      Yearly Consumption              & ", pre_treatment_values[1], " & ", pre_treatment_values[2], " & ", pre_treatment_values[3], "\\\\   \n")
  latex_table <- paste0(latex_table, "      \\midrule\n")
  latex_table <- paste0(latex_table, "      \\emph{Fit statistics}\\\\\n")
  latex_table <- paste0(latex_table, "      Number of Households                   & ", paste(observations, collapse = "           & "), "\\\\  \n")
  latex_table <- paste0(latex_table, "      Number of Cohorts              & ", paste(nG, collapse = "              & "), "\\\\  \n")
  latex_table <- paste0(latex_table, "      Number of Time Periods         & ", paste(nT, collapse = "             & "), "\\\\  \n")
  latex_table <- paste0(latex_table, "      \\midrule \\midrule\n")
  latex_table <- paste0(latex_table, "      \\multicolumn{", length(headers) + 1, "}{l}{Clustered (Household) standard-errors in parentheses}\\\\\n")
  latex_table <- paste0(latex_table, "      \\multicolumn{", length(headers) + 1, "}{l}{Estimation Method: Doubly Robust}\\\\\n")
  latex_table <- paste0(latex_table, "      \\multicolumn{", length(headers) + 1, "}{l}{Control Group: Not Yet Treated, Anticipation Periods: 1}\\\\\n")
  latex_table <- paste0(latex_table, "      \\multicolumn{", length(headers) + 1, "}{l}{Signif. Codes: *** ", conf_level, "\\% confidence band does not cover 0}\\\\\n")
  latex_table <- paste0(latex_table, "   \\end{tabular}\n")
  latex_table <- paste0(latex_table, "\\end{table}\n")
  
  # Write to file
  writeLines(latex_table, file)
}


# Example usage            
rds_files <- list(
  Overall =  "data/scratch/est_cs_total_weekly.RDS",
  Electricity = "data/scratch/est_cs_elec_weekly.RDS",
  Gas = "data/scratch/est_cs_gas_weekly.RDS"
)
               
aggte_simple_overall <- aggte(readRDS(rds_files$Overall), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_elec <- aggte(readRDS(rds_files$Electricity), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_gas <- aggte(readRDS(rds_files$Gas), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)

# Pre treatment average
pre_treatment_averages <- as_tibble(
  bind_cols(
    aggte_simple_elec$DIDparams$data %>%
      ungroup() %>%
      filter(week < firstweek - 1) %>%
      summarise(elec_consumption = mean(elec_consumption)),
    
    aggte_simple_gas$DIDparams$data %>%
      ungroup() %>%
      filter(week < firstweek - 1) %>%
      summarise(gas_consumption = mean(gas_consumption)),
    
    aggte_simple_overall$DIDparams$data %>%
      ungroup() %>%
      filter(week < firstweek - 1) %>%
      summarise(total_consumption = mean(total_consumption))
  )
)


models <- list(Electricity = aggte_simple_elec, Gas = aggte_simple_gas, Overall = aggte_simple_overall)
headers <- c( "Electricity", "Gas", "Overall")
title <- "Heat Pump Installation on Yearly Energy Consumption in kWh"
file <- "tables/hp_did_overall_cs.tex"
label <- "tab:hp-did-cs"
note <- "We show estimates from three CS estimates of the impact of consumption on customers' electricity consumption (column 1), gas consumption (column 2), and overall (electricity plus gas) consumption (column 3). The latter two models' are from a subset of our full sample for customers with gas consumption before their heat pump installation."

create_latex_table(models, headers, title, file, label, pre_treatment_averages, note)


# Function to create the ggplot for each period
create_ggplot <- function(elec_data, gas_data) {
  # Extract coefficients, standard errors, and event time for electricity
  elec_coefficients <- elec_data$att.egt/52.25
  elec_standard_errors <- elec_data$se.egt/52.25
  elec_event_time <- elec_data$egt
  
  # Extract coefficients, standard errors, and event time for gas
  gas_coefficients <- gas_data$att.egt/52.25
  gas_standard_errors <- gas_data$se.egt/52.25
  gas_event_time <- gas_data$egt
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    event_time = c(elec_event_time, gas_event_time),
    coefficient = c(elec_coefficients, gas_coefficients),
    lower_ci = c(elec_coefficients - 1.96 * elec_standard_errors, gas_coefficients - 1.96 * gas_standard_errors),
    upper_ci = c(elec_coefficients + 1.96 * elec_standard_errors, gas_coefficients + 1.96 * gas_standard_errors),
    type = rep(c("Electricity", "Gas"), each = length(elec_event_time))
  )
  
  # Create the ggplot
  p <- ggplot(plot_data, aes(x = event_time, y = coefficient, group = type)) +
    geom_line(aes(color = type)) +
    geom_point(aes(color = type, shape = type), size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = type), width = 0.2, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    scale_x_continuous(breaks = seq(floor(min(plot_data$event_time) / 10) * 10, ceiling(max(plot_data$event_time) / 10) * 10, 10)) +
    scale_colour_manual(name = "Type",
                        labels = c("Electricity", "Gas"),
                        values = c(Electricity = elec_color, Gas = gas_color)) +   
    scale_shape_manual(name = "Type",
                       labels =  c("Electricity", "Gas"),
                       values = c(16, 17)) + 
    labs(
      x = "Weeks since adoption",
      y = "Dynamic ATT for Weekly Consumption (kWh)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")  # Move the legend to the bottom
  
  
  return(p)
}

# Load the data
elec_data <- aggte(readRDS(rds_files$Electricity), type = "dynamic",
                   na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)
gas_data <- aggte(readRDS(rds_files$Gas), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)

# Create the plot
p <- create_ggplot(elec_data, gas_data)
p

# Define the filename
file_name <- "graphs/dynamic_hp_plot_combined.png"

# Save the plot
ggsave(file_name, plot = p, device = "png", width = 10, height = 8, dpi = 300)

# define the start date
start_date <- min(overall_weekly$settlement_week)

# Load the data
elec_data <- aggte(readRDS(rds_files$Electricity), type = "calendar", na.rm = TRUE, clustervars = "id", bstrap = TRUE)
gas_data <- aggte(readRDS(rds_files$Gas), type = "calendar", na.rm = TRUE, clustervars = "id", bstrap = TRUE)

# Create the combined plot
# Extract estimates, standard errors, and event time for electricity
elec_estimates <- elec_data$att.egt/52.25
elec_standard_errors <- elec_data$se.egt/52.25
elec_event_time <- elec_data$egt

# Extract estimates, standard errors, and event time for gas
gas_estimates <- gas_data$att.egt/52.25
gas_standard_errors <- gas_data$se.egt/52.25
gas_event_time <- gas_data$egt

# Calculate the week dates based on the start_date
elec_week_dates <- start_date + weeks(elec_event_time)
gas_week_dates <- start_date + weeks(gas_event_time)

# Align the lengths of the data
max_length <- max(length(elec_estimates), length(gas_estimates))

elec_estimates <- c(elec_estimates, rep(NA, max_length - length(elec_estimates)))
elec_standard_errors <- c(elec_standard_errors, rep(NA, max_length - length(elec_standard_errors)))
elec_week_dates <- c(elec_week_dates, rep(NA, max_length - length(elec_week_dates)))

gas_estimates <- c(gas_estimates, rep(NA, max_length - length(gas_estimates)))
gas_standard_errors <- c(gas_standard_errors, rep(NA, max_length - length(gas_standard_errors)))
gas_week_dates <- c(gas_week_dates, rep(NA, max_length - length(gas_week_dates)))

# Create a data frame for plotting
plot_data <- data.frame(
  week_date = c(elec_week_dates, gas_week_dates),
  estimate = c(elec_estimates, gas_estimates),
  lower_ci = c(elec_estimates - 1.96 * elec_standard_errors, gas_estimates - 1.96 * gas_standard_errors),
  upper_ci = c(elec_estimates + 1.96 * elec_standard_errors, gas_estimates + 1.96 * gas_standard_errors),
  type = rep(c("Electricity", "Gas"), each = max_length)
)


# Create the ggplot for combined effect
ggplot(plot_data, aes(x = as.Date(week_date), y = estimate, color = type)) +
  geom_point(aes(shape = type), size = 3) +  # Different shapes for Electricity and Gas
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  scale_x_date(
    labels = scales::date_format("%b %y"),  # Formatting months and years
    date_breaks = "3 month"  # Adjust this based on your data density
  ) +
  scale_colour_manual(name = "Type",
                      labels = c("Electricity", "Gas"),
                      values = c(Electricity = elec_color, Gas = gas_color)) +   
  scale_shape_manual(name = "Type",
                     labels =  c("Electricity", "Gas"),
                     values = c(16, 17)) + 
  labs(
    x = "Week",
    y = "Calendar ATT for Weekly Consumption (kWh)",
    color = "Type",
    shape = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move the legend to the bottom
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Define the filename
file_name <- "graphs/hp_calendarplot_combined.png"

# Save plot data to CSV
write.csv(plot_data, "data/output/hp_calendarplot_combined.csv", row.names = FALSE)

# Save the plot
ggsave(file_name, device = "png", width = 8, height = 6, dpi = 300)


# UNIVERSAL BASE robustness checks
rds_files <- list(
  Overall =  "data/scratch/est_cs_total_weekly_robust_universal.RDS",
  Electricity = "data/scratch/est_cs_elec_weekly_robust_universal.RDS",
  Gas = "data/scratch/est_cs_gas_weekly_robust_universal.RDS"
)

# Load the data
elec_data <- aggte(readRDS(rds_files$Electricity), type = "dynamic",
                   na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)
gas_data <- aggte(readRDS(rds_files$Gas), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)

# Create the plot
p <- create_ggplot(elec_data, gas_data)
p

# Define the filename
file_name <- "graphs/dynamic_hp_plot_robust_universal_combined.png"

# Save the plot
ggsave(file_name, plot = p, device = "png", width = 10, height = 8, dpi = 300)


# Define paths and base filenames for each anticipation period
output_base_path <- "data/scratch/"
output_filenames <- c("est_cs_elec_weekly", "est_cs_gas_weekly")
anticipation_periods <- 0:10  # The range of anticipation periods

# Define a function to process each anticipation week
process_week <- function(anticipation_week) {
  # File paths
  elec_file <- paste0(output_base_path, output_filenames[1], "_anticipation_", anticipation_week, ".RDS")
  gas_file  <- paste0(output_base_path, output_filenames[2], "_anticipation_", anticipation_week, ".RDS")
  
  # Read and calculate aggregate estimates
  elec_agg <- aggte(readRDS(elec_file), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
  gas_agg  <- aggte(readRDS(gas_file), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
  
  # Create data frames for each type
  df_elec <- data.frame(
    anticipation_week = anticipation_week,
    estimate = elec_agg$overall.att ,
    lower_ci = elec_agg$overall.att  - 1.96 * elec_agg$overall.se,
    upper_ci = elec_agg$overall.att  + 1.96 * elec_agg$overall.se,
    type = "Electricity"
  )
  
  df_gas <- data.frame(
    anticipation_week = anticipation_week,
    estimate = gas_agg$overall.att ,
    lower_ci = gas_agg$overall.att  - 1.96 * gas_agg$overall.se,
    upper_ci = gas_agg$overall.att  + 1.96 * gas_agg$overall.se,
    type = "Gas"
  )
  
  list(df_elec, df_gas)
}

# Apply the function over anticipation weeks and combine results
results_list <- lapply(anticipation_periods, process_week)

# Flatten the list and bind rows into one data frame
plot_data <- do.call(rbind, unlist(results_list, recursive = FALSE))


# Plotting the results with confidence intervals
ggplot(plot_data, aes(x = anticipation_week, y = estimate, color = type, fill = type)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  scale_color_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_fill_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  labs(
    title = "Anticipation Period Estimates for Electricity and Gas",
    x = "Anticipation Week",
    y = "Estimate",
    color = "Type",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graphs/HP_anticipation.png")




rds_files <- list(
  Overall =  "data/scratch/est_cs_total_weekly.RDS",
  Electricity = "data/scratch/est_cs_elec_weekly_gas_only.RDS",
  Gas = "data/scratch/est_cs_gas_weekly.RDS"
)

# Example usage
aggte_simple_overall <- aggte(readRDS(rds_files$Overall), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_elec <- aggte(readRDS(rds_files$Electricity), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_gas <- aggte(readRDS(rds_files$Gas), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)

# Pre treatment average
pre_treatment_averages <- as_tibble(
  bind_cols(
    aggte_simple_elec$DIDparams$data %>%
      ungroup() %>%
      filter(week < firstweek - 1) %>%
      summarise(elec_consumption = mean(elec_consumption)),
    
    aggte_simple_gas$DIDparams$data %>%
      ungroup() %>%
      filter(week < firstweek - 1) %>%
      summarise(gas_consumption = mean(gas_consumption)),
    
    aggte_simple_overall$DIDparams$data %>%
      ungroup() %>%
      filter(week < firstweek - 1) %>%
      summarise(total_consumption = mean(total_consumption))
  )
)


models <- list(Electricity = aggte_simple_elec, Gas = aggte_simple_gas, Overall = aggte_simple_overall)
headers <- c( "Electricity", "Gas", "Overall")
title <- "Heat Pump Installation on Yearly Energy Consumption in kWh"
file <- "tables/hp_did_overall_cs_gas_only.tex"
label <- "tab:hp-did-cs-gas-only"
note <- "We show estimates from three CS estimates of the impact of consumption on customers' electricity consumption (column 1), gas consumption (column 2), and overall (electricity plus gas) consumption (column 3). The latter two models' are from a subset of our full sample for customers with gas consumption before their heat pump installation."

create_latex_table(models, headers, title, file, label, pre_treatment_averages, note)



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



# Apply the TWFE models
m1 <- feols(elec_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week, 
            data = overall_weekly %>% filter(settlement_week < "2024-06-03", treated==1), cluster = ~ account_id)
m2 <- feols(gas_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week, 
            data = overall_weekly %>% filter(settlement_week < "2024-06-03", treated==1), cluster = ~ account_id)
m3 <- feols(total_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week, 
            data = overall_weekly %>% filter(settlement_week < "2024-06-03", treated==1), cluster = ~ account_id)  


# Define the main periods
main_periods <- c("Overall", "Electricity", "Gas")

# Initialize variables to store estimates and standard errors
cs_estimates <- list()
cs_se <- list()
cs_n <- list()
cs_nG <- list()
cs_nT <- list()

# Define paths to the RDS files for CS estimates
cs_files <- list(
  Overall = "data/scratch/est_cs_total_weekly.RDS",
  Electricity = "data/scratch/est_cs_elec_weekly.RDS",
  Gas = "data/scratch/est_cs_gas_weekly.RDS"
)


# Loop through each period to get the Callaway and Sant'Anna estimates
for(period in main_periods) {
  est_cs <- readRDS(cs_files[[period]])
  aggte_simple <- aggte(est_cs, type = "simple", na.rm = TRUE, clustervars="id", bstrap=TRUE, alp = 0.01)
  
  cs_estimates[[period]] <- format_decimal(aggte_simple$overall.att, 1)
  cs_se[[period]] <- format_decimal(aggte_simple$overall.se, 1)
  cs_n[[period]] <- format_number(aggte_simple$DIDparams$n)
  cs_nG[[period]] <- format_number(aggte_simple$DIDparams$nG)
  cs_nT[[period]] <- format_number(aggte_simple$DIDparams$nT)
}


# Generate the initial LaTeX table with TWFE models
etable(m2, m3, m1, 
       m2, m3, m1,
       headers = list(list("TWFE" = 3, "CS" = 3),
                      list(rep(c("Electricity", "Gas", "Overall"), times = 2))),
       depvar = FALSE,
       tex=TRUE, title = "HP Installation on Yearly Energy Consumption in kWh",
       fitstat = ~ N + g + pre_avg + t_obs + r2, file = "tables/hp_did_overall_detailed.tex", replace = TRUE, label="tab:hp-did-overall-conso-detailed")


CleanPreAverage("tables/hp_did_overall_detailed.tex")

# Read the generated LaTeX file
file_path <- "tables/hp_did_overall_detailed.tex"
file_content <- readLines(file_path)

# Function to replace the 6th to 8th columns in a LaTeX table row
replace_columns <- function(line, new_values) {
  parts <- str_split(line, "&")[[1]]
  for (i in seq_along(new_values)) {
    parts[4 + i] <- str_trim(new_values[[i]])
  }
  return(paste(parts, collapse = " & "))
}

# Function to clear the 6th to 8th columns in a LaTeX table row
clear_columns <- function(line) {
  parts <- str_split(line, "&")[[1]]
  for (i in 5:7) {
    parts[i] <- ""
  }
  return(paste(parts, collapse = " & "))
}

# Ensure each replacement maintains the LaTeX table structure
new_estimates <- c(paste0(cs_estimates[["Electricity"]], "$^{***}$"), 
                   paste0(cs_estimates[["Gas"]], "$^{***}$"),
                   paste0(cs_estimates[["Overall"]], "$^{***}$"))
new_se <- c(paste0("(", cs_se[["Electricity"]], ")"), 
            paste0("(", cs_se[["Gas"]], ")"),
            paste0("(", cs_se[["Overall"]], ")"))

# Find the rows that need to be updated
coeff_line <- grep("Is HP Installed \\$=\\$ 1", file_content)
se_line <- coeff_line + 1
obs_line <- grep("Observations", file_content)
sample_line <- grep("Size of the 'effective' sample", file_content)
periods_line <- grep("Number of Time Periods", file_content)
hdd_line <- grep("HDD", file_content)
mpan_line <- grep("Household", file_content)[1]
day_line <- grep("Week", file_content)
r2_line <- grep("R", file_content)

# Replace estimates and standard errors in the LaTeX file
file_content[sample_line] <- replace_columns(file_content[sample_line], cs_n) %>% paste0(" \\\\")
file_content[coeff_line] <- replace_columns(file_content[coeff_line], new_estimates) %>% paste0(" \\\\")
file_content[se_line] <- replace_columns(file_content[se_line], new_se) %>% paste0(" \\\\")
file_content[periods_line] <- replace_columns(file_content[periods_line], cs_nT) %>% paste0(" \\\\")
file_content[obs_line] <- clear_columns(file_content[obs_line]) %>% paste0(" \\\\")
file_content[hdd_line] <- clear_columns(file_content[hdd_line]) %>% paste0(" \\\\")
file_content[mpan_line] <- clear_columns(file_content[mpan_line]) %>% paste0(" \\\\")
file_content[day_line] <- clear_columns(file_content[day_line]) %>% paste0(" \\\\")
file_content[r2_line] <- clear_columns(file_content[r2_line]) %>% paste0(" \\\\")

# Update the line with the clustering information
clustering_line_index <- grep("Clustered \\(Household\\)", file_content)
if (length(clustering_line_index) > 0) {
  file_content[clustering_line_index] <- "\\multicolumn{10}{l}{\\emph{Clustered (Household) standard-errors in parentheses for TWFE}}\\\\"
}

# Modify the label for "Size of the 'effective' sample" to "Number of Households"
file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])

# Add CS clustering explanation
new_row <- "\\multicolumn{10}{l}{\\emph{Clustered cohort (Week of adoption) standard-errors in parentheses for CS}}\\\\"
file_content <- append(file_content, new_row, after = clustering_line_index)

# Add new row for "Number of cohorts (CS)"
new_row <- paste0("Number of cohorts (CS) & & & & ", cs_nG[["Electricity"]], " & ", cs_nG[["Gas"]], " & ", cs_nG[["Overall"]], " \\\\")
file_content <- append(file_content, new_row, after = sample_line)

# Write the modified content back to the LaTeX file
writeLines(file_content, file_path)



# Apply the TWFE models
m1 <- feols(elec_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week, 
            data = overall_weekly %>% filter(settlement_week < "2024-06-03"), cluster = ~ account_id)
m2 <- feols(gas_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week, 
            data = overall_weekly %>% filter(settlement_week < "2024-06-03"), cluster = ~ account_id)
m3 <- feols(total_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week, 
            data = overall_weekly %>% filter(settlement_week < "2024-06-03"), cluster = ~ account_id) 

# Initialize variables to store estimates and standard errors
cs_estimates <- list()
cs_se <- list()
cs_n <- list()
cs_nG <- list()
cs_nT <- list()

# Define paths to the RDS files for CS estimates
cs_files <- list(
  Overall = "data/scratch/est_cs_never_treated_total_weekly.RDS",
  Electricity = "data/scratch/est_cs_never_treated_elec_weekly.RDS",
  Gas = "data/scratch/est_cs_never_treated_gas_weekly.RDS"
)


# Loop through each period to get the Callaway and Sant'Anna estimates
for(period in main_periods) {
  est_cs <- readRDS(cs_files[[period]])
  aggte_simple <- aggte(est_cs, type = "simple", na.rm = TRUE, clustervars="id", bstrap=TRUE, alp = 0.01)
  
  cs_estimates[[period]] <- format_decimal(aggte_simple$overall.att, 1)
  cs_se[[period]] <- format_decimal(aggte_simple$overall.se, 1)
  cs_n[[period]] <- format_number(aggte_simple$DIDparams$n)
  cs_nG[[period]] <- format_number(aggte_simple$DIDparams$nG)
  cs_nT[[period]] <- format_number(aggte_simple$DIDparams$nT)
}

# Generate the initial LaTeX table with TWFE models
etable(m2, m3, m1, 
       m2, m3, m1,
       headers = list(list("TWFE" = 3, "CS" = 3),
                      list(rep(c("Electricity", "Gas", "Overall"), times = 2))),
       depvar = FALSE,
       tex=TRUE, title = "HP Installation on Yearly Energy Consumption in kWh",
       fitstat = ~ N + g + pre_avg + t_obs + r2, file = "tables/hp_did_never_treated_detailed.tex", replace = TRUE, label="tab:hp-did-overall-conso-detailed")


CleanPreAverage("tables/hp_did_never_treated_detailed.tex")

# Read the generated LaTeX file
file_path <- "tables/hp_did_never_treated_detailed.tex"
file_content <- readLines(file_path)

# Function to replace the 6th to 8th columns in a LaTeX table row
replace_columns <- function(line, new_values) {
  parts <- str_split(line, "&")[[1]]
  for (i in seq_along(new_values)) {
    parts[4 + i] <- str_trim(new_values[[i]])
  }
  return(paste(parts, collapse = " & "))
}

# Function to clear the 6th to 8th columns in a LaTeX table row
clear_columns <- function(line) {
  parts <- str_split(line, "&")[[1]]
  for (i in 5:7) {
    parts[i] <- ""
  }
  return(paste(parts, collapse = " & "))
}

# Ensure each replacement maintains the LaTeX table structure
new_estimates <- c(paste0(cs_estimates[["Electricity"]], "$^{***}$"), 
                   paste0(cs_estimates[["Gas"]], "$^{***}$"),
                   paste0(cs_estimates[["Overall"]], "$^{***}$"))
new_se <- c(paste0("(", cs_se[["Electricity"]], ")"), 
            paste0("(", cs_se[["Gas"]], ")"),
            paste0("(", cs_se[["Overall"]], ")"))

# Find the rows that need to be updated
coeff_line <- grep("Is HP Installed \\$=\\$ 1", file_content)
se_line <- coeff_line + 1
obs_line <- grep("Observations", file_content)
sample_line <- grep("Size of the 'effective' sample", file_content)
periods_line <- grep("Number of Time Periods", file_content)
hdd_line <- grep("HDD", file_content)
mpan_line <- grep("Household", file_content)[1]
day_line <- grep("Week", file_content)
r2_line <- grep("R", file_content)

# Replace estimates and standard errors in the LaTeX file
file_content[sample_line] <- replace_columns(file_content[sample_line], cs_n) %>% paste0(" \\\\")
file_content[coeff_line] <- replace_columns(file_content[coeff_line], new_estimates) %>% paste0(" \\\\")
file_content[se_line] <- replace_columns(file_content[se_line], new_se) %>% paste0(" \\\\")
file_content[periods_line] <- replace_columns(file_content[periods_line], cs_nT) %>% paste0(" \\\\")
file_content[obs_line] <- clear_columns(file_content[obs_line]) %>% paste0(" \\\\")
file_content[hdd_line] <- clear_columns(file_content[hdd_line]) %>% paste0(" \\\\")
file_content[mpan_line] <- clear_columns(file_content[mpan_line]) %>% paste0(" \\\\")
file_content[day_line] <- clear_columns(file_content[day_line]) %>% paste0(" \\\\")
file_content[r2_line] <- clear_columns(file_content[r2_line]) %>% paste0(" \\\\")

# Update the line with the clustering information
clustering_line_index <- grep("Clustered \\(Household\\)", file_content)
if (length(clustering_line_index) > 0) {
  file_content[clustering_line_index] <- "\\multicolumn{10}{l}{\\emph{Clustered (Household) standard-errors in parentheses for TWFE}}\\\\"
}

# Modify the label for "Size of the 'effective' sample" to "Number of Households"
file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])

# Add CS clustering explanation
new_row <- "\\multicolumn{10}{l}{\\emph{Clustered cohort (Week of adoption) standard-errors in parentheses for CS}}\\\\"
file_content <- append(file_content, new_row, after = clustering_line_index)

# Add new row for "Number of cohorts (CS)"
new_row <- paste0("Number of cohorts (CS) & & & & ", cs_nG[["Electricity"]], " & ", cs_nG[["Gas"]], " & ", cs_nG[["Overall"]], " \\\\")
file_content <- append(file_content, new_row, after = sample_line)

# Write the modified content back to the LaTeX file
writeLines(file_content, file_path)


# DID Imputation
# Data preparation (reusing your 'did_data' code)
start_date <- min(overall_weekly$settlement_week)
did_data <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption) %>%
  mutate(firstweek = ifelse(firstweek>131, NA, firstweek),
         total_consumption = total_consumption/ 52.25,
         elec_consumption= elec_consumption/ 52.25, 
         gas_consumption= gas_consumption/ 52.25, )

summary(did_data)


# did_imputation command
# Setting up the arguments for did_imputation
imputation_results <- did_imputation(
  data = did_data,
  yname = "gas_consumption",    # Outcome variable (or choose "elec_consumption" or "gas_consumption")
  gname = "firstweek",            # Variable name for unit-specific treatment time
  tname = "week",                 # Calendar period
  idname = "id",                  # Unique unit ID
  first_stage = NULL,             # Default to unit and time fixed effects
  wname = NULL,                   # No weights provided
  wtr = NULL,                     # Treatment weights (for static and event study effects)
  horizon = c(-52, 52),                 # Use all event time horizons
  pretrends = -52:-1,               # Use all pre-trends
  cluster_var = "id"              # Clustering variable
)

# Viewing the results
summary(imputation_results)


# did_imputation command
# Setting up the arguments for did_imputation
imputation_results2 <- did_imputation(
  data = did_data,
  yname = "elec_consumption",    # Outcome variable (or choose "elec_consumption" or "gas_consumption")
  gname = "firstweek",            # Variable name for unit-specific treatment time
  tname = "week",                 # Calendar period
  idname = "id",                  # Unique unit ID
  first_stage = NULL,             # Default to unit and time fixed effects
  wname = NULL,                   # No weights provided
  wtr = NULL,                     # Treatment weights (for static and event study effects)
  horizon = c(-52, 52),                 # Use all event time horizons
  pretrends = -52:-1,               # Use all pre-trends
  cluster_var = "id"              # Clustering variable
)

# Viewing the results
summary(imputation_results2)



# merge the two estimates
plot_data <- rbind(imputation_results %>% mutate(type = "Gas"),
                   imputation_results2 %>% mutate(type = "Electricity")) %>%
  rename(coefficient = estimate,
         event_time = term) %>%
  mutate(event_time = as.numeric(event_time)) %>%
  filter(event_time > -91, event_time < 90) %>%
  mutate(lower_ci =  conf.low,
         upper_ci = conf.high)

#%>%
#mutate(lower_ci = ifelse(event_time < 0, NA, conf.low),
#       upper_ci = ifelse(event_time < 0, NA, conf.high))


# define color
elec_color <- "#AD87CA"
gas_color <- "#2D354A"

# plot
ggplot(plot_data, aes(x = event_time, y = coefficient, group = type)) +
  geom_line(aes(color = type)) +
  geom_point(aes(color = type, shape = type), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = type), width = 0.2, alpha = 0.6) +
  scale_colour_manual(name = "Type",
                      labels = c("Electricity", "Gas"),
                      values = c(Electricity = elec_color, Gas = gas_color)) +   
  scale_shape_manual(name = "Type",
                     labels =  c("Electricity", "Gas"),
                     values = c(16, 17)) + 
  scale_x_continuous(breaks = seq(floor(min(plot_data$event_time) / 10) * 10, ceiling(max(plot_data$event_time) / 10) * 10, 10)) +
  labs(
    x = "Weeks since adoption",
    y = "Dynamic ATT for Weekly Consumption (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move the legend to the bottom
ggsave("graphs/hp_dynamic_att_combined_imputation.png", device = "png", width = 16, height = 12, dpi = 300)

