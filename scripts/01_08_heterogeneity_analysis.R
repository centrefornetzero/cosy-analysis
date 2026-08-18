# Objects to retain when the environment is cleared below
all_objects <- ls()
keep_objects <- c("aggregated_data", "m1", "m1_share", "cosy_color", "flexible_color", "CleanPreAverage", "format_decimal", "format_number")

# Clear all objects except those specified above
#rm(list = setdiff(ls(), list_env))
gc()   

# ===========================================================================
### Figure 11: Impact of Cosy by Outside Temperature
# ===========================================================================
# Fit the model
m1 <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
              hdd, 
            data = aggregated_data, 
            cluster = ~account_id, 
            split = ~ rate_period)

m1_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
                   daily_avg_air_temperature_celsius , 
                 data = aggregated_data %>% filter(daily_avg_air_temperature_celsius <0), 
                 cluster = ~account_id, 
                 split = ~ rate_period)
m2_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
                   daily_avg_air_temperature_celsius , 
                 data = aggregated_data %>% filter(daily_avg_air_temperature_celsius >= 0, 
                                                   daily_avg_air_temperature_celsius < 5), 
                 cluster = ~account_id, 
                 split = ~ rate_period)
m3_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
                   daily_avg_air_temperature_celsius, 
                 data = aggregated_data %>% filter(daily_avg_air_temperature_celsius >= 5, 
                                                   daily_avg_air_temperature_celsius < 10), 
                 cluster = ~account_id, 
                 split = ~ rate_period)
m4_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
                   daily_avg_air_temperature_celsius , 
                 data = aggregated_data %>% filter(daily_avg_air_temperature_celsius >= 10), 
                 cluster = ~account_id, 
                 split = ~ rate_period)

# Extract coefficients and standard errors
coefs <- rbind(
  coeftable(m1_cold) %>%
    data.frame() %>%
    mutate(model = "T < 0°C"),
  coeftable(m2_cold) %>%
    data.frame() %>%
    mutate(model = "0 ≤ T < 5°C"),
  coeftable(m3_cold) %>%
    data.frame() %>%
    mutate(model = "5 ≤ T < 10°C"),
  coeftable(m4_cold) %>%
    data.frame() %>%
    mutate(model = "T ≥ 10°C")) %>%
  mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`,
         `Average Daily Temperature` = factor(model, levels = c("T < 0°C",
                                                                "0 ≤ T < 5°C",
                                                                "5 ≤ T < 10°C",
                                                                "T ≥ 10°C")),
         rate_period = factor(sample, levels = c("Morning Off-peak",
                                                 "Afternoon Off-peak",
                                                 "Peak Rate",
                                                 "Other", 
                                                 "Overall")))


# Define colors with increasing darkness
colors <- c("T < 0°C" = "#AFCBE3",  # Lightest blue
            "0 ≤ T < 5°C" = "#8AAFD4",  # Slightly darker
            "5 ≤ T < 10°C" = "#6694C6", # Medium blue
            "T ≥ 10°C" = "#466CA8")  # Darkest blue

# Create the ggplot
ggplot(coefs %>% filter(rate_period != "Overall"), aes(x = `Average Daily Temperature`, 
                                                       y = Estimate, fill = `Average Daily Temperature`)) +
  geom_col() +  # Use geom_col for pre-computed y values
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  scale_y_continuous(name = "Estimate (kWh)") +  # Set y-axis label
  scale_fill_manual(values = colors) +  # Assign colors with increasing darkness
  labs(fill = "Avg. Daily Temperature") +  # Remove x-axis label, keep legend title
  theme_minimal() +  # Apply minimal theme
  facet_wrap(~ rate_period, scales = "free_y", ncol = 2) +  # Facet with 2 columns and free y-axis scales
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "bottom"       # Place legend at the bottom
  )

ggsave("graphs/cosy_temperature_binned.png",
       width = 17, height = 8, units = "cm")


# Create the ggplot
ggplot(coefs %>% filter(rate_period != "Overall"), aes(x = `Average Daily Temperature`, 
                                                       y = Estimate, fill = `Average Daily Temperature`)) +
  geom_col() +  # Use geom_col for pre-computed y values
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  scale_y_continuous(name = "Estimate (kWh)") +  # Set y-axis label
  scale_fill_manual(values = colors) +  # Assign colors with increasing darkness
  labs(fill = "Daily Temperature",
      title = "Heat pump time-of-use tariff impacts by temperature") +  # Remove x-axis label, keep legend title
  theme_minimal() +  # Apply minimal theme
  facet_wrap(~ rate_period, scales = "free_y", ncol = 2) +  # Facet with 2 columns and free y-axis scales
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "bottom"       # Place legend at the bottom
  )

ggsave("graphs/cosy_temperature_binned_blog_version.png",
       width = 17, height = 8, units = "cm")

# Unique periods 
periods <- unique(aggregated_data$rate_period)

rm(list = ls(pattern = "^m_"))
gc()


# Run the regression model
tempreg <- feols(consumption_hh ~ i(cosy_contract_active, temp_degree, ref=0) |
                   temp_degree,
                 data = aggregated_data %>% 
                   mutate(temp_degree = factor(
                     case_when(
                       daily_avg_air_temperature_celsius < 0 ~ 0,
                       daily_avg_air_temperature_celsius < 25.5 ~ round(daily_avg_air_temperature_celsius),
                       TRUE ~ 25
                     )
                   )),
                 split = ~ rate_period,
                 cluster = ~account_id)

# Loop through each model in tempreg to create plots
for (i in 1:length(tempreg)) {
  
  # Find model
  val <- tempreg[[i]]$model_info$sample$value
  j <- which(sapply(1:length(m1), function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors
  coefs <- coeftable(tempreg[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    as_tibble() %>%
    separate(term, into = c("cosy_contract_active", "remove1", "daily_avg_air_temperature_celsius", "remove2"), sep = "::") %>%
    mutate(daily_avg_air_temperature_celsius = as.numeric(daily_avg_air_temperature_celsius),
           lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`
    ) %>%
    mutate(`/% ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,     
           lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / abs(m1[[j]]$coefficients) * 100),
           upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / abs(m1[[j]]$coefficients) * 100)
    )
  
  # Create the ggplot
  ggplot(coefs, aes(x = daily_avg_air_temperature_celsius, y = Estimate)) +
    geom_point(color = cosy_color) +  # Points in the Cosy palette color
    geom_line(color = cosy_color) +   # Line in the Cosy palette color
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = cosy_color) +  # Error bars in the Cosy palette color
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line at 100% ATE
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    scale_y_continuous(
      name = "Estimate (kWh)", 
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    labs(
      x = "Daily Temperature in Degrees",
    ) +
    theme_minimal()
  
  # Print the plot
  ggsave(paste0("graphs/cosy_temperature_", tolower(gsub(" ", "_", val)), ".png"),
         width = 16, height = 8, units = "cm")
}


# Remove the overall model if it exists
all_coefs <- coeftable(tempreg) %>%
  inner_join(coeftable(m1) %>% select(sample, Estimate) %>% rename(average = Estimate)) %>%
  data.frame() %>%
  filter(sample != "Overall") %>%
  separate(coefficient, into = c("cosy_contract_active", "remove1", "daily_avg_air_temperature_celsius", "remove2"), sep = "::") %>%
  mutate(daily_avg_air_temperature_celsius = as.numeric(daily_avg_air_temperature_celsius),
         lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`
  ) %>%
  mutate(`/% ATE` = Estimate / abs(average) * 100,     
         lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / average * 100),
         upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / average * 100)
  ) %>%
  mutate(sample = factor(sample, levels = c("Morning Off-peak",
                                            "Afternoon Off-peak",
                                            "Peak Rate",
                                            "Other", 
                                            "Overall")))

# Create the ggplot
ggplot(all_coefs, aes(x = daily_avg_air_temperature_celsius, y = Estimate)) +
  geom_point(color = cosy_color) +  # Points in the Cosy palette color
  geom_line(color = cosy_color) +   # Line in the Cosy palette color
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = cosy_color) +  # Error bars in the Cosy palette color
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line for the average
  scale_y_continuous(
    name = "Estimate (kWh)"  ) +
  labs(
    x = "Daily Temperature in Degrees (°C)",
  ) +
  theme_minimal() +
  facet_wrap(~sample)

# Print the plot
ggsave("graphs/cosy_temperature_all.png", width = 16, height = 8, units = "cm")

rm(list = ls(pattern = "^m[0-9]_"))
gc()


## Using share estimation
df <- aggregated_data %>% 
                   mutate(temp_degree = factor(
                     case_when(
                       daily_avg_air_temperature_celsius < 0 ~ 0,
                       daily_avg_air_temperature_celsius < 25.5 ~ round(daily_avg_air_temperature_celsius),
                       TRUE ~ 25
                     )
                   )) %>%
                  filter(rate_period != "overall") %>%        # drop overall
                  mutate(date = as.Date(date)) %>%             # Ensure date is stored as a Date object
                  group_by(account_id, date) %>%
                  mutate(
                    daily_total = sum(consumption_hh, na.rm = TRUE),
                    share_daily = consumption_hh / daily_total
                  ) %>%
                  ungroup()

tempreg <- feols(share_daily ~ i(cosy_contract_active, temp_degree, ref=0) |
                   temp_degree,
                 data = df,
                 split = ~ rate_period,
                 cluster = ~account_id)
m1_share <-   feols(share_daily ~ i(cosy_contract_active, ref=0) |
                   temp_degree,
                 data = df,
                 split = ~ rate_period,
                 cluster = ~account_id)                  

# Get coefficient tables
all_coefs <- coeftable(tempreg) %>%
  data.frame() %>%
  # keep only the i() terms (avoid intercept/other terms if any)
  filter(str_detect(coefficient, "^cosy_contract_active::")) %>%
  # Drop the Overall sample if present in the data
  filter(!tolower(sample) %in% "overall") %>%
  # Extract the temp bin from the coefficient name:
  # expected like: "cosy_contract_active::1:temp_degree::5" (exact pattern depends on fixest)
  mutate(
    temp_degree = str_extract(coefficient, "(?<=temp_degree::)\\-?\\d+"),
    temp_degree = as.numeric(temp_degree)
  ) %>%
  # join the "average" line per sample from m1
  inner_join(
    coeftable(m1_share) %>%
      select(sample, Estimate) %>%
      rename(average = Estimate),
    by = "sample"
  ) %>%
  mutate(
    lower_ci = Estimate - 1.96 * `Std..Error`,
    upper_ci = Estimate + 1.96 * `Std..Error`,
    `/% ATE` = Estimate / abs(average) * 100,
    lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / abs(average) * 100),
    upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / abs(average) * 100)
  ) %>%
  mutate(
    sample = factor(sample, levels = c(
      "Morning Off-peak",
      "Afternoon Off-peak",
      "Peak Rate",
      "Other"
    ))
  )

# Plot of the share-of-consumption estimates, labeled in percentage points rather than kWh
p <- ggplot(all_coefs, aes(x = temp_degree, y = Estimate)) +
  geom_point(color = cosy_color) +
  geom_line(color = cosy_color) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.2, alpha = 0.6, color = cosy_color) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +
  scale_y_continuous(name = "Change in daily share (pp)") +
  labs(x = "Daily Temperature in Degrees (°C)") +
  theme_minimal() +
  facet_wrap(~sample)

ggsave("graphs/cosy_temperature_share_all.png", plot = p, width = 16, height = 8, units = "cm")

# ===========================================================================
### Table A.12: Cosy Adoption by Previous Tariff Type
# ===========================================================================

# Register the pre-treatment average fit statistic
fitstat_register("pre_avg_tou", function(x) {
  
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
  pre_avg <- mean(outcome_values[data_used$previous_is_charged_half_hourly == 1 & data_used$cosy_contract_active == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Half Hourly Consumption ToU")


# Register the pre-treatment average fit statistic
fitstat_register("pre_avg_nontou", function(x) {
  
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
  pre_avg <- mean(outcome_values[data_used$previous_is_charged_half_hourly == 0 & data_used$cosy_contract_active == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Half Hourly Consumption Non-ToU")


# Same 6,631-household CS-estimable sample used in did.tex/cosy_did_cs.tex
# (cached by 01_06_DiD_analysis.R; re-run that script first if this is missing)
mpans <- readRDS(file.path(datapath, "scratch/cosy_mpans_universe.RDS"))

m3 <- feols(consumption_hh ~ i(cosy_contract_active) +  i(cosy_contract_active, previous_is_charged_half_hourly, ref=0) |
              hdd + account_id + date,
            data = aggregated_data %>% filter(hashed_mpan %in% mpans),
            cluster = ~account_id,
            split = ~ rate_period)


etable(m3, title = "Adoption by Previous Tariff Type", 
       tex = TRUE,
       label = "tab:prevrav",
       fitstat = ~ N + g + pre_avg_nontou + pre_avg_tou +t_obs + r2,
       dict = c(previous_is_charged_half_hourly = "Prev is ToU", 
               cosy_contract_active = "Contract Active", 
                rate_period = "Rate Period",
                consumption_hh = "Consumption in kWh", 
                g = "Number of Households",
               account_id = "Household", 
               hdd = "HDD", 
               day = "Day"), 
       file = "tables/did_prevar.tex", replace = TRUE)

# Read the generated LaTeX file


# Read the generated LaTeX file
file_content <- readLines("tables/did_prevar.tex")

# Find the lines with the pre-treatment average and remove them
pre_avg_line_index <- grep("Half Hourly Consumption", file_content)[1]
pre_avg_line_index2 <- grep("Half Hourly Consumption", file_content)[2]
pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index2)]
file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]

# Find the position just after the coefficients
coeff_end_index <- grep("Contract Active", file_content)[2] + 2
if (length(coeff_end_index) > 1) {
  coeff_end_index <- coeff_end_index[-1]
}

# Insert the pre-treatment average row after the coefficients
file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)

file_content <- append(file_content,"\\emph{Pre-Treatment Average}&  & & & & \\\\  '\\", after = coeff_end_index)

# Add a \midrule after the pre-treatment average
file_content <- append(file_content, "\\midrule", after = coeff_end_index+3)

# Write the modified content back to the LaTeX file
writeLines(file_content, "tables/did_prevar.tex")  




# ===========================================================================
### Figure A.29: Impact of Cosy Adoption by EAC on Consumption and Figure A.30: Impact of Cosy Adoption by EAC on Share of Consumption
# ===========================================================================
rm(tempreg)

# Create unique breaks for eac_mwh
breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$eac_mwh),]$eac_mwh, probs = seq(0, 1, by = 0.1)))

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i]), "MWh to ", round(breaks[i+1]), "MWh"))

# Create the categories for eac_mwh
aggregated_data <- aggregated_data %>%
  mutate(eac_mwh_category = cut(eac_mwh, 
                                breaks = breaks, 
                                include.lowest = TRUE,
                                labels = labels))

# Run the regression models
tempreg_total <- feols(consumption_hh ~ i(cosy_contract_active, eac_mwh_category, ref =0) 
                       | date +  account_id + hdd,
                       data = aggregated_data %>% filter(!is.na(eac_mwh)),
                       split = ~ rate_period,
                       cluster = ~account_id)

tempreg_share <- feols(share_consumption ~ i(cosy_contract_active, eac_mwh_category, ref =0) 
                       | date +  account_id + hdd,
                       data = aggregated_data %>% 
                         filter(!is.na(eac_mwh), !rate_period=="Overall") %>%
                         group_by(account_id, date) %>%
                         mutate(share_consumption = total_consumption/sum(total_consumption)),
                       split = ~ rate_period,
                       cluster = ~account_id)

# share
m1_share <- feols(share_consumption ~ i(cosy_contract_active) | hdd + account_id + date, 
                  data = aggregated_data %>% 
                    filter(!is.na(eac_mwh), !rate_period=="Overall") %>%
                    group_by(account_id, date) %>%
                    mutate(share_consumption = total_consumption/sum(total_consumption)), 
                  cluster = ~account_id, 
                  split = ~ rate_period)

for (i in 1:5) {
  
  # Find model
  val <- tempreg_total[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(tempreg_total[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "EAC MWh Category", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`EAC MWh Category` = factor(`EAC MWh Category`, levels = labels))
  if (i<5) {
    # Extract coefficients and standard errors for share_consumption
    coefs_share <- coeftable(tempreg_share[i]) %>%
      data.frame() %>%
      separate(coefficient, into = c("cosy_contract_active", "remove1", "EAC MWh Category", "remove2"), sep = "::") %>%
      mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
             upper_ci = Estimate + 1.96 * `Std..Error`,
             outcome = "Share Consumption") %>%
      mutate(`EAC MWh Category` = factor(`EAC MWh Category`, levels = labels))
  }
  # Choose a color palette that can handle more than 9 categories
  brewer_colors <- scales::hue_pal()(length(unique(coefs_share$`EAC MWh Category`)))
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `EAC MWh Category`, y = Estimate , fill = `EAC MWh Category`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci , ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha=0.6) +  # Add horizontal line at ATE
    scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
    labs(
      x = "EAC MWh Decile",
    ) +
    scale_y_continuous(
      name = "Estimate (kWh)", 
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/eac_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
  if (i<5) {
    # Create the ggplot
    ggplot(coefs_share, aes(x = `EAC MWh Category`, y = Estimate, fill = `EAC MWh Category`)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
      geom_hline(yintercept = m1_share[[i]]$coefficients, linetype = "dashed", color = cosy_color, alpha=0.6) +  # Add horizontal line at ATE
      scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
      labs(
        x = "EAC MWh Decile",
      ) +
      scale_y_continuous(
        name = "% of Daily Consumption",
        labels = scales::percent_format(),
        sec.axis = sec_axis(~ ./m1_share[[i]]$coefficients, name = "% of ATE", labels = scales::percent_format())
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
        legend.position = "none"  # Remove legend
      )
    
    # Print the plot
    ggsave(paste0("graphs/eac_share_", tempreg_total[[i]]$model_info$sample$value %>% tolower() %>% str_replace(" ", "_"), ".png"),
           width = 16, height = 8, units = "cm")
  }
}


# Extract coefficients and standard errors for each model and convert to % of ATE
extract_and_combine_coefs <- function(tempreg_model, m1_model, labels) {
  all_coefs <- data.frame()
  
  for (i in 1:length(tempreg_model)) {
    val <- tempreg_model[[i]]$model_info$sample$value
    j <- which(sapply(1:length(m1_model), function(j) m1_model[[j]]$model_info$sample$value) == val)
    
    coefs <- coeftable(tempreg_model[[i]]) %>%
      data.frame() %>%
      tibble::rownames_to_column("term") %>%
      as_tibble() %>%
      separate(term, into = c("cosy_contract_active", "remove1", "EAC_MWh_Category", "remove2"), sep = "::") %>%
      mutate(
        average = m1_model[[j]]$coefficients,
        EAC_MWh_Category = factor(EAC_MWh_Category, levels = labels),
        lower_ci = Estimate - 1.96 * Std..Error,
        upper_ci = Estimate + 1.96 * Std..Error,
        `%_ATE` = Estimate / abs(m1_model[[j]]$coefficients) * 100,
        lower_ci_ATE = `%_ATE` - 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
        upper_ci_ATE = `%_ATE` + 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
        outcome = ifelse(grepl("share", deparse(substitute(tempreg_model))), "Share Consumption", "Total Consumption"),
        period = factor(val, levels = c("Morning Off-peak",
                                        "Afternoon Off-peak",
                                        "Peak Rate",
                                        "Other", 
                                        "Overall")))
    
    all_coefs <- bind_rows(all_coefs, coefs)
  }
  
  return(all_coefs)
}

# Combine all coefficients
all_coefs_total <- extract_and_combine_coefs(tempreg_total, m1, labels)
all_coefs_share <- extract_and_combine_coefs(tempreg_share, m1_share, labels)
all_coefs <- bind_rows(all_coefs_total, all_coefs_share)

# Create the combined ggplot using facet_wrap
ggplot(all_coefs_share %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line for the average
  scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
  labs(
    x = "EAC MWh Decile",
  ) +
  scale_y_continuous(
    name = "Estimate (kWh)", 
    labels = scales::percent_format()
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)

# Save the combined plot
ggsave("graphs/eac_share_combined.png", device = "png", width = 16, height = 12, dpi = 300)


# Create the combined ggplot using facet_wrap
ggplot(all_coefs_total %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line for the average
  scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
  labs(
    x = "EAC MWh Decile",
  ) +
  scale_y_continuous(
    name = "Estimate (kWh)", 
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)

# Save the combined plot
ggsave("graphs/eac_combined.png", device = "png", width = 16, height = 12, dpi = 300)

# List all objects in the environment
rm(list = ls(pattern = "^m_"))
gc()

# ===========================================================================
### Figure A.28: Impact of Cosy Adoption by EPC Score on Consumption
# ===========================================================================
m2a <- feols(consumption_hh ~ i(cosy_contract_active, epc_letter, ref=0)  | 
               hdd + date + account_id,
             data = aggregated_data, 
             split = ~ rate_period,
             cluster = ~account_id)

# share
m1_share <- feols(share_consumption ~ i(cosy_contract_active) | hdd + account_id + date, 
                  data = aggregated_data %>% 
                    filter(!is.na(epc_letter), !rate_period=="Overall") %>%
                    group_by(account_id, date) %>%
                    mutate(share_consumption = total_consumption/sum(total_consumption)), 
                  cluster = ~account_id, 
                  split = ~ rate_period)

# Define colors to match the Energy Efficiency Rating chart
rating_colors <- c(
  "A" = "#00CC00",  # Green
  "B" = "#66FF33",  # Light Green
  "C" = "#FFFF00",  # Yellow
  "D" = "#FF9900",  # Orange
  "E" = "#FF6600",  # Dark Orange
  "F" = "#FF0000",  # Red
  "G" = "#990000"   # Dark Red
)

for (i in 1:5) {
  # Find model
  val <- m2a[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors
  coefs <- coeftable(m2a[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "EPC letter", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`
    )
  # coefs holds the extracted coefficients used in the plot below; rating_colors (defined above) supplies the EPC letter fill colors
  coefs <- coefs %>%
    mutate(EPC_label_position = max(Estimate) * 0.1)  # Position for EPC labels on the right

  # Create the ggplot
  ggplot(coefs, aes(y = rev(factor(`EPC letter`)), x = Estimate, fill = factor(`EPC letter`))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
    geom_vline(xintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha=0.6) +  # Add vertical line at ATE
    geom_text(aes(x = EPC_label_position, label = `EPC letter`), hjust = 0, color = "white") +  # Add EPC letters on the right
    scale_fill_manual(values = rating_colors, name = "EPC Letter") +  # Use the defined colors+
    labs(
      x = "Half Hourly Consumption",
      y = "EPC Letter"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),  # Hide original y-axis text
      axis.ticks.y = element_blank(),  # Hide original y-axis ticks
      axis.text.y.right = element_text(hjust = 0.5),  # Center the text on the right-hand side
      axis.title.y.right = element_text(margin = margin(l = 10)),  # Add margin to right y-axis title
      legend.position = "none"  # Remove legendx
    )
  
  
  # Print the plot
  ggsave(paste0("graphs/epc_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
}

# m2a holds the per-period EPC models, m1 supplies the corresponding ATE values, and rating_colors (defined above) supplies the EPC letter fill colors
all_coefs <- data.frame()

for (i in 1:5) {
  # Find model
  val <- m2a[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors
  coefs <- coeftable(m2a[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "EPC_letter", "remove2"), sep = "::") %>%
    mutate(
      average = m1[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * Std..Error,
      upper_ci = Estimate + 1.96 * Std..Error,
      `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
      period = val
    )
  
  # Combine all coefficients
  all_coefs <- bind_rows(all_coefs, coefs)
}

# all_coefs holds the combined coefficient table used in the plot below; rating_colors (defined above) supplies the EPC letter fill colors
all_coefs <- all_coefs %>%
  mutate(EPC_label_position = max(`Estimate`) * 0.1)  # Position for EPC labels on the right

# Create the ggplot
ggplot(all_coefs %>% filter(period!="Overall"), aes(y = factor(EPC_letter, levels = rev(unique(EPC_letter))), x = Estimate, fill = factor(EPC_letter))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, color="grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  geom_vline(aes(xintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  geom_text(aes(x = EPC_label_position, label = EPC_letter), color = "black") +  # Add EPC letters on the right
  scale_fill_manual(values = rating_colors, name = "EPC Letter") +  # Use the defined colors
  labs(
    x = "Estimate (kWh)",
    y = "EPC Letter"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Hide original y-axis text
    axis.ticks.y = element_blank(),  # Hide original y-axis ticks
    axis.text.y.right = element_text(hjust = 0.5),  # Center the text on the right-hand side
    axis.title.y.right = element_text(margin = margin(l = 10)),  # Add margin to right y-axis title
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period, scales = "free_y")

# Print the plot
ggsave("graphs/cosy_epc_combined.png", width = 16, height = 8, units = "cm")

rm(m2a)


# ===========================================================================
### Figure A.33: Impact of Cosy Adoption by Heat Loss on Consumption and Figure A.34: Impact of Cosy Adoption by Heat Loss on Share of Consumption
# ===========================================================================
# Create unique breaks for predicted_heatloss_watts
breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$predicted_heatloss_watts),]$predicted_heatloss_watts/1000, probs = seq(0, 1, by = 0.1)))

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i], 1), " kW to ", round(breaks[i+1], 1), " kW"))

# Create the categories for predicted_heatloss_watts
aggregated_data <- aggregated_data %>%
  mutate(predicted_heatloss_watts_category = cut(predicted_heatloss_watts/1000, 
                                                 breaks = breaks, 
                                                 include.lowest = TRUE,
                                                 labels = labels))


m_heatloss <- feols(consumption_hh ~ i(cosy_contract_active, predicted_heatloss_watts_category, ref =0) 
                    | date +  account_id + hdd,
                    data = aggregated_data %>% filter(!is.na(predicted_heatloss_watts_category)),
                    split = ~ rate_period,
                    cluster = ~account_id)

for (i in 1:5) {
  
  # Find model
  val <- m_heatloss[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_heatloss[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Heatloss MW Category", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Heatloss MW Category` = factor(`Heatloss MW Category`, levels = labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Heatloss MW Category`, y = Estimate, fill = `Heatloss MW Category`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Heatloss MW Decile",
    ) +
    scale_y_continuous(
      name = "Estimate (kWh)", 
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
      
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/heatloss_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}


m_heatloss2 <- feols(share_consumption ~ i(cosy_contract_active, predicted_heatloss_watts_category, ref =0)
                     | date +  account_id + hdd,
                     data = aggregated_data %>% 
                       filter(!is.na(predicted_heatloss_watts_category), !rate_period=="Overall") %>%
                       group_by(account_id, date) %>%
                       mutate(share_consumption = total_consumption/sum(total_consumption)),
                     split = ~ rate_period,
                     cluster = ~account_id)

for (i in 1:4) {
  
  # Find model
  val <- m_heatloss2[[i]]$model_info$sample$value
  j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_heatloss2[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Heatloss MW Category", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Heatloss MW Category` = factor(`Heatloss MW Category`, levels = labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Heatloss MW Category`, y = Estimate, fill = `Heatloss MW Category`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Heatloss MW Decile",
    ) +
    scale_y_continuous(
      name = "% of Daily Consumption",
      labels = scales::percent_format(),
      sec.axis = sec_axis(~ ./m1_share[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/share_heatloss_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}

# ===========================================================================
# Extract coefficients and standard errors for each model and convert to % of ATE
# ===========================================================================
extract_and_combine_coefs <- function(tempreg_model, m1_model, labels) {
  all_coefs <- data.frame()
  
  for (i in 1:length(tempreg_model)) {
    val <- tempreg_model[[i]]$model_info$sample$value
    j <- which(sapply(1:length(m1_model), function(j) m1_model[[j]]$model_info$sample$value) == val)
    
    coefs <- coeftable(tempreg_model[[i]]) %>%
      data.frame() %>%
      tibble::rownames_to_column("term") %>%
      as_tibble() %>%
      separate(term, into = c("cosy_contract_active", "remove1", "EAC_MWh_Category", "remove2"), sep = "::") %>%
      mutate(
        average = m1_model[[j]]$coefficients,
        EAC_MWh_Category = factor(EAC_MWh_Category, levels = labels),
        lower_ci = Estimate - 1.96 * Std..Error,
        upper_ci = Estimate + 1.96 * Std..Error,
        `%_ATE` = Estimate / abs(m1_model[[j]]$coefficients) * 100,
        lower_ci_ATE = `%_ATE` - 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
        upper_ci_ATE = `%_ATE` + 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
        outcome = ifelse(grepl("share", deparse(substitute(tempreg_model))), "Share Consumption", "Total Consumption"),
        period = factor(val, levels = c("Morning Off-peak",
                                        "Afternoon Off-peak",
                                        "Peak Rate",
                                        "Other", 
                                        "Overall")))
    
    all_coefs <- bind_rows(all_coefs, coefs)
  }
  
  return(all_coefs)
}


# Combine all coefficients
all_coefs_total <- extract_and_combine_coefs(m_heatloss, m1, labels)
all_coefs_share <- extract_and_combine_coefs(m_heatloss2, m1_share, labels)
all_coefs <- bind_rows(all_coefs_total, all_coefs_share)

# Create the combined ggplot using facet_wrap
ggplot(all_coefs_share %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Heatloss MW Decile",
  ) +
  scale_y_continuous(
    name = "% of Daily Consumption",
    labels = scales::percent_format(),
  ) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)

# Save the combined plot
ggsave("graphs/heatloss_share_combined.png", device = "png", width = 16, height = 12, dpi = 300)


# Create the combined ggplot using facet_wrap
ggplot(all_coefs_total %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Heatloss MW Decile",
  ) +
  scale_y_continuous(
    name = "Estimate (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)

# Save the combined plot
ggsave("graphs/heatloss_combined.png", device = "png", width = 16, height = 12, dpi = 300)

# List all objects in the environment
rm(list = ls(pattern = "^m_"))
gc()

# ===========================================================================
### Figure A.31: Impact of Cosy Adoption by Floor Area on Consumption and Figure A.32: Impact of Cosy Adoption by Floor Area on Share of Consumption
# ===========================================================================
gc()

# Create unique breaks for total_floor_area
breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$total_floor_area),]$total_floor_area, probs = seq(0, 1, by = 0.1)))

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i], 0), " to ", round(breaks[i+1], 0), " m sq."))

# Create the categories for total_floor_area
aggregated_data <- aggregated_data %>%
  mutate(total_floor_area_category = cut(total_floor_area, 
                                         breaks = breaks, 
                                         include.lowest = TRUE,
                                         labels = labels))


m_floor <- feols(consumption_hh ~ i(cosy_contract_active, total_floor_area_category, ref =0) 
                 | date +  account_id + hdd,
                 data = aggregated_data %>% filter(!is.na(total_floor_area_category)),
                 split = ~ rate_period,
                 cluster = ~account_id)

for (i in 1:5) {
  
  # Find model
  val <- m_floor[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_floor[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Floor Area", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Floor Area` = factor(`Floor Area`, levels = labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Floor Area`, y = Estimate, fill = `Floor Area`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Floor Area Decile",
      y = "Half Hourly Consumption in kWh"
    ) +
    scale_y_continuous(
      name = "Estimate (kWh)", 
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/floor_area_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}


m_floor_share <- feols(share_consumption ~ i(cosy_contract_active, total_floor_area_category, ref =0)
                       | date +  account_id + hdd,
                       data = aggregated_data %>% 
                         filter(!is.na(total_floor_area_category), !rate_period=="Overall") %>%
                         group_by(account_id, date) %>%
                         mutate(share_consumption = total_consumption/sum(total_consumption)),
                       split = ~ rate_period,
                       cluster = ~account_id)

for (i in 1:4) {
  
  # Find model
  val <- m_floor_share[[i]]$model_info$sample$value
  j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_floor_share[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Floor Area", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Floor Area` = factor(`Floor Area`, levels = labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Floor Area`, y = Estimate, fill = `Floor Area`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Floor Area Decile",
      y = "Half Hourly Consumption in kWh"
    ) +
    scale_y_continuous(
      name = "% of Daily Consumption",
      labels = scales::percent_format(),
      sec.axis = sec_axis(~ ./m1_share[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/share_floor_area_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}

# Initialize an empty data frame to store all coefficients
all_coefs <- data.frame()

# Extract coefficients for total consumption
for (i in 1:5) {
  val <- m_floor[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  coefs_total <- coeftable(m_floor[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "Floor_Area", "remove2"), sep = "::") %>%
    mutate(
      average = m1[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
      outcome = "Total Consumption",
      `Floor_Area` = factor(`Floor_Area`, levels = labels),
      period = factor(val, levels = c("Morning Off-peak",
                                      "Afternoon Off-peak",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  all_coefs <- bind_rows(all_coefs, coefs_total)
}

# Extract coefficients for share consumption
for (i in 1:4) {
  val <- m_floor_share[[i]]$model_info$sample$value
  j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
  
  coefs_share <- coeftable(m_floor_share[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "Floor_Area", "remove2"), sep = "::") %>%
    mutate(
      average = m1_share[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1_share[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1_share[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1_share[[j]]$coefficients) * 100,
      period = val,
      outcome = "Share Consumption",
      `Floor_Area` = factor(`Floor_Area`, levels = labels),
      period = factor(val, levels = c("Morning Off-peak",
                                      "Afternoon Off-peak",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  all_coefs <- bind_rows(all_coefs, coefs_share)
}

# Define the shades of reds
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")

# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Share Consumption"), aes(x = `Floor_Area`, y = Estimate, fill = `Floor_Area`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Floor Area Decile"
  ) +
  scale_y_continuous(
    name = "Share of Daily Comsumption (%)", 
    labels = scales::percent_format(),
  ) +  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period, scales = "free_y")


# Save the combined plot
ggsave("graphs/floor_area_share_combined.png", device = "png", width = 16, height = 12, units = "cm")



# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
       aes(x = `Floor_Area`, y = Estimate, fill = `Floor_Area`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color="grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Floor Area Decile",
  ) +
  scale_y_continuous(
    name = "Estimate (kWh)"  ) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)


# Save the combined plot
ggsave("graphs/floor_area_combined.png", device = "png", width = 16, height = 12, units = "cm")


rm(list = ls(pattern = "^m_"))
gc()


### Property values

# Create unique breaks for property_value
breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$property_value),]$property_value, 
                          probs = seq(0, 1, by = 0.1)))

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0("£", round(breaks[i]/1000, 0), "k to £", 
                                                          round(breaks[i+1]/1000, 0), "k"))
labels[length(labels)] <- paste0("£", round(breaks[length(breaks)-1]/1000), "+")

# Create the categories for property_value
aggregated_data <- aggregated_data %>%
  mutate(property_value_category = cut(property_value, 
                                       breaks = breaks, 
                                       include.lowest = TRUE,
                                       labels = labels))


m_property_value <- feols(consumption_hh ~ i(cosy_contract_active, property_value_category, ref =0) 
                          | date +  account_id + hdd,
                          data = aggregated_data %>% filter(!is.na(property_value_category)),
                          split = ~ rate_period,
                          cluster = ~account_id)

m_property_value_share <- feols(share_consumption ~ i(cosy_contract_active, property_value_category, ref =0)
                                | date +  account_id + hdd,
                                data = aggregated_data %>% 
                                  filter(!is.na(property_value_category), !rate_period=="Overall") %>%
                                  group_by(account_id, date) %>%
                                  mutate(share_consumption = total_consumption/sum(total_consumption)),
                                split = ~ rate_period,
                                cluster = ~account_id)

# Initialize an empty data frame to store all coefficients
all_coefs <- data.frame()

for (i in 1:5) {
  
  # Find model
  val <- m_property_value[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_property_value[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Property Value", "remove2"), 
             sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption",
           period = factor(sample, levels = c("Morning Off-peak",
                                              "Afternoon Off-peak",
                                              "Peak Rate",
                                              "Other", 
                                              "Overall")),
           `Property Value` = factor(`Property Value`, levels = labels),
           average = m1[[j]]$coefficients)
  
  all_coefs <- bind_rows(all_coefs, coefs_total)
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Property Value Decile",
      y = "Half Hourly Consumption in kWh"
    ) +
    scale_y_continuous(
      name = "Estimate (kWh)", 
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/property_value_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}


for (i in 1:4) {
  
  # Find model
  val <- m_property_value_share[[i]]$model_info$sample$value
  j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_property_value_share[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Property Value", "remove2"), sep = "::") %>%
    mutate(
      average = m1_share[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1_share[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1_share[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1_share[[j]]$coefficients) * 100,
      period = val,
      outcome = "Share Consumption",
      `Property Value` = factor(`Property Value`, levels = labels),
      period = factor(val, levels = c("Morning Off-peak",
                                      "Afternoon Off-peak",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  all_coefs <- bind_rows(all_coefs, coefs_total)
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Property Value Decile",
      y = "Half Hourly Consumption in kWh"
    ) +
    scale_y_continuous(
      name = "% of Daily Consumption",
      labels = scales::percent_format(),
      sec.axis = sec_axis(~ ./m1_share[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/share_property_value_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}


# Define the shades of reds
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")

# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Share Consumption"), 
       aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Property Value Decile"
  ) +
  scale_y_continuous(
    name = "Share of Daily Comsumption (%)", 
    labels = scales::percent_format(),
  ) +  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period, scales = "free_y")


# Save the combined plot
ggsave("graphs/property_value_share_combined.png", device = "png", width = 16, height = 12, units = "cm")



# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
       aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color="grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Property Value Decile",
  ) +
  scale_y_continuous(
    name = "Estimate (kWh)"  ) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)


# Save the combined plot
ggsave("graphs/property_value_combined.png", device = "png", width = 16, height = 12, units = "cm")





### Figure A.36: Impact of Cosy by Region
m_region <- feols(consumption_hh ~ i(cosy_contract_active, region, ref =0)
                  | hdd + account_id + date,
                  data = aggregated_data %>% filter(!is.na(region), !region==""),
                  split = ~ rate_period,
                  cluster = ~account_id)
etable(m_region, tex = TRUE, title = "Cosy Adoption by Region",
       label = "tab:regionaldid",
       fitstat = ~ N + g + pre_avg +t_obs + r2, 
       file = "tables/did_region.tex", replace = TRUE)


# Extract coefficients for total consumption
all_coefs <- data.frame()
for (i in 1:5) {
  val <- m_region[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  coefs_total <- coeftable(m_region[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "Region", "remove2"), sep = "::") %>%
    mutate(
      average = m1[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
      period = factor(val, levels = c("Morning Off-peak",
                                      "Afternoon Off-peak",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  
  all_coefs <- bind_rows(all_coefs, coefs_total)
}

# Select a color palette from RColorBrewer
region_colors <- brewer.pal(n = length(unique(all_coefs$Region)), name = "Set3")

# Order regions by their estimate size for period == "Morning Off-peak"
morning_cosy_order <- all_coefs %>%
  filter(period == "Morning Off-peak") %>%
  arrange(desc(Estimate)) %>%
  pull(Region)

# Reorder the Region factor based on the estimate size in "Morning Off-peak"
all_coefs <- all_coefs %>%
  mutate(Region = factor(Region, levels = morning_cosy_order))

# Create the ggplot
ggplot(all_coefs %>% filter(period != "Overall"), aes(x = Region, y = Estimate, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = region_colors) +  # Apply random colors to regions
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)

# Save the plot
ggsave("graphs/region_combined.png", device = "png", width = 16, height = 12, units = "cm")


rm(list = ls(pattern = "^m_"))
gc()

### Figure A.35: Impact of Cosy Adoption by MSOA Income on Consumption
# Load and preprocess the cosy_hp_details data
cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_07_24.csv")) %>%
  inner_join(aggregated_data %>% select(hashed_mpan) %>% distinct(), by = "hashed_mpan") %>%
  filter(!is.na(postcode)) %>%
  distinct(hashed_mpan, postcode)


postcode_msoa <- fread(file.path(datapath, "input/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")) %>%
  left_join(cosy_hp_details, by = c("pcds" = "postcode")) %>%
  mutate(n = ifelse(is.na(n), 0, 1)) %>%
  select(msoa21cd, pcds) %>%
  distinct(pcds, .keep_all = TRUE)

# Income is natively on 2021 MSOA boundaries (ONS FYE2023 release), matching
# postcode_msoa -- no crosswalk needed here. Matches the source used in
# 01_05_balance_table.R (previously this read the older saiefy1920 vintage).
income <- readxl::read_excel(file.path(datapath, "input/small_area_income_estimates_fye2023.xlsx"), sheet = "Total annual income", skip = 3) %>%
  select(`MSOA code`, `Total annual income (£)`) %>%
  distinct() %>%
  inner_join(postcode_msoa, by=c("MSOA code"="msoa21cd")) %>%
  distinct(pcds, .keep_all = TRUE)


# Create unique breaks for predicted_heatloss_watts
income_dist <- readxl::read_excel(file.path(datapath, "input/small_area_income_estimates_fye2023.xlsx"), sheet = "Total annual income", skip = 3) %>%
  select(`MSOA code`, `Total annual income (£)`)


breaks <- unique(quantile(income_dist$`Total annual income (£)`/1000, probs = seq(0, 1, by = 0.1)))

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0("£", round(breaks[i]), "k-", round(breaks[i+1]), "k"))

df <- aggregated_data %>%
  select(account_id, hdd, hashed_mpan, date, cosy_contract_active, rate_period, total_consumption, consumption_hh) %>%
  left_join(cosy_hp_details) %>%
  left_join(income, by=c("postcode"="pcds")) %>%
  mutate(income_category = cut(`Total annual income (£)`/1000, 
                               breaks = breaks, 
                               include.lowest = TRUE,
                               labels = labels))
# income check
m_income <- feols(consumption_hh ~ i(cosy_contract_active, income_category, ref =0) 
                  | date +  account_id + hdd,
                  data = df,
                  split = ~ rate_period,
                  cluster = ~account_id)


m_income_share <- feols(share_consumption ~ i(cosy_contract_active, income_category, ref =0) 
                        | date +  account_id + hdd,
                        data = df %>% 
                          filter(!is.na(income_category), !rate_period=="Overall") %>%
                          group_by(account_id, date) %>%
                          mutate(share_consumption = total_consumption/sum(total_consumption)),
                        split = ~ rate_period,
                        cluster = ~account_id)


for (i in 1:5) {
  
  # Find model
  val <- m_income[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_income[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Income Category", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Income Category` = factor(`Income Category`, levels = labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Income Category`, y = Estimate, fill = `Income Category`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Income Decile",
      y = "Half Hourly Consumption in kWh"
    ) +
    scale_y_continuous(
      name = "Estimate (kWh)", 
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
      
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/income_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}


# Initialize an empty data frame to store all coefficients
all_coefs <- data.frame()

# Extract coefficients for total consumption
for (i in 1:5) {
  val <- m_income[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  coefs_total <- coeftable(m_income[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "Income Category", "remove2"), sep = "::") %>%
    mutate(
      average = m1[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
      outcome = "Total Consumption",
      `Income Category` = factor(`Income Category`, levels = labels),
      period = factor(val, levels = c("Morning Off-peak",
                                      "Afternoon Off-peak",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  all_coefs <- bind_rows(all_coefs, coefs_total)
}


# Define the shades of reds
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")


# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
       aes(x =`Income Category`, y = Estimate, fill = `Income Category`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "MSOA Income Decile",
  ) +
  scale_y_continuous(
    name = "Estimate (kWh)", 
  ) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period, scales = "free_y")


# Save the combined plot
ggsave("graphs/income_category_combined.png", device = "png", width = 16, height = 12, units = "cm")


all_coefs <- list()
# Extract coefficients for share consumption
for (i in 1:4) {
  val <- m_income_share[[i]]$model_info$sample$value
  j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
  
  coefs_share <- coeftable(m_income_share[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("cosy_contract_active", "remove1", "income_decile", "remove2"), sep = "::") %>%
    mutate(
      average = m1_share[[j]]$coefficients,
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1_share[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1_share[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1_share[[j]]$coefficients) * 100,
      period = val,
      outcome = "Share Consumption",
      `income_decile` = factor(`income_decile`, levels = labels),
      period = factor(val, levels = c("Morning Off-peak",
                                      "Afternoon Off-peak",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  all_coefs <- bind_rows(all_coefs, coefs_share)
}

# Define the shades of reds
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")

# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Share Consumption"), 
       aes(x = `income_decile`, y = Estimate, fill = `income_decile`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = red_palette) +
  labs(
    x = "MSOA Income Decile"
  ) +
  scale_y_continuous(
    name = "Share of Daily Comsumption (%)", 
    labels = scales::percent_format(),
  ) +  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period, scales = "free_y")


# Save the combined plot
ggsave("graphs/income_share_combined.png", device = "png", width = 16, height = 12, units = "cm")
