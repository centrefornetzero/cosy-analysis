# =================================================================
### Table A.9: Adoption on Electricity Consumption Controlling for EV Charging
# =================================================================
# ev half hours 
# Read the CSV file
ev_charging <- fread(file.path(datapath, "input/cosy_-_ev_detection_2024_07_04.csv")) %>%
   mutate(ev_charging = 1,
         date = as.Date(interval_start),
         interval_start = as.POSIXct(interval_start, format="%Y-%m-%d %H:%M:%S"),
         hour = as.integer(format(interval_start, "%H")),
         rate_period = case_when(
           hour >= 4 & hour < 7 ~ "Morning Off-peak",
           hour >= 13 & hour < 16 ~ "Afternoon Off-peak",
           hour >= 16 & hour < 19 ~ "Peak Rate",
           TRUE ~ "Other"
         )
  )

# Aggregate at the account id, mpan, date and rate period level
ev_charging_agg <- rbind(ev_charging %>%
                           group_by(account_id,  hashed_mpan, date, rate_period) %>%
                           tally(),
                         ev_charging %>%
                           group_by(account_id,  hashed_mpan, date) %>%
                           tally() %>% 
                           mutate(rate_period="Overall")) %>%
  rename(ev_charging=n)

# EV users details
ev_users <- ev_charging %>%
  group_by(account_id) %>%
  summarise(is_ev_detected= min(as.Date(interval_start)))

# Update hp_installed with the new ev_charging values using case_when
aggregated_data <- aggregated_data %>%
  left_join(ev_charging_agg) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging),
         ev_charging = case_when(
           rate_period == "Overall" ~ ev_charging / 48,
           rate_period == "Other" ~ ev_charging / 30,
           TRUE ~ ev_charging / 6
         ),
         rate_period = factor(rate_period, levels = c("Morning Off-peak",
                                                      "Afternoon Off-peak",
                                                      "Peak Rate",
                                                      "Other", 
                                                      "Overall"))) %>%
  left_join(ev_users) %>%
  mutate(has_ev = as.numeric(is_ev_detected <= date),
         has_ev = ifelse(is.na(has_ev), 0, has_ev)) %>%
  distinct(account_id, date, rate_period, .keep_all=TRUE)

# Fit the model
m1c <- feols(consumption_hh ~ i(cosy_contract_active, ref=0) + has_ev + i(cosy_contract_active, has_ev, ref=0) | 
               hdd + account_id + date, 
             data = aggregated_data, 
             cluster = ~account_id, 
             split = ~ rate_period)

etable( m1c, cluster = ~ account_id + date)

# Generate the initial LaTeX table
etable(m1c, tex = TRUE, title = "Adoption on Electricity Consumption Controlling for EV Charging", 
       fitstat = ~ N + g + pre_avg + t_obs + r2, 
       file = "tables/did_ev.tex", replace = TRUE, label = "tab:hp-did-ev")
CleanPreAverage("tables/did_ev.tex")


###  Table 3: Adoption on Probability of Charging EV by Period

# Identify the period with the highest EV charging for each mpan and date
ev_charging_max <- ev_charging %>%
  group_by(account_id, hashed_mpan, date, rate_period) %>%
  summarise(ev_charging = sum(ev_charging, na.rm = TRUE)) %>%
  group_by(account_id, hashed_mpan, date) %>%
  filter(ev_charging == max(ev_charging)) %>%
  mutate(highest_ev_charging = 1) %>%
  ungroup()

ev_charging_max <- ev_charging_max %>%
  left_join(aggregated_data %>% select(account_id, hashed_mpan, date, cosy_contract_active) %>% distinct()) 

# Create dummy variables for rate periods
ev_charging_max <- ev_charging_max %>%
  mutate(
    Morning_Cosy = ifelse(rate_period == "Morning Off-peak", 1, 0),
    Afternoon_Cosy = ifelse(rate_period == "Afternoon Off-peak", 1, 0),
    Peak_Rate = ifelse(rate_period == "Peak Rate", 1, 0),
    Other = ifelse(rate_period == "Other", 1, 0)
  )

# Run the fixed effects models
m_charging1 <- feols(Morning_Cosy ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
m_charging2 <- feols(Afternoon_Cosy ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
m_charging3 <- feols(Peak_Rate ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
m_charging4 <- feols(Other ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)


# Generate the LaTeX table with the dependent variable named "Charging EV"
etable(m_charging1, m_charging2, m_charging3, m_charging4, 
       tex = TRUE, 
       title = "Adoption on Probability of Charging EV by Period", 
       headers = c("Morning Off-peak", "Afternoon Off-peak", "Peak Rate", "Other"),
       fitstat = ~ N + g + pre_avg + r2, 
       file = "tables/ev_charging.tex", 
       replace = TRUE, 
       label = "tab:ev-charging",
       dict = c(Morning_Cosy = "Charging EV", 
                Afternoon_Cosy = "Charging EV", 
                Peak_Rate = "Charging EV", 
                Other = "Charging EV"))

file_path <- "tables/ev_charging.tex"

# Read the generated LaTeX file
file_content <- readLines(file_path)

# Find the lines with the pre-treatment average and remove them
if (length(grep("Charging EV", file_content))==1) {
  pre_avg_line_index <- grep("Charging EV", file_content)
} else {
  pre_avg_line_index <- grep("Charging EV", file_content)[2]
}

pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index)]
file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index)]

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

# Modify the name of the dependent in pre-treatment averages
var_line <- grep("Half Hourly Consumption", file_content)
file_content[sample_line] <- gsub("Half Hourly Consumption", "Charging EV", file_content[var_line])

# Add note
note <- "\\floatfoot{\\justifying \\footnotesize \\upshape \\textbf{Note:} We show the results of four OLS models where the dependent variable is whether a charging event occurred in the period of interest – morning off-peak 4am-7am (column 1), afternoon off-peak 1pm-4pm (column 2), peak 4pm-7pm (column 3), and all other hours of the day (column 4). The sample is 127,789 charging events among 1,743 adopters for whom we detect evidence of EV charging. Where a charging events stretches across multiple periods, we attribute it to the period that comprises the \\textit{majority} of the event (in minutes). We see that among these EV owning adopters, adoption is associated with more charging the off-peak period and less in the peak and other periods.}"

file_content <- append(file_content, note, after = grep("\\centering", file_content)-1)

# Write the modified content back to the LaTeX file
writeLines(file_content, file_path)


# =================================================================
### Table A.10: Impact of Cosy for Leavers
# =================================================================
# Function to create the ggplot for each period
create_ggplot <- function(period_data, period_name) {
  # Extract coefficients, standard errors, and event time
  coefficients <- period_data$att.egt   # Convert to daily values
  standard_errors <- period_data$se.egt   # Convert to daily values
  event_time <- period_data$egt
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    event_time = event_time,
    coefficient = coefficients,
    lower_ci = coefficients - 1.96 * standard_errors,
    upper_ci = coefficients + 1.96 * standard_errors,
    period = ifelse(event_time < 0, "No", "Yes")
  )
  
  # Reverse the color order
  plot_data$period <- factor(plot_data$period, levels = c("Yes", "No"))
  
  # Create the ggplot
  p <- ggplot(plot_data, aes(x = event_time, y = coefficient, color = period)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, , alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    scale_color_manual(
      name = "Has Adopted", 
      labels = c("No" = "No", "Yes" = "Yes"),
      values = c("No" = flexible_color, "Yes" = cosy_color)  # Custom colors
    ) +
    # scale_x_continuous(breaks = seq(-50, 50, 10)) +
    labs(
      x = "Weeks since adoption",
      y = "Dynamic ATT for Half Hourly Consumption im kWh"
    ) +
    theme_minimal()
  
  return(p)
}

# Identify the cases where cosy_contract_active switches from 1 to 0
aggregated_data <- aggregated_data %>%
  mutate(leavers = (account_id %in% contract_analysis[!contract_analysis$category == "Stayed (ongoing)",]$account_id))

# Identify when they leave
leave_date <- aggregated_data  %>%
  filter(leavers == 1) %>%
  mutate(no_active_contract = as.numeric(cosy_contract_active==0 & date >= first_adoption)) %>% 
  ungroup() %>%
  group_by(account_id) %>%
  filter(no_active_contract==1) %>%
  summarise(leave_date = min(date, na.rm = TRUE))

# Create weeks since leaving
start_date <- min(leave_date$leave_date)

# Create the data for CS
leavers_did <- aggregated_data %>%
  inner_join(leave_date) %>%
  ungroup() %>%
  mutate(
    # Calculate the difference in weeks from the start_date
    week = as.numeric(difftime(date, start_date, units = "weeks")) %/% 1 + 1,
    # Assuming you have a way to determine 'firstweek', adjust similarly if needed
    firstweek = as.numeric(difftime(leave_date, start_date, units = "weeks")) %/% 1 + 1) %>%
  group_by(hashed_mpan, firstweek, week, rate_period) %>%
  summarise(consumption_hh = mean(consumption_hh)) %>%
  mutate(
    id = as.numeric(hashed_mpan),
    firstweek = as.numeric(firstweek),
    week = as.numeric(week)
  )

m_leavers <- feols(consumption_hh ~ i(cosy_contract_active, ref=0) + i(cosy_contract_active, leavers, ref=0, ref2=0)  | hdd + account_id + date, 
                   data = aggregated_data, 
                   cluster = ~account_id, 
                   split = ~ rate_period)

etable(m_leavers, tex=TRUE, title = "Impact for Leavers",
       fitstat = ~ N + g + pre_avg +t_obs + r2, file = "tables/did_leavers.tex", replace = TRUE, label="tab:did-leavers")
CleanPreAverage("tables/did_leavers.tex")

leavers_ev <- aggregated_data %>%
  distinct(account_id, leavers) %>%
  left_join(ev_users) %>%
  mutate(has_ev = (!is.na(is_ev_detected)))


# Group by leavers and has_ev, then count
grouped_data <- leavers_ev %>%
  group_by(leavers, has_ev) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(leavers) %>%
  mutate(proportion = count / sum(count))

# Plot the bar plot
ggplot(grouped_data, aes(x = as.factor(leavers), y = proportion, fill = as.factor(has_ev))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("skyblue", "orange"), labels = c("No EV", "Has EV")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Leavers", y = "Proportion", fill = "EV Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(filename= "graphs/leavers_ev.png",width = 10, height = 8, dpi = 300)

### Table A.11: Impact of Cosy by LCTs Ownership
survey_responses <- fread(file.path(datapath, "input/cosy_-_smart_tariff_survey_2024_09_12.csv"))

# Step 1: Clean and split 'all_lcts' column without modifying original data
cleaned_lcts <- survey_responses$all_lcts %>%
  str_remove_all("[\\[\\]\"]") %>%
  str_split(",")

# Step 2: Find unique values across all rows
unique_values <- cleaned_lcts %>%
  unlist() %>%
  str_trim() %>%
  unique()

# Step 3: Create new columns for each unique value
for (val in unique_values) {
  # Add column with 1 if the value is present in the row, 0 otherwise, without changing original df
  survey_responses[[val]] <- sapply(cleaned_lcts, function(x) ifelse(val %in% x, 1, 0))
}

survey_responses <- survey_responses %>%
  mutate(`Has EV` = as.numeric(!has_ev == ""),
         `Has EV Charger` = as.numeric(!has_charger_ev=="")) %>%
  select(-c(has_ev, has_charger_ev, charging_method))

# View the resulting dataframe
summary(survey_responses)

df <- aggregated_data %>%
  select(account_id, consumption_hh, cosy_contract_active, date, hdd, rate_period) %>%
  inner_join(survey_responses %>% 
               rename(`Has Solar PV` = `Solar panels or other microgeneration`)) %>%
  filter(account_id %in% unique(survey_responses$account_id))


# Run the regression model
m1_filtered <- feols(consumption_hh ~ i(cosy_contract_active, ref=0) |
                       account_id + date + hdd,
                     data = df,
                     split = ~ rate_period,
                     cluster = ~account_id)
tempreg <- feols(consumption_hh ~ i(cosy_contract_active) +
                   i(cosy_contract_active, `Home battery`, ref=0) +
                   i(cosy_contract_active, `Has Solar PV`, ref=0) +
                   i(cosy_contract_active, `Has EV`, ref=0) |
                   account_id + date + hdd,
                 data = df,
                 split = ~ rate_period,
                 cluster = ~account_id)
etable(m1_filtered)

etable(tempreg, tex=TRUE, title = "Impact by low-carbon technologies",
       fitstat = ~ N + g + pre_avg +t_obs + r2, file = "tables/did_lcts.tex", replace = TRUE, label="tab:did-lcts")

CleanPreAverage("tables/did_lcts.tex")

# Step 2: Create combinations of EV, Solar, and Battery and count the occurrences
survey_responses_filtered <- survey_responses %>%
  filter(account_id %in% unique(aggregated_data$account_id)) 

lct_matrix <-survey_responses_filtered %>%
  group_by(`Has EV`, `Solar panels or other microgeneration`, `Home battery`) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(share = count / sum(count))

# Step 3: Create readable labels for combinations
lct_matrix_wide <- lct_matrix %>%
  mutate(
    # Combine only the values that exist, ignoring any empty or missing LCTs
    Combination = trimws(paste(
      ifelse(`Has EV` == 1, "EV", ""),
      ifelse(`Home battery` == 1, "Battery", ""),
      ifelse(`Solar panels or other microgeneration` == 1, "Solar", "")
    )),
    # Remove any trailing/leading spaces and '+' when no tech is present
    Combination = gsub("\\s+", " + ", Combination),  # Ensures proper spacing
    Combination = gsub("^\\s*\\+\\s*", "", Combination),  # Removes leading '+'
    Combination = gsub("\\s*\\+\\s*$", "", Combination),  # Removes trailing '+'
    # If nothing is in the combination, label it as "No other LCT"
    Combination = ifelse(Combination == "", "No other LCT", Combination)
  ) %>%
  arrange(desc(share))

# Step 4: Create a bar plot with ColorBrewer and no borders
ggplot(lct_matrix_wide, aes(x = reorder(Combination, -share), y = share, fill = Combination)) +
  geom_bar(stat = "identity") +  # No border around bars
  coord_flip() +  # Flip coordinates for easier reading
  scale_fill_brewer(palette = "Set3") +  # Use ColorBrewer scheme
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = " ",
    y = paste0("Proportion of Sample (%) [N=", dim(survey_responses_filtered)[1], ']')
  ) +
  theme_minimal() +
  theme(
    legend.position = "none")  # Remove legend

ggsave("graphs/lct_combinaison.png",
       width = 16, height = 8, units = "cm")


#  Calculate the share of each LCT
lct_summary <- survey_responses_filtered %>%
  summarise(
    `Home battery (%)` = mean(`Home battery`) * 100,
    `Solar PV (%)` = mean(`Solar panels or other microgeneration`) * 100,
    `EV (%)` = mean(`Has EV`) * 100
  ) %>%
  pivot_longer(cols = everything(), names_to = "LCT", values_to = "Share")

