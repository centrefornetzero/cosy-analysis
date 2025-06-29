## Table A.3: HP Installation on Electricity Consumption Controlling for EV Ownership 

# ev half hours 
# Read the CSV file
ev_charging <- fread("data/input/cosy_-_ev_detection_2024_07_04.csv") %>%
  mutate(ev_charging = 1,
         date = as.Date(interval_start),
         interval_start = as.POSIXct(interval_start, format="%Y-%m-%d %H:%M:%S"),
         hour = as.integer(format(interval_start, "%H")),
         rate_period = case_when(
           hour >= 4 & hour < 7 ~ "Morning Cosy",
           hour >= 13 & hour < 16 ~ "Afternoon Cosy",
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
hp_installed <- hp_installed %>%
  left_join(ev_charging_agg) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging),
         ev_charging = case_when(
           rate_period == "Overall" ~ ev_charging / 48,
           rate_period == "Other" ~ ev_charging / 30,
           TRUE ~ ev_charging / 6
         )
  ) %>%
  left_join(ev_users) %>%
  mutate(has_ev = as.numeric(is_ev_detected <= date),
         has_ev = ifelse(is.na(has_ev), 0, has_ev))

# Fit the model
m1 <- feols(total_consumption ~ i(is_hp_installed, ref=0)  |
              hdd + account_id + date, 
            data = hp_installed %>% 
              filter(treated==1, rate_period %in% c("Overall")) %>% 
              mutate(total_consumption=365.25*total_consumption) %>%
              ungroup(), 
            cluster = ~account_id)

m1c <- feols(total_consumption  ~ i(is_hp_installed, ref=0) + has_ev + i(is_hp_installed, has_ev, ref=0) |
               hdd + account_id + date, 
             data = hp_installed %>% 
               filter(treated==1, rate_period %in% c("Overall")) %>% 
               mutate(total_consumption=365.25*total_consumption) %>%
               ungroup(), 
             cluster = ~account_id)

# Generate the initial LaTeX table
etable(m1, m1c, tex = TRUE, title = "HP Installation on Electricity Consumption Controlling for EV Ownership", 
       dict = c("total_consumption" = "Yearly Consumption in kWh"),
       fitstat = ~ N + g + pre_avg + t_obs + r2, 
       file = "tables/hp_did_ev.tex", replace = TRUE, label = "tab:hp-did-ev")

CleanPreAverage("tables/hp_did_ev.tex")





## Table A.4: HP Installation on Probability of Charging EV by Period

# Identify the period with the highest EV charging for each mpan and date
ev_charging_max <- ev_charging %>%
  group_by(account_id, hashed_mpan, date, rate_period) %>%
  summarise(ev_charging = sum(ev_charging, na.rm = TRUE)) %>%
  group_by(account_id, hashed_mpan, date) %>%
  filter(ev_charging == max(ev_charging)) %>%
  mutate(highest_ev_charging = 1) %>%
  ungroup()

ev_charging_max <- ev_charging_max %>%
  left_join(hp_installed %>% select(account_id, hashed_mpan, date, is_hp_installed) %>% distinct())

# Create dummy variables for rate periods
ev_charging_max <- ev_charging_max %>%
  mutate(
    Morning_Cosy = ifelse(rate_period == "Morning Cosy", 1, 0),
    Afternoon_Cosy = ifelse(rate_period == "Afternoon Cosy", 1, 0),
    Peak_Rate = ifelse(rate_period == "Peak Rate", 1, 0),
    Other = ifelse(rate_period == "Other", 1, 0)
  )

# Run the fixed effects models
m_charging1 <- feols(Morning_Cosy ~ i(is_hp_installed) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
m_charging2 <- feols(Afternoon_Cosy ~ i(is_hp_installed) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
m_charging3 <- feols(Peak_Rate ~ i(is_hp_installed) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
m_charging4 <- feols(Other ~ i(is_hp_installed) | account_id + date, data = ev_charging_max, cluster = ~ account_id)


# Generate the LaTeX table with the dependent variable named "Charging EV"
etable(m_charging1, m_charging2, m_charging3, m_charging4, 
       tex = TRUE, 
       title = "HP Installation on Probability of Charging EV by Period", 
       headers = c("Morning Cosy", "Afternoon Cosy", "Peak Rate", "Other"),
       fitstat = ~ N + g + pre_avg + r2, 
       file = "tables/hp_ev_charging.tex", 
       replace = TRUE, 
       label = "tab:hp-ev-charging",
       dict = c(Morning_Cosy = "Charging EV", 
                Afternoon_Cosy = "Charging EV", 
                Peak_Rate = "Charging EV", 
                Other = "Charging EV"))

# Read the generated LaTeX file
file_path <- "tables/hp_ev_charging.tex"

# Read the generated LaTeX file
file_content <- readLines(file_path)

# Find the lines with the pre-treatment average and remove them
if (length(grep("Charging EV", file_content))==1) {F
  pre_avg_line_index <- grep("Charging EV", file_content)
} else {
  pre_avg_line_index <- grep("Charging EV", file_content)[2]
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
