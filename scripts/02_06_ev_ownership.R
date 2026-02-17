## Table A.3: HP Installation on Electricity Consumption Controlling for EV Ownership 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------------------- Data Cleaning --------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# Load IDs
ids_cs_elec <-  readRDS(file.path(datapath, "scratch/ids_cs_elec.RS"))

# Update hp_installed with the new ev_charging values using case_when
hp_installed <- readRDS(file.path(datapath, "output/hp_installed.rds")) %>%
  left_join(ev_charging_agg) %>%
  left_join(ev_users) %>%
  mutate(has_ev = as.numeric(is_ev_detected <= date),
         has_ev = ifelse(is.na(has_ev), 0, has_ev)) %>%
  filter(account_id %in% ids_cs_elec)  %>%
  filter(week <= 129, firstweek <= 129)  %>%
  filter(week < firstweek - 4 | week >= firstweek) %>%
  filter(rate_period %in% c("Overall")) %>% 
  mutate(total_consumption=365.25*total_consumption)  %>%
               ungroup()

# Fit the model
m1 <- feols(total_consumption ~ i(is_hp_installed, ref=0)  |  
              hdd + account_id + date, 
            data = hp_installed, 
            cluster = ~account_id)

m1c <- feols(total_consumption  ~ i(is_hp_installed, ref=0) + has_ev + i(is_hp_installed, has_ev, ref=0) |
               hdd + account_id + date, 
             data = hp_installed, 
             cluster = ~account_id)

# Generate the initial LaTeX table
etable(m1, m1c, tex = TRUE, title = "HP Installation on Electricity Consumption Controlling for EV Ownership", 
       dict = c("total_consumption" = "Electricity Consumption in kWh"),
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
    Morning_Cosy = ifelse(rate_period == "Morning Off-peak", 1, 0),
    Afternoon_Cosy = ifelse(rate_period == "Afternoon Off-peak", 1, 0),
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
       headers = c("Morning Off-peak", "Afternoon Off-Peak", "Peak Rate", "Other"),
       fitstat = ~ N + g + pre_avg + r2, 
       file = "tables/hp_ev_charging.tex", 
       replace = TRUE, 
       label = "tab:hp-ev-charging",
       dict = c(Morning_Cosy = "Charging EV", 
                Afternoon_Cosy = "Charging EV", 
                Peak_Rate = "Charging EV", 
                Other = "Charging EV"))

# Read tex
file_path <- "tables/hp_ev_charging.tex"
file_content <- readLines(file_path)

# ---- settings you control ----
new_row_name   <- "Baseline charging"          # what the row should be called in the table
new_label_line <- "\\emph{Baseline charging}\\\\"

# where to insert the baseline row (choose an anchor that exists in your table)
# examples: "Is HP Installed", "Treatment", "Constant", etc.
anchor_pattern <- "Is HP Installed"
insert_offset  <- 2   # how many lines after the anchor match to insert
# ------------------------------

# 1) locate the pre-treatment row(s)
pre_idx <- grep("Pre-Treatment Consumption", file_content)

if (length(pre_idx) == 0) stop("Couldn't find 'Pre-Treatment Consumption' in the .tex file.")

# If it appears multiple times, pick the 2nd like you were doing, otherwise the 1st
pre_line_index <- if (length(pre_idx) == 1) pre_idx[1] else pre_idx[2]

# grab the row line (single line) and remove it from file
pre_line <- file_content[pre_line_index]
file_content <- file_content[-pre_line_index]

# 2) rename the row itself (left-hand label inside the row line)
pre_line <- gsub("Pre-Treatment Consumption", new_row_name, pre_line)

# 3) find insertion point using an anchor (more robust than hardcoding one variable)
anchor_hits <- grep(anchor_pattern, file_content)
if (length(anchor_hits) == 0) stop(paste0("Anchor pattern not found: ", anchor_pattern))

insert_after <- anchor_hits[length(anchor_hits)] + insert_offset

# 4) insert: label line + actual baseline row + midrule
file_content <- append(file_content, new_label_line, after = insert_after)
file_content <- append(file_content, pre_line,       after = insert_after + 1)
file_content <- append(file_content, "\\midrule",    after = insert_after + 2)

# 5) rename sample size row label
sample_line <- grep("Size of the 'effective' sample", file_content)
if (length(sample_line) > 0) {
  file_content[sample_line] <- gsub("Size of the 'effective' sample",
                                    "Number of Households",
                                    file_content[sample_line])
}

# write back
writeLines(file_content, file_path)