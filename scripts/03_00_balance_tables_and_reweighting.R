# ============================================================
# Matching + Balance + TWFE (Cosy tariff & HP adopters)
# Cleaned script: preserves your file paths + variable names
# ============================================================

# The MatchIt, lmtest and sandwich libraries are used.
library(MatchIt)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
library(tidyr)
library(data.table)
library(fixest)

# ----------------------------
# 0) Fixest dictionary
# ----------------------------
setFixest_dict(c(
  total_consumption = "Consumption in kWh per period",
  daily_consumption = "Consumption in kWh per day",
  consumption_hh = "Half Hourly Consumption in kWh",
  share_consumption = "Share of daily consumption",
  weekly_consumption = "Gas Consumption per Week in kWh",
  cosy_contract_active = "Contract Active",
  daily_avg_heating_degree = "HDD",
  hdd = "HDD",
  avg_heating_degree = "HDD",
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
  has_ev = "EV User"
))

# ----------------------------
# 1) Helper functions
# ----------------------------

format_number <- function(number) {
  format(round(number), big.mark = ",", scientific = FALSE)
}

format_decimal <- function(number, digits = 4) {
  format(round(number, digits), nsmall = digits, big.mark = ",", scientific = FALSE)
}

# Apply consistent labeling to MatchIt summary tables
clean_matchit_summary <- function(ms, treated_label, control_label) {
  out <- ms
  for (nm in c("sum.all", "sum.matched")) {
    if (!is.null(out[[nm]]) && nrow(out[[nm]]) > 0) {
      rownames(out[[nm]]) <- gsub("_", " ", rownames(out[[nm]]))
      rownames(out[[nm]]) <- tools::toTitleCase(rownames(out[[nm]]))
      colnames(out[[nm]]) <- gsub("Treated", treated_label, colnames(out[[nm]]))
      colnames(out[[nm]]) <- gsub("Control", control_label, colnames(out[[nm]]))
    }
  }
  if (!is.null(out$nn) && nrow(out$nn) > 0) {
    colnames(out$nn) <- c(control_label, treated_label)
  }
  out
}

# One consistent cleaner for etable output (moves pre_avg line into main body)
CleanPreAverage <- function(file_path, pre_avg_label, coeff_anchor_pattern = "Fixed-effects") {
  file_content <- readLines(file_path)

  # locate the pre_avg line
  idx <- grep(pre_avg_label, file_content)
  if (length(idx) == 0) return(invisible(NULL))

  # choose the 2nd occurrence if it exists (your original logic)
  pre_avg_line_index <- if (length(idx) == 1) idx else idx[2]

  pre_avg_lines <- file_content[pre_avg_line_index]
  file_content <- file_content[-c(pre_avg_line_index)]

  # insert after coefficients: default anchor "Fixed-effects"
  coeff_end_index <- grep(coeff_anchor_pattern, file_content)
  if (length(coeff_end_index) == 0) {
    # fallback: insert near the end before sample size if anchor missing
    coeff_end_index <- grep("Size of the 'effective' sample", file_content)
    coeff_end_index <- if (length(coeff_end_index) == 0) length(file_content) else coeff_end_index[1] - 1
  } else {
    coeff_end_index <- coeff_end_index[1] - 2
  }

  file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
  file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
  file_content <- append(file_content, "\\midrule", after = coeff_end_index)

  # rename effective sample label
  sample_line <- grep("Size of the 'effective' sample", file_content)
  if (length(sample_line) > 0) {
    file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
  }

  writeLines(file_content, file_path)
}

# Register fitstats once (Cosy version) and then overwrite labels when needed
register_fitstats_cosy <- function() {
  fitstat_register("pre_avg", function(x) {
    formula <- x$fml_all$linear
    outcome_variable <- all.vars(formula)[1]

    call_object <- x$call
    data_expr <- call_object$data
    data <- eval(data_expr)

    obs_used <- obs(x)
    data_used <- data[obs_used, , drop = FALSE]

    outcome_values <- data_used[[outcome_variable]]
    pre_avg <- mean(outcome_values[data_used$cosy_contract_active == 0], na.rm = TRUE)
    format_decimal(pre_avg, digits = 4)
  }, "Half Hourly Consumption")

  fitstat_register("t_obs", function(x) {
    t_var <- x$fixef_vars[3]
    t_obs <- x$fixef_sizes[t_var]
    format_number(t_obs)
  }, "Number of Time Periods")
}

register_fitstats_hp <- function() {
  fitstat_register("pre_avg", function(x) {
    model_data <- eval(x$call$data, envir = x$call_env)
    outcome_variable <- all.vars(x$fml_all$linear)[1]
    obs_used <- obs(x)

    if (is.logical(obs_used) && length(obs_used) == nrow(model_data)) {
      data_used <- model_data[obs_used, , drop = FALSE]
    } else if (is.numeric(obs_used) && all(obs_used <= nrow(model_data))) {
      data_used <- model_data[obs_used, , drop = FALSE]
    } else {
      stop("Unable to correctly subset data. Check the obs_used vector.")
    }

    outcome_values <- data_used[[outcome_variable]]
    pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0], na.rm = TRUE)
    format_decimal(pre_avg, digits = 2)
  }, "Yearly Consumption")

  fitstat_register("t_obs", function(x) {
    t_var <- x$fixef_vars[3]
    t_obs <- x$fixef_sizes[t_var]
    format_number(t_obs)
  }, "Number of Time Periods")
}

# Small utility for mean/sd balance tables used later
summarise_numeric <- function(data) {
  data %>%
    ungroup() %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(),
                     list(mean = ~mean(., na.rm = TRUE),
                          sd   = ~sd(., na.rm = TRUE)),
                     .names = "{col}_{.fn}"))
}

count_observations <- function(data) {
  data %>%
    ungroup() %>%
    summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>%
    summarise(N = min(across(everything())))
}

format_number_2dp <- function(x) {
  formatC(x, format = "f", big.mark = ",", digits = 2)
}


CleanBalanceSections <- function(file_path) {
  x <- readLines(file_path)

  # Replace NA cells on the section header rows with blanks
  # Stargazer prints "NA" for those cells; we blank them out on those lines.
  x <- gsub("(\\\\textbf\\{Pre-matching\\}).*",
            "\\\\[-0.8ex]\\\\textbf{Pre-matching} &  &  &  \\\\",
            x)
  x <- gsub("(\\\\textbf\\{Post-matching\\}).*",
            "\\\\[-0.8ex]\\\\textbf{Post-matching} &  &  &  \\\\",
            x)

  # Add a little spacing + a midrule before Post-matching (optional)
  post_idx <- grep("\\\\textbf\\{Post-matching\\}", x)
  if (length(post_idx) == 1) {
    x <- append(x, "\\\\midrule", after = post_idx - 1)
  }

  writeLines(x, file_path)
}


# ----------------------------
# 2) Load core data used early
# ----------------------------
print("Load data")

aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS"))

# ----------------------------
# 3) Build samples (Cosy, HP, Random)
# ----------------------------

# cosy sample
cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_07_24.csv")) %>%
  inner_join(aggregated_data %>% select(hashed_mpan, account_id) %>% distinct()) %>%
  distinct() %>%
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency)) %>%
  select(account_id, energy_efficiency, property_value, floor_area) %>%
  mutate(sample = "Heat Pump Tariff")

# heat pump sample
hp_details <- fread(file.path(datapath, "input/cosy_-_hp_aggregated_up_2024_06_18.csv")) %>%
  distinct(account_id) %>%
  inner_join(
    fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>%
      distinct(account_id, .keep_all = TRUE),
    by = c("account_id")
  ) %>%
  filter(installed_at <= "2024-05-29") %>%
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency)) %>%
  select(account_id, energy_efficiency, property_value, floor_area) %>%
  mutate(sample = "HP")

hp_installed <- readRDS(file.path(datapath, "output/hp_installed.rds"))
start_date <- min(hp_installed$settlement_week)

overall_weekly <- readRDS(file.path(datapath, "output/overall_weekly.rds")) %>%
  ungroup() %>%
  mutate(
    week      = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at,     start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  filter(week <= 129, firstweek <= 129) %>%
  filter(week < firstweek - 4 | week >= firstweek)

# random sample
random_domus_sample <- fread(file.path(datapath, "input/cosy_-_random_sample_details_2024_08_23.csv")) %>%
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency)) %>%
  filter(!account_id %in% cosy_hp_details$account_id, !account_id %in% hp_details$account_id) %>%
  select(account_id, energy_efficiency, property_value, floor_area) %>%
  mutate(sample = "Random Sample")

# latest eac
latest_eac <- fread(file.path(datapath, "input/latest_eac.csv")) %>%
  distinct(account_id, .keep_all = TRUE)

# ----------------------------
# 4) Matching: Cosy vs Random
# ----------------------------
print("Matching Cosy")

matching_data <- rbind(
  latest_eac %>% inner_join(random_domus_sample),
  latest_eac %>% inner_join(cosy_hp_details),
  latest_eac %>% inner_join(hp_details)
) %>%
  filter(!is.na(sample), !is.na(estimated_annual_consumption)) %>%
  distinct(account_id, .keep_all = TRUE)

match_obj <- matchit(
  formula = treated ~ property_value + energy_efficiency + floor_area,
  data = matching_data %>%
    filter(sample %in% c("Heat Pump Tariff", "Random Sample")) %>%
    mutate(treated = as.numeric(!sample == "Random Sample")),
  method = "full",
  estimand = "ATC",
  caliper = c(energy_efficiency = .5, property_value = 0.5, floor_area = 0.5)
)

match_summary <- summary(match_obj)
match_summary <- clean_matchit_summary(match_summary, treated_label = "Heat Pump Tariff", control_label = "Random Sample")

# plotting balance
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

# Extract pre and post tables
pre  <- as.data.frame(match_summary$sum.all)
post <- as.data.frame(match_summary$sum.matched)

# Keep only what you want + enforce consistent column names
keep_cols <- c("Means Heat Pump Tariff", "Means Random Sample", "Std. Mean Diff.")

pre_tbl <- pre[, keep_cols, drop = FALSE]
post_tbl <- post[, keep_cols, drop = FALSE]

colnames(pre_tbl)  <- c("Mean (Treated)", "Mean (Control)", "Std. Mean Diff.")
colnames(post_tbl) <- c("Mean (Treated)", "Mean (Control)", "Std. Mean Diff.")

# Add variable names as a column
pre_tbl  <- cbind(Variable = rownames(pre_tbl),  pre_tbl)
post_tbl <- cbind(Variable = rownames(post_tbl), post_tbl)

rownames(pre_tbl)  <- NULL
rownames(post_tbl) <- NULL

# Insert a section header row between the two blocks
section_row <- data.frame(
  Variable = "\\\\[-0.8ex]\\textbf{Post-matching}",
  `Mean (Treated)` = NA,
  `Mean (Control)` = NA,
  `Std. Mean Diff.` = NA,
  check.names = FALSE
)

# Also add a header for the first block (optional, but matches your request)
section_row_pre <- data.frame(
  Variable = "\\\\[-0.8ex]\\textbf{Pre-matching}",
  `Mean (Treated)` = NA,
  `Mean (Control)` = NA,
  `Std. Mean Diff.` = NA,
  check.names = FALSE
)

balance_long <- rbind(section_row_pre, pre_tbl, section_row, post_tbl)

# -----------------------------
# Export with stargazer
# -----------------------------
stargazer(
  balance_long,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  digits = 2,
  title = "Covariate Balance Before and After Matching (Tariff)",
  label = "tab:balance-cosy-matching",
  out = "tables/balance_cosy_matching.tex"
)

CleanBalanceSections("tables/balance_cosy_matching.tex")

# NN table
stargazer(
  match_summary$nn,
  title = "Sample Size (Heat Pump Tariff)",
  rownames = TRUE,
  label = "tab:balance-cosy-nn",
  out = "tables/balance_cosy_nn.tex"
)

#Extract matched data
matched_data <- match.data(match_obj)

# ----------------------------
# 5) TWFE: Cosy sample (unweighted vs matched-weighted)
# ----------------------------
m1 <- feols(
  consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date,
  data = aggregated_data,
  cluster = ~account_id,
  split = ~rate_period
)

m1_matched <- feols(
  consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date,
  weights = ~weights,
  data = aggregated_data %>%
    inner_join(matched_data %>% distinct(account_id, weights), by = "account_id"),
  cluster = ~account_id,
  split = ~rate_period
)

register_fitstats_cosy()

etable(
  m1_matched, tex = TRUE, title = "TWFE using Matching Weights",
  headers = list(
    list("Matching" = 5),
    list(rep(as.character(sort(unique(aggregated_data$rate_period))), times = 1))
  ),
  fitstat = ~ N + g + pre_avg + t_obs + r2,
  file = "tables/matching_did.tex", replace = TRUE, label = "tab:did-matching"
)

CleanPreAverage("tables/matching_did.tex", pre_avg_label = "Half Hourly Consumption", coeff_anchor_pattern = "Fixed-effects")

# ----------------------------
# 6) Matching: HP vs Random
# ----------------------------

print("Matching HP")


set.seed(12345678)

matching_data <- rbind(
  latest_eac %>% inner_join(random_domus_sample),
  latest_eac %>% inner_join(hp_details)
) %>%
  filter(!is.na(sample), !is.na(estimated_annual_consumption)) %>%
  distinct(account_id, .keep_all = TRUE)

match_obj2 <- matchit(
  treated ~ property_value + estimated_annual_consumption + energy_efficiency + floor_area,
  data = matching_data %>%
    filter(sample %in% c("HP", "Random Sample")) %>%
    mutate(treated = as.numeric(!sample == "Random Sample")),
  method = "full",
  estimand = "ATC",
  caliper = c(
    estimated_annual_consumption = 0.5,
    energy_efficiency = .5,
    property_value = 0.5,
    floor_area = 0.5
  )
)

match_summary <- summary(match_obj2)
match_summary <- clean_matchit_summary(match_summary, treated_label = "Heat Pump", control_label = "Random Sample")

plot(match_obj2, type = "jitter", interactive = FALSE)
plot(summary(match_obj2), abs = FALSE)


# Extract pre and post tables
pre  <- as.data.frame(match_summary$sum.all)
post <- as.data.frame(match_summary$sum.matched)

# Keep only what you want + enforce consistent column names
keep_cols <- c("Means Heat Pump", "Means Random Sample", "Std. Mean Diff.")

pre_tbl <- pre[, keep_cols, drop = FALSE]
post_tbl <- post[, keep_cols, drop = FALSE]

colnames(pre_tbl)  <- c("Mean (Treated)", "Mean (Control)", "Std. Mean Diff.")
colnames(post_tbl) <- c("Mean (Treated)", "Mean (Control)", "Std. Mean Diff.")

# Add variable names as a column
pre_tbl  <- cbind(Variable = rownames(pre_tbl),  pre_tbl)
post_tbl <- cbind(Variable = rownames(post_tbl), post_tbl)

rownames(pre_tbl)  <- NULL
rownames(post_tbl) <- NULL

# Insert a section header row between the two blocks
section_row <- data.frame(
  Variable = "\\\\[-0.8ex]\\textbf{Post-matching}",
  `Mean (Treated)` = NA,
  `Mean (Control)` = NA,
  `Std. Mean Diff.` = NA,
  check.names = FALSE
)

# Also add a header for the first block (optional, but matches your request)
section_row_pre <- data.frame(
  Variable = "\\\\[-0.8ex]\\textbf{Pre-matching}",
  `Mean (Treated)` = NA,
  `Mean (Control)` = NA,
  `Std. Mean Diff.` = NA,
  check.names = FALSE
)

balance_long <- rbind(section_row_pre, pre_tbl, section_row, post_tbl)

# -----------------------------
# Export with stargazer
# -----------------------------
stargazer(
  balance_long,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  digits = 2,
  title = "Covariate Balance Before and After Matching (Heat Pump)",
  label = "tab:balance-hp-matching",
  out = "tables/balance_hp_matching.tex"
)

CleanBalanceSections("tables/balance_hp_matching.tex")

# NN table  [keeps your original output path (typo preserved): tables/balance_hpy_nn.tex]
stargazer(
  match_summary$nn,
  title = "Sample Size (Heatpump)",
  rownames = TRUE,
  label = "tab:balance-hp-nn",
  out = "tables/balance_hpy_nn.tex"
)

matched_data2 <- match.data(match_obj2)

# ----------------------------
# 7) TWFE: HP sample with matching weights
# ----------------------------
register_fitstats_hp()

ids_elec <- readRDS(file.path(datapath, "scratch/ids_cs_elec.RS"))
ids_gas  <- readRDS(file.path(datapath, "scratch/ids_cs_gas.RS"))

m1 <- feols(
  elec_consumption ~ i(is_hp_installed) | hdd + account_id + settlement_week,
  data = overall_weekly %>%
    filter(account_id %in% ids_elec) %>%
    inner_join(matched_data2 %>% distinct(account_id, weights), by = "account_id"),
  cluster = ~account_id
)

m2 <- feols(
  gas_consumption ~ i(is_hp_installed) | hdd + account_id + settlement_week,
  data = overall_weekly %>%
    ungroup() %>%
    filter(account_id %in% ids_gas) %>%
    inner_join(matched_data2 %>% distinct(account_id, weights), by = "account_id"),
  cluster = ~account_id
)

etable(
  m1, m2, tex = TRUE, title = "TWFE using Matching Weights (Heatpump)",
  headers = c("Electricity", "Gas"),
  fitstat = ~ N + g + pre_avg + t_obs + r2,
  file = "tables/matching_hp.tex", replace = TRUE, label = "tab:hp-matching",
  depvar = FALSE
)

# Use a dedicated cleaner for this file consistent with your old intent
CleanPreAverage("tables/matching_hp.tex", pre_avg_label = "Yearly Consumption", coeff_anchor_pattern = "Is HP Installed")

# ----------------------------
# 8) Balance table: Cosy vs HP vs Random (mean (sd))
# ----------------------------

print("Balance with random")

aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS"))

cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_07_24.csv")) %>%
  inner_join(aggregated_data %>% select(hashed_mpan) %>% distinct()) %>%
  distinct() %>%
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency), !is.na(estimated_annual_consumption))

rm(aggregated_data)

hp_details <- fread(file.path(datapath, "input/cosy_-_hp_aggregated_up_2024_06_18.csv")) %>%
  mutate(date = as.Date(settlement_date)) %>%
  inner_join(
    fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>%
      distinct(account_id, .keep_all = TRUE),
    by = c("account_id", "hashed_mpan")
  ) %>%
  mutate(
    date = as.Date(date),
    is_hp_installed = as.numeric(installed_at <= date)
  ) %>%
  group_by(account_id) %>%
  mutate(treated = max(is_hp_installed)) %>%
  distinct(account_id, treated, property_value, floor_area, energy_efficiency, estimated_annual_consumption) %>%
  filter(treated == 1, !is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency), !is.na(estimated_annual_consumption))

random_domus_sample <- fread(file.path(datapath, "input/cosy_-_random_sample_details_2024_08_23.csv")) %>%
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency), !is.na(estimated_annual_consumption))

# Common vars
common_vars <- c("energy_efficiency", "floor_area", "property_value", "estimated_annual_consumption")
numeric_vars <- cosy_hp_details %>%
  select(all_of(common_vars)) %>%
  select(where(is.numeric)) %>%
  names()

# Ns
n_cosy   <- count_observations(cosy_hp_details %>% select(all_of(numeric_vars)))$N
n_hp     <- count_observations(hp_details %>% select(all_of(numeric_vars)))$N
n_random <- count_observations(random_domus_sample %>% select(all_of(numeric_vars)))$N

# Summaries
cosy_numeric   <- cosy_hp_details %>% select(all_of(numeric_vars)) %>% summarise_numeric()
hp_numeric     <- hp_details %>% select(all_of(numeric_vars)) %>% summarise_numeric()
random_numeric <- random_domus_sample %>% select(all_of(numeric_vars)) %>% summarise_numeric()

cosy_numeric_long <- cosy_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Heat Pump Tariff")

hp_numeric_long <- hp_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Heat Pump Adopters")

random_numeric_long <- random_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Random Sample")

numeric_summary <- rbind(cosy_numeric_long, hp_numeric_long, random_numeric_long)

final_table <- numeric_summary %>%
  pivot_wider(names_from = Sample, values_from = c(mean, sd)) %>%
  arrange(Variable)

final_table_with_sd <- final_table %>%
  pivot_longer(
    cols = matches("^(mean|sd)_"),
    names_to = c("Statistic", "Sample"),
    names_pattern = "^(mean|sd)_(.*)$"
  ) %>%
  pivot_wider(names_from = Sample, values_from = value) %>%
  arrange(Variable, Statistic) %>%
  mutate(
    Variable = ifelse(Statistic == "mean", Variable, ""),
    `Heat Pump Tariff` = ifelse(Statistic == "mean",
                               format_number_2dp(`Heat Pump Tariff`),
                               paste0("(", format_number_2dp(`Heat Pump Tariff`), ")")),
    `Heat Pump Adopters` = ifelse(Statistic == "mean",
                                 format_number_2dp(`Heat Pump Adopters`),
                                 paste0("(", format_number_2dp(`Heat Pump Adopters`), ")")),
    `Random Sample` = ifelse(Statistic == "mean",
                             format_number_2dp(`Random Sample`),
                             paste0("(", format_number_2dp(`Random Sample`), ")")),
    Variable = case_when(
      Variable == "energy_efficiency" ~ "Energy Efficiency",
      Variable == "estimated_annual_consumption" ~ "EAC",
      Variable == "floor_area" ~ "Floor Area",
      Variable == "property_value" ~ "Property Value",
      TRUE ~ Variable
    )
  ) %>%
  select(-Statistic) %>%
  rename(
    !!paste0("Heat Pump Tariff (N = ", n_cosy, ")") := `Heat Pump Tariff`,
    !!paste0("Heat Pump Adopters (N = ", n_hp, ")") := `Heat Pump Adopters`,
    !!paste0("Random Sample (N = ", n_random, ")") := `Random Sample`
  )

stargazer(
  final_table_with_sd, type = "latex", summary = FALSE,
  title = "External Validity by Area for Tariff and Heat Pump Adopters",
  rownames = FALSE,
  digits = 2,
  label = "tab:cosy-hp-random",
  out = "tables/balance_table_cosy_hp_random.tex"
)

# ----------------------------
# 9) Survey responders vs non-responders (Cosy)
# ----------------------------

print("Balance survey")

aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS"))

survey_selection <- fread(file.path(datapath, "input/cosy_survey_ids.csv"))
n_frame <- nrow(survey_selection)

# Respondent list: deduplicated on the survey's own customer key (kid), which
# is the same identifier as account_number in cosy_survey_ids.csv. Cached by
# 01_11_cosy_survey_figures.R from the raw questionnaire export -- see that
# script for the dedup logic (3 households submitted the survey twice; the
# later submission is kept). Replaces the older, stale survey_ids.csv (384
# rows), which did not match the raw export's 390 deduplicated respondents.
respondent_account_numbers <- readRDS(file.path(datapath, "scratch/cosy_survey_respondent_kids.RDS"))
n_resp_raw <- length(respondent_account_numbers)
unmatched <- setdiff(respondent_account_numbers, survey_selection$account_number)
if (length(unmatched) > 0) {
  cat(sprintf("NOTE: %d of %d survey respondents not found in cosy_survey_ids.csv\n",
              length(unmatched), n_resp_raw))
}
responders <- survey_selection %>%
  filter(account_number %in% respondent_account_numbers) %>%
  distinct(account_id, account_number)
n_resp_matched <- nrow(responders)

non_responders_frame <- survey_selection %>% filter(!account_id %in% responders$account_id)
n_nonresp_frame <- nrow(non_responders_frame)
stopifnot(n_resp_matched + n_nonresp_frame == n_frame)

cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_07_24.csv")) %>%
  inner_join(aggregated_data %>% distinct(hashed_mpan, account_id))

cosy_survey <- cosy_hp_details %>%
  filter(account_id %in% responders$account_id) %>%
  select(-account_id) %>%
  distinct() %>%
  ungroup()
n_resp_linked <- nrow(cosy_survey)
cosy_survey <- cosy_survey %>%
  select(floor_area, estimated_annual_consumption, energy_efficiency, property_value)

cosy_non_survey <- non_responders_frame %>%
  inner_join(cosy_hp_details, by = "account_id") %>%
  ungroup()
n_nonresp_linked <- nrow(cosy_non_survey)
cosy_non_survey <- cosy_non_survey %>%
  select(floor_area, estimated_annual_consumption, energy_efficiency, property_value)

rm(aggregated_data, cosy_hp_details, responders, survey_selection, non_responders_frame)

n_survey   <- count_observations(cosy_survey)$N
n_nosurvey <- count_observations(cosy_non_survey)$N

# --- Reconciling macros for the survey attrition footnote (subsec:survey) --
# Traces exactly how the 390 respondents / non-respondent frame narrow down
# to the N actually shown in tab:cosy-survey-stats, so the paper text can
# cite the intermediate stages instead of just the final N.
survey_balance_tex <- c(
  sprintf("%% Auto-generated %s by 03_00_balance_tables_and_reweighting.R -- do not edit by hand.",
          format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("\\newcommand{\\SurveyBalanceRespondents}{%d}", n_resp_raw),
  sprintf("\\newcommand{\\SurveyBalanceRespondentsMatched}{%d}", n_resp_matched),
  sprintf("\\newcommand{\\SurveyBalanceRespondentsLinked}{%d}", n_resp_linked),
  sprintf("\\newcommand{\\SurveyBalanceN}{%d}", n_survey),
  sprintf("\\newcommand{\\SurveyBalanceFrame}{%d}", n_frame),
  sprintf("\\newcommand{\\SurveyBalanceNonRespondentsFrame}{%d}", n_nonresp_frame),
  sprintf("\\newcommand{\\SurveyBalanceNonRespondentsLinked}{%d}", n_nonresp_linked),
  sprintf("\\newcommand{\\SurveyBalanceNoSurveyN}{%d}", n_nosurvey)
)
writeLines(survey_balance_tex, "tables/survey_balance_numbers.tex")
cat(sprintf("Wrote tables/survey_balance_numbers.tex (%d definitions)\n",
            sum(grepl("^\\\\newcommand", survey_balance_tex))))

cosy_survey_numeric <- summarise_numeric(cosy_survey)
cosy_non_survey_numeric <- summarise_numeric(cosy_non_survey)

cosy_survey_numeric_long <- cosy_survey_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Survey")

cosy_non_survey_numeric_long <- cosy_non_survey_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "NoSurvey")

numeric_summary <- rbind(cosy_survey_numeric_long, cosy_non_survey_numeric_long)

final_table <- numeric_summary %>%
  pivot_wider(names_from = Sample, values_from = c(mean, sd)) %>%
  arrange(Variable)

final_table_with_sd <- final_table %>%
  pivot_longer(cols = c(mean_Survey, mean_NoSurvey, sd_Survey, sd_NoSurvey),
               names_to = c("Statistic", "Sample"),
               names_sep = "_") %>%
  pivot_wider(names_from = Sample, values_from = value) %>%
  arrange(Variable, Statistic) %>%
  mutate(
    Variable = ifelse(Statistic == "mean", Variable, ""),
    Survey = ifelse(Statistic == "mean", format_number_2dp(Survey), paste0("(", format_number_2dp(Survey), ")")),
    NoSurvey = ifelse(Statistic == "mean", format_number_2dp(NoSurvey), paste0("(", format_number_2dp(NoSurvey), ")"))
  ) %>%
  select(Variable, Survey, NoSurvey) %>%
  mutate(
    Variable = case_when(
      Variable == "energy_efficiency" ~ "Energy Efficiency",
      Variable == "estimated_annual_consumption" ~ "EAC",
      Variable == "floor_area" ~ "Floor Area",
      Variable == "property_value" ~ "Property Value",
      TRUE ~ Variable
    )
  ) %>%
  rename(
    !!paste0("Survey (N = ", n_survey, ")") := Survey,
    !!paste0("No Survey (N = ", n_nosurvey, ")") := NoSurvey
  )

stargazer(
  final_table_with_sd, type = "latex", summary = FALSE,
  title = "Balance Table for Heat Pump Tariff Survey Responders and Non-Responders",
  rownames = FALSE,
  digits = 2,
  label = "tab:cosy-survey-stats",
  out = "tables/balance_table_cosy_survey.tex"
)

# ----------------------------
# 10) Cosy early vs late adopters
# ----------------------------

print("Late vs Early")

aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS"))

cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_07_24.csv")) %>%
  inner_join(aggregated_data %>% distinct(hashed_mpan, account_id, first_adoption)) %>%
  distinct(account_id, .keep_all = TRUE) %>%
  ungroup() %>%
  select(floor_area, estimated_annual_consumption, energy_efficiency, property_value, first_adoption) %>%
  mutate(early = as.numeric(first_adoption <= "2023-11-19"))

early_adopters <- cosy_hp_details %>% filter(early == 1) %>% select(-early)
late_adopters  <- cosy_hp_details %>% filter(early == 0) %>% select(-early)

n_early <- count_observations(early_adopters)$N
n_late  <- count_observations(late_adopters)$N

early_adopters_numeric <- summarise_numeric(early_adopters)
late_adopters_numeric  <- summarise_numeric(late_adopters)

early_adopters_numeric_long <- early_adopters_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Early")

late_adopters_numeric_long <- late_adopters_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Late")

numeric_summary <- rbind(early_adopters_numeric_long, late_adopters_numeric_long)

final_table <- numeric_summary %>%
  pivot_wider(names_from = Sample, values_from = c(mean, sd)) %>%
  arrange(Variable)

final_table_with_sd <- final_table %>%
  pivot_longer(cols = c(mean_Early, mean_Late, sd_Early, sd_Late),
               names_to = c("Statistic", "Sample"),
               names_sep = "_") %>%
  pivot_wider(names_from = Sample, values_from = value) %>%
  arrange(Variable, Statistic) %>%
  mutate(
    Variable = ifelse(Statistic == "mean", Variable, ""),
    Early = ifelse(Statistic == "mean", format_number_2dp(Early), paste0("(", format_number_2dp(Early), ")")),
    Late  = ifelse(Statistic == "mean", format_number_2dp(Late),  paste0("(", format_number_2dp(Late),  ")"))
  ) %>%
  select(Variable, Early, Late) %>%
  mutate(
    Variable = case_when(
      Variable == "energy_efficiency" ~ "Energy Efficiency",
      Variable == "estimated_annual_consumption" ~ "EAC",
      Variable == "floor_area" ~ "Floor Area",
      Variable == "property_value" ~ "Property Value",
      TRUE ~ Variable
    )
  ) %>%
  rename(
    !!paste0("Early (N = ", n_early, ")") := Early,
    !!paste0("Late (N = ", n_late, ")") := Late
  )

stargazer(
  final_table_with_sd, type = "latex", summary = FALSE,
  title = "Balance Table for Heat Pump Tariff Early and Late Adopters",
  rownames = FALSE,
  digits = 2,
  label = "tab:cosy-late-early",
  out = "tables/balance_table_cosy_adoption.tex"
)

# ----------------------------
# 11) HP early vs late adopters
# ----------------------------
hp_details <- fread(file.path(datapath, "input/cosy_-_hp_aggregated_up_2024_06_18.csv")) %>%
  mutate(date = as.Date(settlement_date)) %>%
  inner_join(
    fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>%
      distinct(account_id, .keep_all = TRUE),
    by = c("account_id", "hashed_mpan")
  ) %>%
  mutate(
    date = as.Date(date),
    is_hp_installed = as.numeric(installed_at <= date)
  ) %>%
  group_by(account_id) %>%
  mutate(treated = max(is_hp_installed)) %>%
  distinct(account_id, treated, property_value, floor_area, energy_efficiency, estimated_annual_consumption, installed_at) %>%
  ungroup() %>%
  filter(treated == 1, !is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency), !is.na(estimated_annual_consumption))

summary(hp_details$installed_at)

early_adopters <- hp_details %>%
  filter(installed_at < "2024-02-12") %>%
  select(-installed_at, -account_id, -treated)

late_adopters <- hp_details %>%
  filter(installed_at >= "2024-02-12") %>%
  select(-installed_at, -account_id, -treated)

n_early <- count_observations(early_adopters)$N
n_late  <- count_observations(late_adopters)$N

early_adopters_numeric <- summarise_numeric(early_adopters)
late_adopters_numeric  <- summarise_numeric(late_adopters)

early_adopters_numeric_long <- early_adopters_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Early")

late_adopters_numeric_long <- late_adopters_numeric %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_pattern = "(.*)_(.*)") %>%
  mutate(Sample = "Late")

numeric_summary <- rbind(early_adopters_numeric_long, late_adopters_numeric_long)

final_table <- numeric_summary %>%
  pivot_wider(names_from = Sample, values_from = c(mean, sd)) %>%
  arrange(Variable)

final_table_with_sd <- final_table %>%
  pivot_longer(cols = c(mean_Early, mean_Late, sd_Early, sd_Late),
               names_to = c("Statistic", "Sample"),
               names_sep = "_") %>%
  pivot_wider(names_from = Sample, values_from = value) %>%
  arrange(Variable, Statistic) %>%
  mutate(
    Variable = ifelse(Statistic == "mean", Variable, ""),
    Early = ifelse(Statistic == "mean", format_number_2dp(Early), paste0("(", format_number_2dp(Early), ")")),
    Late  = ifelse(Statistic == "mean", format_number_2dp(Late),  paste0("(", format_number_2dp(Late),  ")"))
  ) %>%
  select(Variable, Early, Late) %>%
  mutate(
    Variable = case_when(
      Variable == "energy_efficiency" ~ "Energy Efficiency",
      Variable == "estimated_annual_consumption" ~ "EAC",
      Variable == "floor_area" ~ "Floor Area",
      Variable == "property_value" ~ "Property Value",
      TRUE ~ Variable
    )
  ) %>%
  rename(
    !!paste0("Early (N = ", n_early, ")") := Early,
    !!paste0("Late (N = ", n_late, ")") := Late
  )

# Keep your original label (even though it duplicates cosy adoption label)
stargazer(
  final_table_with_sd, type = "latex", summary = FALSE,
  title = "Balance Table for Heat Pump Early and Late Adopters",
  rownames = FALSE,
  digits = 2,
  label = "tab:hp-late-early",
  out = "tables/balance_table_hp_adoption.tex"
)