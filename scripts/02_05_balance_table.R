### Table A.15: External Validity by Area for Heat Pump Installation

# Function to calculate weighted standard deviation
# NB: filters (x, w) to jointly non-missing pairs first. Some MSOAs are missing
# income/property price (2011-vintage ONS releases) after the 2021 MSOA boundary
# review, so w can be non-missing while x is NA; without this filter, sum_w would
# include weight from those NA-x rows and bias mean_w/SD downward.
weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  sum_w <- sum(w)
  mean_w <- sum(w * x) / sum_w
  sqrt(sum(w * (x - mean_w)^2) / sum_w)
}

# Function to perform weighted t-test
# NB: same jointly-non-missing filtering as weighted_sd, for the same reason.
weighted_t_test <- function(x, w, y, v) {
  okx <- !is.na(x) & !is.na(w)
  oky <- !is.na(y) & !is.na(v)
  x <- x[okx]; w <- w[okx]
  y <- y[oky]; v <- v[oky]
  n_x <- sum(w)
  n_y <- sum(v)
  mean_x <- sum(w * x) / n_x
  mean_y <- sum(v * y) / n_y
  var_x <- sum(w * (x - mean_x)^2) / n_x
  var_y <- sum(v * (y - mean_y)^2) / n_y
  t_stat <- (mean_x - mean_y) / sqrt(var_x / n_x + var_y / n_y)
  df <- (var_x / n_x + var_y / n_y)^2 / ((var_x / n_x)^2 / (n_x - 1) + (var_y / n_y)^2 / (n_y - 1))
  p_value <- 2 * pt(-abs(t_stat), df)
  return(p_value)
}

# Function to summarize and test
summarize_and_test <- function(data, var_name, weight_name) {
  # Calculate weighted means, standard deviations, and non-missing observation counts for each treatment group
  summary_stats <- data %>%
    group_by(treated) %>%
    summarise(
      N = sum(!is.na(.data[[var_name]])), # Count non-missing observations
      Weighted_Mean = weighted.mean(.data[[var_name]], .data[[weight_name]], na.rm = TRUE),
      SD = weighted_sd(.data[[var_name]], .data[[weight_name]]),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = treated, values_from = c("N", "Weighted_Mean", "SD"), names_sep = " ")
  
  # Prepare for weighted t-test by separating data and weights
  data1 <- filter(data, treated == 1)[[var_name]]
  weights1 <- filter(data, treated == 1)[[weight_name]]
  data2 <- filter(data, treated == 0)[[var_name]]
  weights2 <- filter(data, treated == 0)[[weight_name]]
  
  # Check if both groups have sufficient data for weighted t-test
  if (length(unique(data1)) > 1 && length(unique(data2)) > 1 && length(data1) > 1 && length(data2) > 1) {
    p_value <- weighted_t_test(data1, weights1, data2, weights2)
  } else {
    p_value <- NA_real_ # Insufficient data or variability
  }
  
  # Combine results
  tibble(Variable = var_name) %>%
    bind_cols(summary_stats) %>%
    mutate(`P Value` = p_value)
}

# Read and process each dataset
population2022 <- read_excel(file.path(datapath, "input/sapemsoasyoatablefinal.xlsx"), sheet = "Mid-2022 MSOA 2021", skip = 3)

cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_hp_details_2024_07_03.csv")) %>%
  inner_join(hp_installed %>% filter(treated == 1) %>% select(account_id) %>% distinct()) %>%
  select(account_id, postcode) %>%
  distinct() %>%
  group_by(postcode) %>%
  tally() %>%
  filter(!postcode == "")

postcode_msoa <- fread(file.path(datapath, "input/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")) %>%
  left_join(cosy_hp_details, by = c("pcds" = "postcode")) %>%
  mutate(n = ifelse(is.na(n), 0, 1)) %>%
  select(msoa21cd, n) %>%
  group_by(msoa21cd) %>%
  summarise(treated = sum(n))

income <- readxl::read_excel(file.path(datapath, "input/saiefy1920finalqaddownload280923.xlsx"), sheet = "Total annual income", skip = 4) %>%
  select(`MSOA code`, `Total annual income (£)`) %>%
  distinct() 

net_income <- readxl::read_excel(file.path(datapath, "input/saiefy1920finalqaddownload280923.xlsx"), sheet = "Net annual income", skip = 4) %>%
  select(`MSOA code`, `Net annual income (£)`) %>%
  distinct() 

net_housing_income <- readxl::read_excel(file.path(datapath, "input/saiefy1920finalqaddownload280923.xlsx"), sheet = "Net income after housing costs", skip = 4) %>%
  select(`MSOA code`, `Net annual income after housing costs (£)`) %>%
  distinct()

# Load and preprocess the property_prices data (2011-vintage MSOA; kept as "MSOA code" here)
property_prices <- read_excel(file.path(datapath, "/input/HPSSA Dataset 3 - Mean price paid by MSOA.xls"),
                              sheet = "1a", skip = 4) %>%
  select(`MSOA code`, `Year ending Mar 2023`) %>%
  rename(`Property price (£)` = `Year ending Mar 2023`)

# income and property_prices are on 2011 MSOA boundaries (7,201 E&W areas), while
# postcode_msoa and the Census-derived tables below are on 2021 MSOA boundaries
# (7,264 E&W areas); matching msoa21cd directly against 2011-vintage MSOA codes
# would silently drop the 184 areas created/renumbered in the 2011->2021 boundary
# review. Postcodes have no such ambiguity (each belongs to exactly one 2011 MSOA
# and one 2021 MSOA), so we build a postcode-level crosswalk between the two
# vintages and attach income/price there, then average up to msoa21cd -- this
# correctly handles both 2011->2021 splits (all child postcodes share one
# 2011-vintage value, so the mean is just that value) and merges (child postcodes
# span >1 2011 MSOA, so we average across them).
msoa21_to_msoa11 <- fread(file.path(datapath, "input/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv"),
                          select = c("pcds", "msoa21cd")) %>%
  inner_join(
    fread(file.path(datapath, "input/PCD_OA_LSOA_MSOA_LAD_NOV21_UK_LU.csv"), select = c("pcds", "msoa11cd")),
    by = "pcds"
  )

income_property_2021 <- msoa21_to_msoa11 %>%
  left_join(income, by = c("msoa11cd" = "MSOA code")) %>%
  left_join(property_prices, by = c("msoa11cd" = "MSOA code")) %>%
  group_by(msoa21cd) %>%
  summarise(
    `Total annual income (£)` = mean(`Total annual income (£)`, na.rm = TRUE),
    `Property price (£)` = mean(`Property price (£)`, na.rm = TRUE)
  )

hh_size <- fread(file.path(datapath, "input/custom-filtered-2024-07-03T10_58_30Z.csv")) %>%
  group_by(`Middle layer Super Output Areas Code`) %>%
  mutate(sum_obs = sum(Observation), weight = Observation / sum_obs) %>%
  summarise(`Average HH Size` = sum(weight * `Household size (9 categories) Code`))

hh_deprivaton <- fread(file.path(datapath, "input/custom-filtered-2024-07-03T10_43_12Z.csv")) %>%
  group_by(`Middle layer Super Output Areas Code`) %>%
  mutate(sum_obs = sum(Observation), `HH Not Deprived in Any Dim. (%)` = 100 * Observation / sum_obs) %>%
  filter(`Household deprivation (6 categories) Code` == 1)

avg_age <- fread(file.path(datapath, "input/custom-filtered-2024-07-03T11_15_15Z.csv")) %>%
  group_by(`Middle layer Super Output Areas Code`) %>%
  mutate(sum_obs = sum(Observation), weight = Observation / sum_obs) %>%
  summarise(`Average Age` = sum(weight * `Age (101 categories) Code`))

education <- fread(file.path(datapath, "input/custom-filtered-2024-07-03T11_22_39Z.csv")) %>%
  group_by(`Middle layer Super Output Areas Code`) %>%
  mutate(sum_obs = sum(Observation), 
         `Share Level 4 Qualifications (%)` = 100 * Observation / sum_obs) %>%
  filter(`Highest level of qualification (7 categories) Code` == 4)

# Merge all datasets by `msoa21cd` or `Middle layer Super Output Areas Code`
merged_data <- postcode_msoa %>%
  left_join(income_property_2021, by = "msoa21cd") %>%
  inner_join(hh_size, by = c("msoa21cd" = "Middle layer Super Output Areas Code")) %>%
  inner_join(hh_deprivaton, by = c("msoa21cd" = "Middle layer Super Output Areas Code")) %>%
  inner_join(avg_age, by = c("msoa21cd" = "Middle layer Super Output Areas Code")) %>%
  inner_join(education, by = c("msoa21cd" = "Middle layer Super Output Areas Code")) %>%
  inner_join(population2022 %>% select(`MSOA 2021 Code`, Total), by = c("msoa21cd" = "MSOA 2021 Code")) %>%
  mutate(country = substr(msoa21cd, 1, 1),
         treated = as.numeric(treated > 0)) %>%
  filter(!msoa21cd == "", country %in% c("E", "W"))

# List of variables of interest
variables <- c(
  "Total annual income (£)",
  "Property price (£)",
  "Average HH Size",
  "HH Not Deprived in Any Dim. (%)",
  "Average Age",
  "Share Level 4 Qualifications (%)"
)

# Applying the function across all variables
results <- purrr::map_dfr(variables, ~summarize_and_test(merged_data, .x, "Total")) %>%
  as.data.frame()

# Format the counts with thousand separators
results <- results %>%
  mutate(`N 1` = format(`N 1`, big.mark = ",", scientific = FALSE),
         `N 0` = format(`N 0`, big.mark = ",", scientific = FALSE))

# Adjust the format of the table for LaTeX output
formatted_results <- results %>%
  select(Variable, `Weighted_Mean 1`, `SD 1`, `Weighted_Mean 0`, `SD 0`, `P Value`) %>%
  rename(
    `Weighted Mean Treated` = `Weighted_Mean 1`,
    `SD Treated` = `SD 1`,
    `Weighted Mean Others` = `Weighted_Mean 0`,
    `SD Others` = `SD 0`
  ) %>%
  mutate(
    `Weighted Mean Treated` = paste0(format(round(`Weighted Mean Treated`, 2), big.mark = ",", nsmall = 2), " (", format(round(`SD Treated`, 2), big.mark = ",", nsmall = 2), ")"),
    `Weighted Mean Others` = paste0(format(round(`Weighted Mean Others`, 2), big.mark = ",", nsmall = 2), " (", format(round(`SD Others`, 2), big.mark = ",", nsmall = 2), ")"),
    `P Value` = format(round(`P Value`, 2), nsmall = 2)
  ) %>%
  select(Variable, `Weighted Mean Treated`, `Weighted Mean Others`)

# Add the N values to the column names
colnames(formatted_results)[2] <- paste0("MSOAs with HP Installations (N = ", results$`N 1`[1], ")")
colnames(formatted_results)[3] <- paste0("Other MSOAs (N = ", results$`N 0`[1], ")")

# Create the LaTeX table using stargazer
stargazer(formatted_results, type = "latex", summary = FALSE, 
          title = "External Validity by Area for Heat Pump Installations",
          rownames = FALSE,
          digits = 2,
          label = "tab:msoa_stats",
          out = "tables/balance_table_temp.tex")

# Read the content of the generated LaTeX table
latex_table <- readLines("tables/balance_table_temp.tex")

# Insert custom headers with multicolumn
header_row <- " & \\multicolumn{2}{c}{Weighted Mean} \\\\"
position <- grep("\\\\begin\\{tabular\\}", latex_table) + 1
latex_table <- append(latex_table, header_row, after = position)

# Replace the first and last instances of \hline \\[-1.8ex] with \hline \hline \\[-1.8ex]
hline_ex_lines <- grep("\\hline" , latex_table)
if (length(hline_ex_lines) >= 2) {
  latex_table[hline_ex_lines[1]] <- gsub("\\hline", "\\hline\\hline", latex_table[hline_ex_lines[1]], fixed = TRUE)
  latex_table[hline_ex_lines[length(hline_ex_lines)]] <- gsub("\\hline", "\\hline\\hline", latex_table[hline_ex_lines[length(hline_ex_lines)]], fixed = TRUE)
}

# Write the modified LaTeX table to a new file
writeLines(latex_table, "tables/balance_table.tex")
file.remove("tables/balance_table_temp.tex")
