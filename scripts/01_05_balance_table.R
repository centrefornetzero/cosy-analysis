## External Validity Tables 

# start date
start_date <- aggregated_data %>% ungroup() %>% summarise(date=min(first_adoption, na.rm = TRUE))
start_date <- start_date$date

# Found the previous contract before adopting cosy
Prev_contract <- fread(file.path(datapath, "input/Cosy_-_agreement_data_2024_07_24.csv")) %>%
  arrange(hashed_mpan, as.Date(agreement_valid_from)) %>%
  group_by(hashed_mpan) %>%
  mutate(
    previous_contract = lag(product_display_name),
    previous_is_variable = lag(is_variable),
    previous_is_charged_half_hourly = lag(is_charged_half_hourly),
    is_cosy = product_display_name == "Cosy Octopus"
  ) %>%
  filter(is_cosy) %>%
  slice_head(n=1)

# Create a hashed_mpan dataset
first_adoption <- aggregated_data %>%
  left_join(Prev_contract) %>%
  ungroup() %>%
  select(hashed_mpan, tariff_gsp_group_id, first_adoption, urbanity, previous_is_charged_half_hourly,
         floor_area, eac_mwh, property_value, energy_efficiency, estimated_annual_consumption) %>%
  distinct() %>%
  mutate(adoption_week =  round(as.numeric(difftime(floor_date(first_adoption, "week"), start_date, units = "weeks"))),
         urban = case_when(
           urbanity %in% c('Large Urban Areas', 'Smaller Urban Areas') ~ 1,
           urbanity %in% c('Accessible Settlements', 'Sparse/Remote Villages/Dwellings', 'Accessible Villages/Dwellings', 'Sparse/Remote Settlements') ~ 0,
           TRUE ~ NA
         ))

# early adoptors table
m_adopters <- feols(adoption_week ~ i(urban, ref=0) + log(floor_area) + log(property_value) + log(energy_efficiency) + log(estimated_annual_consumption) + previous_is_charged_half_hourly, data = first_adoption, se = "hetero")
etable(m_adopters)

# Extract coefficients and standard errors
coefs <- coeftable(m_adopters) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(term == "i(factor_var = urban, ref = 0)" ~ "Urban",
                          term == "log(floor_area)" ~ "Log Floor Area",
                          term == "log(property_value)" ~ "Log Property Value",
                          term == "log(energy_efficiency)" ~ "Log Energy Efficiency",
                          term == "log(estimated_annual_consumption)" ~ "Log Estimated Consumption",
                          term == "previous_is_touTRUE" ~ "Previous Contract Is ToU",
                          TRUE ~ term),
         lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`
  ) %>%
  arrange(Estimate)


# Add a note below the graph
note <- "Note: The dependent variable is adoption week (0 for the first week adopters up to 65 for the later). \n Early adopters are more urban, have higher electricity consumption and more energy efficient homes. \n Data: Domus dataset and OE energy."

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

rm(m_adopters)

# Read and process each dataset
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates

population2022 <- read_excel(file.path(datapath, "input/sapemsoasyoatablefinal.xlsx"), sheet = "Mid-2022 MSOA 2021", skip = 3)

cosy_hp_details <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_07_24.csv")) %>%
  inner_join(aggregated_data %>% select(hashed_mpan) %>% distinct()) %>%
  select(hashed_mpan, postcode) %>%
  distinct() %>%
  group_by(postcode) %>%
  tally() %>%
  filter(!postcode == "")
# https://www.data.gov.uk/dataset/c2235117-cbfd-480d-8fc7-b564bd0f4d58/output-area-2021-to-lsoas-to-msoas-to-lep-to-lad-dec-2022-best-fit-lookup-in-en-v2
postcode_msoa <- fread(file.path(datapath, "input/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")) %>%
  left_join(cosy_hp_details, by = c("pcds" = "postcode")) %>%
  mutate(n = ifelse(is.na(n), 0, 1)) %>%
  select(msoa21cd, n) %>%
  group_by(msoa21cd) %>%
  summarise(treated = sum(n))
# https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/smallareamodelbasedincomeestimates/financialyearending2020
income <- readxl::read_excel(file.path(datapath,"input/saiefy1920finalqaddownload280923.xlsx"), sheet = "Total annual income", skip = 4) %>%
  select(`MSOA code`, `Total annual income (£)`) %>%
  distinct() 

# Load and preprocess the property_prices data (2011-vintage MSOA; kept as "MSOA code" here)
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/hpssadataset3meanhousepricebymsoaquarterlyrollingyear
property_prices <- read_excel(file.path(datapath, "input/HPSSA Dataset 3 - Mean price paid by MSOA.xls"),
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

# customs dataset from https://www.ons.gov.uk/datasets/create
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
  mutate(sum_obs = sum(Observation), `Share Level 4 Qualifications (%)` = 100 * Observation / sum_obs) %>%
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
colnames(formatted_results)[2] <- paste0("MSOAs with Tariff Adopters (N = ", results$`N 1`[1], ")")
colnames(formatted_results)[3] <- paste0("Other MSOAs (N = ", results$`N 0`[1], ")")

# Create the LaTeX table using stargazer
stargazer(formatted_results, type = "latex", summary = FALSE, 
          title = "External Validity by Area for Tariff Adopters",
          rownames = FALSE,
          digits = 2,
          label = "tab:msoa-stats-cosy",
          out = "tables/balance_table_cosy.tex")

# Read the content of the generated LaTeX table
latex_table <- readLines("tables/balance_table_cosy.tex")

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
writeLines(latex_table, "tables/balance_table_cosy.tex")



# DELETE?
# Load and preprocess the property_prices data
# Merge all datasets
# NB: reuses income_property_2021 (postcode-level 2011->2021 crosswalk) computed above.
merged_data <- postcode_msoa %>%
  left_join(income_property_2021 %>% select(msoa21cd, `Property price (£)`), by = "msoa21cd") %>%
  mutate(country = substr(msoa21cd, 1, 1)) %>%
  filter(msoa21cd != "", country %in% c("E", "W"))

# Create combined data frame for plotting
treated_1 <- merged_data %>%
  filter(treated == 1) %>%
  select(property_value = `Property price (£)`) %>%
  mutate(group = "MSOAs with Adopters")

treated_0 <- merged_data %>%
  filter(treated == 0) %>%
  select(property_value = `Property price (£)`) %>%
  mutate(group = "MSOAs without Adopters")

property_value_data <- merged_data %>%
  select(property_value = `Property price (£)`) %>%
  filter(!is.na(property_value)) %>%
  mutate(group = "Adopters")

combined_data <- bind_rows(
  treated_1,
  treated_0,
  property_value_data
)

# Trim outliers by removing values outside the 1st and 99th percentiles
trimmed_combined_data <- combined_data %>%
  group_by(group) %>%
  filter(property_value > quantile(property_value, 0.01) & property_value < quantile(property_value, 0.99))

# Calculate means for each group
means <- trimmed_combined_data %>%
  group_by(group) %>%
  summarise(mean_value = mean(property_value, na.rm = TRUE))

ggplot(trimmed_combined_data, aes(x = property_value, color = group, fill = group)) +
  geom_density(alpha = 0.5, aes(y = ..scaled..)) +
  geom_vline(data = means, aes(xintercept = mean_value, color = group), linetype = "dashed") +
  scale_color_manual(values = c("skyblue", "lightgreen", "orange")) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange")) +
  scale_x_continuous(labels = dollar_format(prefix = "£", suffix = "k", scale = 1e-3, big.mark = ",")) +
  labs(
    title = "Density Plot of Property Prices",
    x = "Property Value",
    y = "Density",
    color = NULL,  # Remove the legend title for color
    fill = NULL    # Remove the legend title for fill
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


ggsave("graphs/average_property_price.png", width = 12, height = 8, dpi = 300)



