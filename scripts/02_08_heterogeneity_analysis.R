## Heterogeneity analysis


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Figure 6: Impact of Heat Pump Installation by Outside 
# Temperature (and Figure A.2 to A.5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load IDs
ids_cs_elec <-  readRDS(file.path(datapath, "scratch/ids_cs_elec.RS"))

# Update hp_installed with the new ev_charging values using case_when
hp_installed <- readRDS(file.path(datapath, "output/hp_installed.rds")) %>%
  filter(account_id %in% ids_cs_elec)  %>%
  filter(week <= 129, firstweek <= 129)  %>%
  filter(week <= firstweek - 5 | week > firstweek) %>% 
  ungroup() %>% 
   mutate(
     consumption_weekly = consumption_hh *48 *7, 
     consumption_yearly = consumption_hh *48*365.25,
     temp_degree = factor(
     case_when(
       daily_avg_air_temperature_celsius < 0 ~ 0,
       daily_avg_air_temperature_celsius < 25.5 ~ round(daily_avg_air_temperature_celsius),
       TRUE ~ 25
     )))

# Build readable labels
temp_levels <- 0:25
temp_labels <- as.character(temp_levels)
temp_labels[temp_levels == 0]  <- "< 0°C"
temp_labels[temp_levels == 25] <- "≥ 25°C"
temp_labels[!(temp_levels %in% c(0, 25))] <-
  paste0(temp_levels[!(temp_levels %in% c(0, 25))], "°C")
rm(m1, m1c, ev_charging, ev_users, ev_charging_agg)

# Fit the model
m1 <- feols(consumption_weekly ~ i(is_hp_installed) | hdd + account_id + date, 
            data =hp_installed, 
            cluster = ~account_id, 
            split = ~ rate_period)

# Unique periods 
periods <- unique(hp_installed$rate_period)

# Run the regression model
tempreg <- feols(consumption_weekly ~ i(is_hp_installed, temp_degree, ref=0) |
                   account_id + temp_degree  + date,
                 data = hp_installed,
                 split = ~ rate_period,
                 cluster = ~account_id)

for (i in 1:5) {
  
  # Find model
  val <- tempreg[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors
  coefs <- coeftable(tempreg[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    as_tibble() %>%
    separate(term, into = c("is_hp_installed", "remove1", "daily_avg_air_temperature_celsius", "remove2"), sep = "::") %>%
    mutate(daily_avg_air_temperature_celsius = as.numeric(daily_avg_air_temperature_celsius),
           lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`
    ) %>%
    mutate(`/% ATE` = Estimate / m1[[j]]$coefficients * 100,     
           lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / m1[[j]]$coefficients * 100),
           upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / m1[[j]]$coefficients * 100)
    ) %>%
    mutate(
      daily_avg_air_temperature_celsius = factor(
      daily_avg_air_temperature_celsius,
      levels = temp_levels,
      labels = temp_labels,
      ordered = TRUE
    )
  )
                    
  temp_breaks <- c(0, 5, 10, 15, 20, 25)
  temp_break_labels <- temp_labels[temp_levels %in% temp_breaks]
  
  # Create the ggplot
  ggplot(coefs, aes(x = daily_avg_air_temperature_celsius, y = Estimate)) +
    geom_point(color = hp_color) +
    geom_line(color = hp_color) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = hp_color) +
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", alpha = 0.6, color = hp_color) +  # Add horizontal line at 100% ATE
    scale_y_continuous(
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    scale_x_discrete(breaks = temp_break_labels)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    labs(
      x = "Average Daily Temperature in Degrees (°C)",
      y = "Weekly estimate (kWh)"
    ) +
    theme_minimal()
  
  # Print the plot
  ggsave(paste0("graphs/hp_temperature_", tolower(gsub(" ", "_", val)), ".png"),
         width = 16, height = 8, units = "cm")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Figure A.6: Impact of Heat Pump Installation by EPC Rating 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(tempreg)

m1 <- feols(consumption_weekly ~ i(is_hp_installed) | hdd + account_id + date, 
            data =hp_installed, 
            cluster = ~account_id, 
            split = ~ rate_period)

m2a <- feols(consumption_weekly ~ i(is_hp_installed, epc_letter, ref=0)  | 
               hdd + account_id + date,
             data = hp_installed %>%  
               filter(treated==1) %>%
               mutate(epc_letter = case_when(
                 energy_efficiency >= 91 ~ "A",
                 energy_efficiency >= 81 & energy_efficiency <= 90 ~ "B",
                 energy_efficiency >= 69 & energy_efficiency <= 80 ~ "C",
                 energy_efficiency >= 55 & energy_efficiency <= 68 ~ "D",
                 energy_efficiency >= 39 & energy_efficiency <= 54 ~ "E",
                 energy_efficiency >= 21 & energy_efficiency <= 38 ~ "F",
                 energy_efficiency <= 20 ~ "G",
                 TRUE ~ NA_character_
               )), 
             split = ~ rate_period,
             cluster = ~ account_id)


for (i in 1:5) {
  # Find model
  val <- m2a[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors
  coefs <- coeftable(m2a[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("is_hp_installed", "remove1", "EPC_letter", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`
    ) %>%
    mutate(`% ATE` = Estimate / m1[[j]]$coefficients * 100,
           lower_ci_ATE = lower_ci / m1[[j]]$coefficients * 100,
           upper_ci_ATE = upper_ci / m1[[j]]$coefficients * 100)
  
  # Assuming coefs is your data frame and rating_colors is your color vector
  coefs <- coefs %>%
    mutate(EPC_label_position = max(`% ATE`) * 0.1)  # Position for EPC labels on the right
  
  # Create the ggplot
  if (mean(coefs$Estimate) > 0) {
    nudge_x <-  0.01
    
  } else {
    nudge_x <- -0.01
  }
  
  ggplot(coefs, aes(y = factor(EPC_letter, levels = rev(unique(EPC_letter))), x = Estimate, fill = factor(EPC_letter))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, color="grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
    geom_vline(xintercept = m1[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add vertical line at 100% ATE
    scale_fill_manual(values = rating_colors, name = "EPC Letter") +  # Use the defined colors
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", alpha = 0.6, color = hp_color) +  # Add horizontal line at 100% ATE
    # Add black "shadow" text for border effect
    geom_text(aes(label = EPC_letter, x = Estimate-nudge_x),  
              size = 4,
              color = "black",  # Black shadow
              fontface = "bold") +
    
    # Add white text on top
    geom_text(aes(label = EPC_letter, x = Estimate-nudge_x),  
              size = 4,
              color = "white",  # White text on top
              fontface = "bold") +
    scale_x_continuous(
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
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
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_epc_", tolower(gsub(" ", "_", val)), ".png"),
         width = 16, height = 8, units = "cm")
}

rm(m2a)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Figure A.10: Impact of Heat Pump Installation by Previous 
# Heat Source
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


m_sources <- feols(consumption_weekly ~ i(is_hp_installed, hp_survey_outcome_existing_heat_source, ref=0)  | 
                     hdd + account_id + date,
                   data = hp_installed %>% filter(treated == 1, !hp_survey_outcome_existing_heat_source==""), 
                   split = ~ rate_period,
                   cluster = ~ account_id)

# Count unique occurrences per 'heat_source'
heat_source_counts <- hp_installed %>%
  filter(treated == 1, !hp_survey_outcome_existing_heat_source=="") %>%
  select(hp_survey_outcome_existing_heat_source, account_id) %>%   
  distinct() %>%
  group_by(hp_survey_outcome_existing_heat_source) %>%
  tally() %>%
  mutate(share = round(100*n/sum(n), digits=2))


for (i in 1:5) {
  # Find model
  val <- m_sources[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Assuming m_sources[[i]] is a model object that has already been defined
  coefs <- coeftable(m_sources[[i]]) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    separate(term, into = c("is_hp_installed", "remove1", "hp_survey_outcome_existing_heat_source", "remove2"), sep = "::") %>%
    mutate(
      heat_source = str_remove_all(hp_survey_outcome_existing_heat_source, "[{}]"),  # Remove curly braces
      heat_source = str_replace_all(heat_source, "_", " "),  # Replace underscores with spaces
      heat_source = str_to_title(heat_source),  # Capitalize the first letter of each word
      # Special handling for specific terms like 'LPG'
      heat_source = stringr::str_replace_all(heat_source, regex("Lpg", ignore_case = TRUE), "LPG"),
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`
    ) %>%
    inner_join(heat_source_counts, by = "hp_survey_outcome_existing_heat_source") %>%
    mutate(`% ATE` = Estimate / m1[[j]]$coefficients * 100,
           lower_ci_ATE = lower_ci / m1[[j]]$coefficients * 100,
           upper_ci_ATE = upper_ci / m1[[j]]$coefficients * 100,
           heat_source = paste0(heat_source,"\n(", format(share, nsmall = 2, trim = TRUE), "%)"))  # Add share to heat source
  
  # Ensure that 'heat_source' is a factor ordered by 'Estimate' values
  coefs <- coefs %>%
    mutate(heat_source = factor(heat_source, levels = heat_source[order(Estimate)]))
  
  # Create the ggplot
  ggplot(coefs, aes(x = heat_source, y = Estimate, fill = heat_source)) +  # Scale Estimate
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_line(color = hp_color) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                  width = 0.2, alpha = 0.6, color = "grey") +  # Scale CI
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    scale_y_continuous(
      name = "Estimate (kWh)",  
      sec.axis = sec_axis(~ ./ (m1[[j]]$coefficients), 
                          name = "% of ATE", 
                          labels = percent_format())  # Secondary y-axis as percentage
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Zero line
    labs(
      x = "Is Installed x Previous Heat Source"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_hs_", tolower(gsub(" ", "_", val)), ".png"),
         width = 20, height = 12, units = "cm")
}

rm(m_sources)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Figure 7: Impact of Heat Pump Installation by MSOA Income
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load and preprocess the cosy_hp_details data
cosy_hp_details <- fread(file.path(datapath,  "input/cosy_-_hp_details_2024_07_03.csv")) %>%
  inner_join(hp_installed %>% filter(treated == 1) %>% select(account_id) %>% distinct()) %>%
  filter(!postcode=="") %>%
  distinct(account_id, postcode)

postcode_msoa <- fread(file.path(datapath, "input/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")) 
                    
postcode_matched <- cosy_hp_details %>%
  distinct(postcode) %>%
  inner_join(postcode_msoa, by = c("postcode"="pcds"))   %>%
  select(msoa21cd, postcode) %>%
  distinct(postcode, .keep_all = TRUE)

income <- readxl::read_excel(file.path(datapath, "input/saiefy1920finalqaddownload280923.xlsx"), sheet = "Total annual income", skip = 4) %>%
  select(`MSOA code`, `Total annual income (£)`) %>%
  distinct(`MSOA code`, .keep_all = TRUE) %>%
  inner_join(postcode_matched, by=c("MSOA code"="msoa21cd")) %>%
  distinct(postcode, .keep_all = TRUE)

cosy_hp_details <- cosy_hp_details %>%
                inner_join(income) %>%
                distinct(account_id, .keep_all=TRUE) %>%
                filter(account_id %in% hp_installed$account_id)

breaks <- unique(quantile(cosy_hp_details$`Total annual income (£)`/1000, probs = seq(0, 1, by = 0.1)))

# Round to nearest thousand
rounded_breaks <- round(breaks)

# Build readable labels like "< £19k", ..., "≥ £26k"
income_labels <- c(paste0("< £", rounded_breaks[-1], "k"))

# For last bin: "≥ £26k"
income_labels[length(income_labels)] <- paste0("£", rounded_breaks[length(rounded_breaks) - 1] , "k+")
income_labels
                    
hp_installed <- hp_installed %>%
  left_join(cosy_hp_details %>% select(c(account_id, `Total annual income (£)`))) %>%
  mutate(income_category = cut(`Total annual income (£)`/1000, 
                               breaks = breaks, 
                               include.lowest = TRUE,
                               labels = income_labels))
# income check
m_income <- feols(consumption_yearly ~ i(is_hp_installed, income_category, ref =0) 
                  | date +  account_id + hdd,
                  data = hp_installed,
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
    mutate(`Income Category` = factor(`Income Category`, levels = income_labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Income Category`, y = Estimate, fill = `Income Category`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Income Decile",
      y = "Yearly Estimate (kWh)"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ./(m1[[j]]$coefficients), name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_income_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
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
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
      lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
      upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
      outcome = "Total Consumption",
      `Floor_Area` = factor(`Income Category`, levels = income_labels),
      period = factor(val, levels = c("Morning Cosy",
                                      "Afternoon Cosy",
                                      "Peak Rate",
                                      "Other", 
                                      "Overall")))
  
  all_coefs <- bind_rows(all_coefs, coefs_total)
}


# Define the shades of reds
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")


# Create the combined ggplot using facet_wrap
ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
       aes(x = `Income Category`, 
           y = Estimate, fill = `Income Category`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Income Category Decile",
    y = "Yearly Estimate (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~ period)


# Save the combined plot
ggsave("graphs/hp_income_category_combined.png", device = "png", width = 16, height = 12, units = "cm")


## property value
# create property value decile
breaks <- hp_installed %>%
  filter(!is.na(property_value)) %>%
  select(account_id, property_value) %>%
  distinct(account_id, .keep_all=TRUE)
breaks <-
  quantile(breaks$property_value/1000, probs = seq(0, 1, by = 0.1)) %>%
  unique()


# Round to nearest thousand
rounded_breaks <- round(breaks)

# Build readable labels like "< £19k", ..., "≥ £26k"
labels <- c(paste0("< £", rounded_breaks[-1], "k"))

# For last bin: "≥ £26k"
labels[length(income_labels)] <- paste0("£", rounded_breaks[length(rounded_breaks) - 1] , "k+")
labels


hp_installed <- hp_installed %>%
  mutate(property_value_category = cut(property_value/1000, 
                                       breaks = breaks, 
                                       include.lowest = TRUE,
                                       labels = labels))
# property_value check
m_property_value <- feols(consumption_yearly ~ i(is_hp_installed, property_value_category, ref =0) 
                          | date +  account_id + hdd,
                          data = hp_installed,
                          split = ~ rate_period,
                          cluster = ~account_id)

for (i in 1:5) {
  
  # Find model
  val <- m_property_value[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_property_value[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Property Value Category", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Property Value Category` = factor(`Property Value Category`, levels = labels))
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Property Value Category`, y = Estimate, 
                          fill = `Property Value Category`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Property Value Decile",
      y = "Yearly Estimate (kWh)"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ./(m1[[j]]$coefficients), name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_property_value_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Figure A.8: Impact of Heat Pump Installation by Heat Loss Decile
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# survey
hl <- fread(file.path(datapath, "input/cosy_-_hp_details_2024_07_03.csv")) %>%
              filter(!is.na(latest_survey_heat_loss))  %>%   
              select(account_id, latest_survey_heat_loss) %>%
              distinct(account_id, .keep_all=TRUE)

# Create unique breaks for predicted_heatloss_watts
breaks <- unique(quantile(hl$latest_survey_heat_loss/1000, probs = seq(0, 1, by = 0.1)))
            
hp_installed <- hp_installed %>%
  left_join(hl)
           
# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i], 1), " mW to ", round(breaks[i+1], 1), " mW"))

# Create the categories for latest_survey_heat_loss
hp_installed <- hp_installed %>%
  mutate(latest_survey_heat_loss_category = cut(latest_survey_heat_loss/1000, 
                                                breaks = breaks, 
                                                include.lowest = TRUE,
                                                labels = labels))

m_heatloss <- feols(consumption_weekly ~ i(is_hp_installed, latest_survey_heat_loss_category, ref =0) 
                    | account_id + hdd + date,
                    data = hp_installed %>% filter(!is.na(latest_survey_heat_loss_category)),
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
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color="grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add horizontal line at ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Heatloss MW Decile",
      y = "Half Hourly Consumption in kWh"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_heatloss_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
         width = 16, height = 8, units = "cm")
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Figure A.9: Impact of Heat Pump Installation on Half-Hourly 
# Electricity Consumption by region
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
m_region <- feols(consumption_weekly ~ i(is_hp_installed, region, ref =0) | 
                    account_id + hdd + date,
                  data = hp_installed %>% filter(!is.na(region), !region=="", rate_period == "Overall"),
                  cluster = ~account_id)


coefs_total <- coeftable(m_region) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  separate(term, into = c("is_hp_installed", "remove1", "Region", "remove2"), sep = "::") %>%
  mutate(
    average = m1[[4]]$coefficients,
    lower_ci = Estimate - 1.96 * `Std..Error`,
    upper_ci = Estimate + 1.96 * `Std..Error`,
    `%_ATE` = Estimate / abs(m1[[4]]$coefficients) * 100,
    lower_ci_ATE = lower_ci / abs(m1[[4]]$coefficients) * 100,
    upper_ci_ATE = upper_ci / abs(m1[[4]]$coefficients) * 100)

# Select a color palette from RColorBrewer
region_colors <- brewer.pal(n = length(unique(coefs_total$Region)), name = "Set3")

# Order regions by their estimate size for period == "Morning Cosy"
hp_order <- coefs_total %>%
  arrange(desc(Estimate)) %>%
  pull(Region)

# Reorder the Region factor based on the estimate size in "Morning Cosy"
coefs_total <- coefs_total %>%
  mutate(Region = factor(Region, levels = hp_order))

# Create the ggplot
ggplot(coefs_total, aes(x = Region, y = Estimate, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_hline(aes(yintercept = m1[[4]]$coefficients), linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add vertical line at average ATE
  scale_fill_manual(values = region_colors) +  # Apply random colors to regions
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  ) 

# Save the plot
ggsave("graphs/hp_region_combined.png", device = "png", width = 16, height = 12, units = "cm")





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Figure A.7: Impact of Heat Pump Installation by Floor Area
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
                    
##
breaks <- hp_installed %>%
  filter(!is.na(total_floor_area)) %>%
  select(account_id, total_floor_area) %>%
  distinct(account_id, .keep_all=TRUE)
breaks <-
  quantile(breaks$total_floor_area, probs = seq(0, 1, by = 0.1)) %>%
  unique()

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i], 0), " to ", round(breaks[i+1], 0), "m sq."))

# Create the categories for total_floor_area
hp_installed <- hp_installed %>%
  mutate(total_floor_area_category = cut(total_floor_area, 
                                         breaks = breaks, 
                                         include.lowest = TRUE,
                                         labels = labels))


m_floor <- feols(consumption_yearly ~ i(is_hp_installed, total_floor_area_category, ref =0) 
                 | date +  account_id + hdd,
                 data = hp_installed %>% filter(!is.na(total_floor_area_category), treated==1),
                 split = ~ rate_period,
                 cluster = ~account_id)

for (i in 1:5) {
  
  # Find model
  val <- m_floor[[i]]$model_info$sample$value
  j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
  
  # Extract coefficients and standard errors for total_consumption
  coefs_total <- coeftable(m_floor[i]) %>%
    data.frame() %>%
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Floor_Area", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Floor_Area` = factor(`Floor_Area`, levels = labels),
           `% ATE` = Estimate / m1[[j]]$coefficients * 100,
           lower_ci_ATE = lower_ci / m1[[j]]$coefficients * 100,
           upper_ci_ATE = upper_ci / m1[[j]]$coefficients * 100)
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Floor_Area`, y = Estimate, fill = `Floor_Area`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add horizontal line at 100% ATE
    scale_fill_manual(values = red_palette) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ./(m1[[j]]$coefficients), name = "% of ATE", labels = scales::percent_format())
    ) +
    labs(
      x = "Floor Area Decile",
      y = "Yearly Estimate (kWh)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_floor_area_", tolower(gsub(" ", "_", val)), ".png"),
         width = 16, height = 8, units = "cm")
}



m_floor_share <- feols(share_consumption ~ i(is_hp_installed, total_floor_area_category, ref =0)
                       | date +  account_id + hdd,
                       data = hp_installed %>% 
                         filter(!is.na(total_floor_area_category), !rate_period=="Overall", treated==1) %>%
                         group_by(account_id, date) %>%
                         mutate(share_consumption = total_consumption/sum(total_consumption)),
                       split = ~ rate_period,
                       cluster = ~account_id)

m1_share <- feols(share_consumption ~ i(is_hp_installed, ref =0)
                  | date +  account_id + hdd,
                  data = hp_installed %>% 
                    filter(!rate_period=="Overall", treated==1) %>%
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
    separate(coefficient, into = c("cosy_contract_active", "remove1", "Floor_Area", "remove2"), sep = "::") %>%
    mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
           upper_ci = Estimate + 1.96 * `Std..Error`,
           outcome = "Total Consumption") %>%
    mutate(`Floor_Area` = factor(`Floor_Area`, levels = labels),
           `% ATE` = Estimate / m1_share[[j]]$coefficients * 100,
           lower_ci_ATE = lower_ci / m1_share[[j]]$coefficients * 100,
           upper_ci_ATE = upper_ci / m1_share[[j]]$coefficients * 100)
  
  # Define the shades of reds
  red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
  
  # Create the ggplot
  ggplot(coefs_total, aes(x = `Floor_Area`, y = Estimate, fill = `Floor_Area`)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
    geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = hp_color, alpha = 0.6) +  # Add horizontal line at 100% ATE
    scale_fill_manual(values = red_palette) +
    labs(
      x = "Floor Area Decile",
      y = "Share of Daily Consumption (%)"
    ) +
    scale_y_continuous(
      breaks = seq(0, max(coefs_total$`% ATE`, na.rm = TRUE)),  # Set y-axis to % ATE with breaks at 100% intervals
      labels = function(x) paste0(x, "%")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot
  ggsave(paste0("graphs/hp_share_floor_area_", tolower(gsub(" ", "_", val)), ".png"),
         width = 16, height = 8, units = "cm")
}
