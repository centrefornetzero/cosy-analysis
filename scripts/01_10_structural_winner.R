
# Load the prices
rates <- fread(file.path(datapath, "input/cosy_-_rate_analysis_2024_07_15.csv"))  %>%
  mutate(valid_from = as.Date(valid_from),
         valid_to = as.Date(valid_to),
         valid_from = ifelse(is.na(valid_from), as.Date("2022-12-13"), valid_from),
         valid_to = ifelse(is.na(valid_to), as.Date("2024-07-15"), valid_to),
         valid_from = as.Date(valid_from),
         valid_to = as.Date(valid_to)
  ) %>%
  filter(!(valid_from == as.Date("2022-12-13") & valid_to == as.Date("2023-03-31")), !valid_from == "2024-06-30") %>%
  mutate(rate_period = case_when(
    rate_start_at == "INTERVAL '04:00:00' HOUR TO SECOND" ~ "Morning Off-peak",
    rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND" ~ "Other",
    rate_start_at == "INTERVAL '13:00:00' HOUR TO SECOND" ~ "Afternoon Off-peak",
    rate_start_at == "INTERVAL '16:00:00' HOUR TO SECOND" ~ "Peak Rate",
    rate_start_at == "INTERVAL '19:00:00' HOUR TO SECOND" ~ "Other",
    TRUE ~ "Other"
  )) %>%
  rename(gsp_group_id=tariff_gsp_group_id)

# Add the typical marginal price as a reference column
marginal_price <- rates %>%
  filter(rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND") %>%
  distinct(gsp_group_id, valid_from, unit_rate) %>%
  rename(typical_marginal_price=unit_rate)

# Merge typical marginal price back into the full dataset
rates <- rates %>%
  left_join(marginal_price) %>%
  mutate(share_of_typical = unit_rate / typical_marginal_price * 100)

# load aggregated data
aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS")) 

# Create unique breaks for property_value
breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$property_value),]$property_value, 
                          probs = seq(0, 1, by = 0.1)))

# Create pretty labels for the categories
labels <- sapply(1:(length(breaks)-1), function(i) paste0("£", round(breaks[i]/1000, 0), "k to £", 
                                                          round(breaks[i+1]/1000, 0), "k"))
labels[length(labels)] <- paste0("£", round(breaks[length(breaks)-1]/1000), "k+")

# Create the categories for property_value
aggregated_data <- aggregated_data %>%
  mutate(property_value_category = cut(property_value, 
                                       breaks = breaks, 
                                       include.lowest = TRUE,
                                       labels = labels))


# get the load shifting coefficients 
m1 <- feols(consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date, 
            data = aggregated_data, 
            cluster = ~account_id, 
            split = ~ rate_period)

m1_coefs <- coeftable(m1) %>%
  data.frame() %>%
  filter(sample != "Overall") %>%
  mutate(rate_period = factor(sample, levels = c("Morning Off-peak",
                                                 "Afternoon Off-peak",
                                                 "Peak Rate",
                                                 "Other", 
                                                 "Overall")))

# get the load shifting coefficient by property value decile
m_property_value <- feols(consumption_hh ~ i(cosy_contract_active, property_value_category, ref =0) 
                          | date +  account_id + hdd,
                          data = aggregated_data %>% filter(!is.na(property_value_category)),
                          split = ~ rate_period,
                          cluster = ~account_id)

all_coefs <- coeftable(m_property_value) %>%
  data.frame() %>%
  filter(sample != "Overall") %>%
  separate(coefficient, into = c("cosy_contract_active", "remove1", "property_value_category", "remove2"), 
           sep = "::") %>%
  mutate(rate_period = factor(sample, levels = c("Morning Off-peak",
                                                 "Afternoon Off-peak",
                                                 "Peak Rate",
                                                 "Other", 
                                                 "Overall")))

# summarise half hourly consumption at the property value, gsp and rate period
# Only the latest rates are used in these calculations, so that cohort or time
# effects do not impact the results

# get the average saving for the sample
cosy_avg_saving <- aggregated_data %>%
  filter(date < first_adoption, !rate_period == "Overall") %>%
  group_by(rate_period) %>%
  summarise(mean_consumption = mean(consumption_hh, na.rm = TRUE)) %>%
  left_join(rates %>% 
              filter(valid_from=="2024-03-31") %>%
              group_by(rate_period) %>%
              summarise(unit_rate = mean(unit_rate), typical_marginal_price = mean(typical_marginal_price)), 
            by = join_by(rate_period)) %>%
  left_join(m1_coefs) %>%
  mutate(weight = ifelse(rate_period == "Other", 30/48,  6/48),
         mean_consumption = mean_consumption*weight,
         cosy_price = mean_consumption*unit_rate,
         marginal_price = mean_consumption*typical_marginal_price,
         load_shifting_gain= Estimate*unit_rate*weight) %>%
  ungroup() %>%
  summarise(cosy_price = mean(cosy_price, na.rm = TRUE),
            marginal_price = mean(marginal_price, na.rm = TRUE),
            load_shifting_gain = mean(load_shifting_gain, na.rm = TRUE),
            mean_consumption = mean(mean_consumption)) %>%  
  mutate(structural_gain =  marginal_price-cosy_price,
          share_gain = 1-(marginal_price-structural_gain+load_shifting_gain)/marginal_price)



# create the saving for each property value decile
cosy_saving5 <- aggregated_data %>%
  filter(date < first_adoption, !rate_period == "Overall", !is.na(property_value_category)) %>%
  group_by(rate_period, property_value_category) %>%
  summarise(mean_consumption = mean(consumption_hh, na.rm = TRUE)) %>%
  left_join(rates %>% 
              filter(valid_from=="2024-03-31") %>%
              group_by(rate_period) %>%
              summarise(unit_rate = mean(unit_rate), typical_marginal_price = mean(typical_marginal_price)), 
            by = join_by(rate_period)) %>%
  ungroup() %>%
  left_join(all_coefs) %>%
  mutate(weight = ifelse(rate_period == "Other", 30/48,  6/48),
         mean_consumption = mean_consumption*weight,
         cosy_price = mean_consumption*unit_rate,
         marginal_price = mean_consumption*typical_marginal_price,
         load_shifting_gain= Estimate*unit_rate*weight) %>%
  group_by(property_value_category) %>%
  summarise(cosy_price = sum(cosy_price, na.rm = TRUE),
            marginal_price = sum(marginal_price, na.rm = TRUE),
            load_shifting_gain = sum(load_shifting_gain, na.rm = TRUE),
            mean_consumption = sum(mean_consumption)) %>%
  mutate( structural_gain =  marginal_price-cosy_price,
          share_gain = 1-(marginal_price-structural_gain+load_shifting_gain)/marginal_price,
          saving_gbp = 365.25*(marginal_price-cosy_price-load_shifting_gain),
          property_value_category = factor(property_value_category, labels = labels))

# Define the shades of reds
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")

# Create the ggplot
ggplot(cosy_saving5, aes(x = `property_value_category`, y = share_gain, fill = `property_value_category`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(yintercept = cosy_avg_saving$share_gain, linetype = "dashed", color = cosy_color, alpha=0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  scale_fill_manual(values = red_palette) +
  labs(
    x = "Property Value Decile",
    y = "Savings %"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "none"  # Remove legend
  )

# Figure A.37: Average Cosy Savings by Property Valu

# Create the ggplot
ggplot(cosy_saving5, aes(x = `property_value_category`, y = share_gain, fill = `property_value_category`)) +
  # Bar plot for share_gain
  geom_bar(stat = "identity", show.legend = FALSE) +
  # Horizontal dashed line for "Average Savings in the Sample (%)" with legend
  geom_hline(aes(yintercept = cosy_avg_saving$share_gain, color = "Average Savings in Sample (%)"), 
             linetype = "dashed", alpha = 0.6, show.legend = TRUE) +
  # Second horizontal line at y = 0 (no legend needed)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Line for savings in GBP on the secondary y-axis
  geom_line(aes(x = `property_value_category`, y = saving_gbp/3000, group = 1, color = "Savings in £"), size = 1) +
  # Manual fill color for bars
  scale_fill_manual(values = red_palette) +
  # Define color legend for the horizontal line and the line plot
  scale_color_manual(name = "Legend", values = c("Savings in Sample (%)" = cosy_color, 
                                                 "Savings in £" = cosy_color)) +
  # Labels
  labs(
    x = "Property Value Decile",
    y = "Savings (%)"
  ) +
  # Primary y-axis for share_gain, secondary y-axis for saving_gbp
  scale_y_continuous(
    labels = scales::percent_format(), 
    sec.axis = sec_axis(~.*3000, name = "Savings in £")
  ) +
  # Minimal theme
  theme_minimal() +
  # Tilt x-axis labels for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none"  # Place legend at the bottom
  )

# Save the combined plot
ggsave("graphs/property_value_average_bill_saving.png", device = "png", width = 16, height = 12, units = "cm")


