## Rates Graphs 

### Figure 2: Cosy Rate by Period

# Load the prices
rates <- fread(file.path(datapath, "input/cosy_-_rate_analysis_2024_07_15.csv"))  %>%
  mutate(valid_from = as.Date(valid_from),
         valid_to = as.Date(valid_to),
         valid_from = ifelse(is.na(valid_from), as.Date("2022-12-13"), valid_from),
         valid_to = ifelse(is.na(valid_to), as.Date("2024-07-15"), valid_to),
         valid_from = as.Date(valid_from),
         valid_to = as.Date(valid_to)
  ) %>%
  filter(!(valid_from == as.Date("2022-12-13") & valid_to == as.Date("2023-03-31")), !valid_from == "2024-06-30") 

# Add the typical marginal price as a reference column
marginal_price <- rates %>%
  filter(rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND") %>%
  distinct(tariff_gsp_group_id, valid_from, unit_rate) %>%
  rename(typical_marginal_price=unit_rate)

# Merge typical marginal price back into the full dataset
rates <- rates %>%
  left_join(marginal_price) %>%
  mutate(share_of_typical = unit_rate / typical_marginal_price * 100)

plot_selection <- rates %>%
  filter(tariff_gsp_group_name == "North Western",valid_from == "2022-12-13") 

# Get the unique valid_from date
unique_valid_from <- unique(plot_selection$valid_from)

# Create a sequence of times for the single day in 1-minute intervals
times <- seq(from = as.POSIXct(paste(unique_valid_from, "00:00:00")), 
             to = as.POSIXct(paste(unique_valid_from, "23:59:00")), by = "1 min")

# Initialize rates with NA and group
rate_data <- data.frame(
  time = times,
  rate = NA,
  group = "Cosy"
)

# Function to convert INTERVAL strings to times and apply the rates
apply_rates <- function(data, rates) {
  for (i in 1:nrow(data)) {
    start_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_start_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
    end_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_end_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
    if (start_time > end_time) {
      # Handle cases where the interval crosses midnight
      rates$rate[rates$time >= start_time | rates$time < end_time] <- data$unit_rate[i]
    } else {
      rates$rate[rates$time >= start_time & rates$time < end_time] <- data$unit_rate[i]
    }
  }
  return(rates)
}

# Apply the rates using the most recent period
rate_data <- apply_rates(plot_selection, rate_data)

# Create a data frame for Flexible Octopus with a constant rate
flexible_octopus <- data.frame(
  time = times,
  rate = plot_selection[plot_selection$rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND",]$unit_rate,
  group = "Typical Marginal Price"
)

# Combine both data frames
combined_rates <- rbind(rate_data, flexible_octopus)

# Define the specific rate values for y-axis breaks
rate_values <- sort(unique(plot_selection$unit_rate))

# Define the time periods for shading
shaded_times <- data.frame(
  xmin = as.POSIXct(paste(unique_valid_from, c("04:00:00", "13:00:00", "16:00:00")), format="%Y-%m-%d %H:%M:%S"),
  xmax = as.POSIXct(paste(unique_valid_from, c("07:00:00", "16:00:00", "19:00:00")), format="%Y-%m-%d %H:%M:%S"),
  fill = c("red", "red", "lightblue")
)

# Calculate the typical marginal price (you can adjust this based on your data)
typical_marginal_price <- mean(combined_rates %>% filter(group == "Typical Marginal Price") %>% pull(rate), na.rm = TRUE)

# Add a new column for the share of the typical marginal price
combined_rates <- combined_rates %>%
  mutate(share_of_typical = rate / typical_marginal_price * 100)

# Plot the line chart with Flexible Octopus in dashed line and specific y-axis breaks
# Assuming unique_valid_from is the date used in your 'time' sequence
ggplot(combined_rates, aes(x = time, y = rate, color = group, linetype = group)) +
  geom_rect(data = shaded_times, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line(size = 1) +
  labs(x = "Time of Day",
       y = "Rate (p/kWh)",
       color = "Tariff") +
  scale_x_datetime(date_labels = "%H:%M", 
                   date_breaks = "2 hour", 
                   limits = c(as.POSIXct(min(combined_rates$time)), 
                              as.POSIXct(max(combined_rates$time)- hours(1)))) +  # Set x-axis limits with correct date
  scale_y_continuous(
    name = "Rate (p/kWh)",
    breaks = rate_values,
    labels = scales::label_number(accuracy = 0.01),  # Format y-axis with 2 decimal places
    sec.axis = sec_axis(~ . / typical_marginal_price, 
                        name = "Share of Typical Marginal Price (%)", 
                        labels = scales::percent_format(accuracy = 1))
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Cosy" = cosy_color, "Typical Marginal Price" = flexible_color)) +
  scale_linetype_manual(values = c("Cosy" = "solid", "Typical Marginal Price" = "dashed")) +
  scale_fill_identity() +
  guides(linetype = "none")

ggsave("graphs/Cosy Tariff.png", width = 10, height = 4, dpi = 300)


# Load the prices
rates <- fread(file.path(datapath, "input/cosy_-_rate_analysis_2024_07_15.csv"))  %>%
  mutate(valid_from = as.Date(valid_from),
         valid_to = as.Date(valid_to),
         valid_from = ifelse(is.na(valid_from), as.Date("2022-12-13"), valid_from),
         valid_to = ifelse(is.na(valid_to), as.Date("2024-07-15"), valid_to),
         valid_from = as.Date(valid_from),
         valid_to = as.Date(valid_to)
  ) %>%
  filter(!(valid_from == as.Date("2022-12-13") & valid_to == as.Date("2023-03-31")), !valid_from == "2024-06-30") 

# Add the typical marginal price as a reference column
marginal_price <- rates %>%
  filter(rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND") %>%
  distinct(tariff_gsp_group_id, valid_from, unit_rate) %>%
  rename(typical_marginal_price=unit_rate)

# Merge typical marginal price back into the full dataset
rates <- rates %>%
  left_join(marginal_price) %>%
  mutate(share_of_typical = unit_rate / typical_marginal_price * 100)

plot_selection <- rates %>%
  filter(tariff_gsp_group_name == "North Western",valid_from == "2024-03-31") 

# Get the unique valid_from date
unique_valid_from <- unique(plot_selection$valid_from)

# Create a sequence of times for the single day in half-hour intervals
times <- seq(from = as.POSIXct(paste(unique_valid_from, "00:00:00")), 
             to = as.POSIXct(paste(unique_valid_from, "23:30:00")), by = "30 min")

# Initialize rates with NA and group
rate_data <- data.frame(
  time = times,
  rate = NA,
  group = "Cosy"
)

# Function to convert INTERVAL strings to times and apply the rates
apply_rates <- function(data, rates) {
  for (i in 1:nrow(data)) {
    start_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_start_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
    end_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_end_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
    if (start_time > end_time) {
      # Handle cases where the interval crosses midnight
      rates$rate[rates$time >= start_time | rates$time < end_time] <- data$unit_rate[i]
    } else {
      rates$rate[rates$time >= start_time & rates$time < end_time] <- data$unit_rate[i]
    }
  }
  return(rates)
}

# Apply the rates using the most recent period
rate_data <- apply_rates(plot_selection, rate_data)

# Create a data frame for Flexible Octopus with a constant rate
flexible_octopus <- data.frame(
  time = times,
  rate = plot_selection[plot_selection$rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND",]$unit_rate,
  group = "Typical Marginal Price"
)

# Combine both data frames
combined_rates <- rbind(rate_data, flexible_octopus)

# Define the specific rate values for y-axis breaks
rate_values <- sort(unique(plot_selection$unit_rate))

# Define the time periods for shading
shaded_times <- data.frame(
  xmin = as.POSIXct(paste(unique_valid_from, c("04:00:00", "13:00:00", "16:00:00")), format="%Y-%m-%d %H:%M:%S"),
  xmax = as.POSIXct(paste(unique_valid_from, c("07:00:00", "16:00:00", "19:00:00")), format="%Y-%m-%d %H:%M:%S"),
  fill = c("lightblue", "lightblue", "red")
)

# Calculate the typical marginal price (you can adjust this based on your data)
typical_marginal_price <- mean(combined_rates %>% filter(group == "Typical Marginal Price") %>% pull(rate), na.rm = TRUE)

# Add a new column for the share of the typical marginal price
combined_rates <- combined_rates %>%
  mutate(share_of_typical = rate / typical_marginal_price * 100)

# Plot the line chart with Flexible Octopus in dashed line and specific y-axis breaks
ggplot(combined_rates, aes(x = time, y = rate, color = group, linetype = group)) +
  geom_rect(data = shaded_times, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line(size = 1) +
  labs(x = "Time of Day",
       y = "Rate (p/kWh)",
       color = "Tariff") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
  scale_y_continuous(
    name = "Rate (p/kWh)",
    breaks = rate_values,
    labels = scales::label_number(accuracy = 0.01),  # Format y-axis with 2 decimal places
    sec.axis = sec_axis(~ . / typical_marginal_price, 
                        name = "Share of Typical Marginal Price (%)", 
                        labels = scales::percent_format(accuracy = 1))
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Cosy" = cosy_color, "Typical Marginal Price" = flexible_color)) +
  scale_linetype_manual(values = c("Cosy" = "solid", "Typical Marginal Price" = "dashed")) +
  scale_fill_identity() +
  guides(linetype = "none")




### Figure A.16: Rates by Rate Period and GSP Group as of 01 June 2024

# Create rate_period indicator
rates <- rates %>%
  mutate(rate_period = case_when(
    rate_start_at == "INTERVAL '04:00:00' HOUR TO SECOND" ~ "Morning \n & Afternoon Cosy",
    rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND" ~ "Other \n ( ~ Typical Marginal Price)",
    rate_start_at == "INTERVAL '13:00:00' HOUR TO SECOND" ~ "Morning \n & Afternoon Cosy",
    rate_start_at == "INTERVAL '16:00:00' HOUR TO SECOND" ~ "Peak Rate",
    rate_start_at == "INTERVAL '19:00:00' HOUR TO SECOND" ~ "Other \n ( ~ Typical Marginal Price)",
    TRUE ~ "Other"
  ),
  rate_period = factor(rate_period, levels = c("Morning \n & Afternoon Cosy","Other \n ( ~ Typical Marginal Price)", "Peak Rate")))

# Get the number of unique GSP group names
num_gsp_groups <- length(unique(rates$tariff_gsp_group_name))

# Define a color palette using RColorBrewer and colorRampPalette to generate more colors if needed
palette <- colorRampPalette(brewer.pal(12, "Set3"))(num_gsp_groups)

# Add the typical marginal price as a reference column
rates_selection <- rates %>%
  filter(valid_from == "2024-03-31") 


# Part 1: Bar graph showing all the rates by rate_period and GSP group name
ggplot(rates_selection, aes(x = rate_period, y = unit_rate, fill = tariff_gsp_group_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rate Period",
       y = "Rate (p/kWh)",
       fill = "GSP Group") +
  scale_y_continuous(
    name = "Rate (p/kWh)",
    labels = scales::label_number(accuracy = 0.01),  # Format y-axis with 2 decimal places
    sec.axis = sec_axis(~ . / typical_marginal_price, 
                        name = "Share of Typical Marginal Price (%)", 
                        labels = scales::percent_format(accuracy = 1))
  ) +
  theme_minimal() +
  scale_fill_manual(values = palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("graphs/Rates_by_Rate_Period_and_GSP_Group.png", width = 10, height = 6, dpi = 300)



### Figure A.17: Rates Over Time

# Get the global min and max unit_rate
global_min_rate <- min(rates$unit_rate, na.rm = TRUE)
global_max_rate <- max(rates$unit_rate, na.rm = TRUE)

# Part 2: Line graphs showing the change in rates over time for each rate_period, grouped by region
rates_long <- rates %>%
  group_by(tariff_gsp_group_name, rate_period) %>%
  arrange(valid_from)

# Create annotations data frame
annotations <- rates_long %>%
  group_by(rate_period) %>%
  summarise(
    valid_from = as.Date("2023-01-01"),
    share_of_typical = median(share_of_typical, na.rm = TRUE),
    unit_rate = median(unit_rate, na.rm = TRUE),
    tariff_gsp_group_name = unique(tariff_gsp_group_name)
  )

ggplot(rates_long, aes(x = valid_from, y = unit_rate, color = tariff_gsp_group_name, group = interaction(tariff_gsp_group_name, rate_period))) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Rate (p/kWh)",
    color = "GSP Group") +
  theme_minimal() +
  scale_color_manual(values = palette) +
  ylim(global_min_rate, global_max_rate) +
  geom_text(data = annotations, aes(label = rate_period), vjust = 0.8, hjust = 0, color = "grey")


ggsave("graphs/Rate_Changes_by_Period.png", width = 12, height = 8, dpi = 300)




### Figure A.18: Rates as Share of Typical Marginal Price Over Time

# Aggregate the data by rate_period and valid_from to get the average for all GSP groups
rates_avg <- rates %>%
  group_by(rate_period, valid_from) %>%
  summarise(
    avg_unit_rate = mean(unit_rate, na.rm = TRUE),
    avg_share_of_typical = mean(share_of_typical, na.rm = TRUE)
  )

# Create annotations data frame
annotations <- rates_avg %>%
  group_by(rate_period) %>%
  summarise(
    valid_from = as.Date("2023-01-01"),
    avg_unit_rate = median(avg_unit_rate, na.rm = TRUE),
    avg_share_of_typical = median(avg_share_of_typical, na.rm = TRUE)
  )

# Define specific y-axis breaks for share of typical marginal price
share_breaks <- seq(0, 160, 20)

# Define a palette of blue colors
blue_palette <- scales::brewer_pal(palette = "Blues")(length(unique(rates_avg$rate_period)))

# Plot average share of typical marginal price over time
ggplot(rates_avg, aes(x = valid_from, y = avg_share_of_typical, color = rate_period, group = rate_period)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Average Share of Typical Marginal Price (%)",
    color = "Rate Period"
  ) +
  theme_minimal() +
  scale_color_manual(values = blue_palette) +
  scale_y_continuous(
    name = "Average Share of Typical Marginal Price (%)",
    breaks = share_breaks,
    labels = scales::percent_format(accuracy = 0.1, scale = 1)
  ) + 
  theme(legend.position = "bottom") +
  geom_text(data = annotations, aes(label = rate_period), vjust = 1.2, hjust = 0, color = "grey")


ggsave("graphs/Average_Share_of_Typical_Marginal_Price_by_Period.png", width = 12, height = 8, dpi = 300)

