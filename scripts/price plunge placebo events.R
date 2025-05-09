library(dplyr)
library(lubridate)
library(data.table)

agile <- fread("~/Downloads/agile-half-hour-actual-rates-01-01-2024_24-09-2024.csv") %>%
  mutate(
    Period_from = dmy_hm(`Period from`),
    Period_to = dmy_hm(`Period to`)
  )  %>%
  mutate(under_zero = `Agile Import price (p/kWh)` < 13,
         group = cumsum(c(1, diff(under_zero) != 0))) %>% # Group consecutive 'under_zero' periods
  group_by(group) %>%
  summarise(
    price = mean(`Agile Import price (p/kWh)`),
    start_time = min(Period_from),
    end_time = max(Period_to),
    duration_hours = as.numeric(difftime(max(Period_to), min(Period_from), units = "hours")),
    all_under_zero = all(under_zero)
  ) %>%
  mutate(
    time_of_day = case_when(
      hour(start_time) >= 6 & hour(start_time) < 9 ~ "Morning",
      hour(start_time) >= 9 & hour(start_time) < 18 ~ "Day",
      hour(start_time) >= 18 ~ "Evening",
      TRUE ~ "Night"
    )
  ) %>%
  ungroup()

# Filter for periods where the price was under zero for more than 4 hours
result <- agile %>%
  filter(all_under_zero == TRUE & duration_hours > 4)

# View the result
table(result$time_of_day)

# save
fwrite(result, "~/Downloads/agile_under_13.csv")
