# Detect HP install


library(data.table)
library(dplyr)
library(lubridate)

hp_install <- fread("~/Downloads/equinox winter 1 HP install dates.csv") %>%
  mutate(`Date of install` = as.Date(`Date of install`, format = "%d/%m/%Y")) %>% 
  filter(`Date of install` > as.Date("2017-11-05", format = "%Y-%m-%d")) 

Cosy_HP_Adopters_2024 <- fread("data/input/Cosy HP Adopters Apr 24.csv") %>%
  inner_join(hp_install, by=c("account_number"="kid")) %>%
  mutate(read_date = as.Date(read_date, format = "%Y-%m-%d"),
         month = month(read_date),
         weeks_since_hp = ceiling(as.numeric(difftime(read_date, `Date of install`, units = "weeks"))),
         weeks_recoded = ifelse(weeks_since_hp< -52, -52, weeks_since_hp),
         weeks_recoded = ifelse(weeks_recoded> 52, 52, weeks_recoded), 
         # Extract the year from the date
         year = year(read_date),
         # Extract the month as a numerical value
         month_num = month(read_date),
         # Mark as winter if the month is within November to April
         is_winter = (month_num >= 11 | month_num <= 4),
         # Adjust the year for months from January to April to count them as previous year's winter season
         winter_year = ifelse(month_num <= 4, year - 1, year),
  )




m <- feols(daily_value ~ i(weeks_recoded, ref=-1) | account_number + read_date, data = Cosy_HP_Adopters_2024 %>%
             filter(is_winter),
           cluster = ~ account_number)
etable(m)
coefplot(m)


m <- feols(daily_value ~ HP | account_number + read_date, data = Cosy_HP_Adopters_2024 %>%
             filter(is_winter) %>%
             mutate(HP = read_date>=`Date of install`),
           cluster = ~ account_number)
etable(m)
