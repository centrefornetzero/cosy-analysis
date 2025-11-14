############################## Initial numbers from paper/other sources ###################################################
# We will keep everything in 2023 currency
# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
############################# Hard coded numbers #######################################################

# ________ values which can be changed _____________
discount_rate <- 0.035 # 0.035 0.02
social_cost_of_carbon <- "uk" #"usa""uk"
energy_prices <- "hmg" #"hmg""octopus"
gas_connection <- "no" #"yes""no"
percent_marginal_consumers <- 0.5
base_year <- 2024
uk_gdp_as_proportion_of_global <- 0.032
uk_tax_as_proportion_of_gdp <- 0.335
datapath <- "../gcs/cosy2"
# ______________________________________________________

# ___________________ Heat pump numbers from paper __________________________________
# per year
# change in electricity consumption due to heat pump annually in kWh
electricity_change_heatpump <- 3080.0
# change in gas consumption due to heat pump annually in kWh
gas_change_heatpump <- -9350.7
# overall energy change in kWh.  (why is it not 3080 -9350.7 = -6,270.7)
total_energy_change_heatpump <- -6119.8

# initial electricity consumption annually in kWh
electricity_initial_use_heatpump <- 5062.1
# initial gas consumption annually in kWh
gas_initial_use_heatpump <- 10355.7
# overall initial energy usage in kWh
total_initial_energy_use_heatpump <- 15287.2

gov_subsidy_heatpump <- 7500

# from https://assets.publishing.service.gov.uk/media/5f4e14328fa8f57fba704517/cost-of-installing-heating-measures-in-domestic-properties.pdf
private_cost_gas_boiler <- 2250
implied_before_tax_cost_boiler <- private_cost_gas_boiler/1.2

# ___________________________ Cosy numbers _________________________________________
# per half hour
morning_time_initial_use_cosy <- 0.3659 
afternoon_time_initial_use_cosy <- 0.3199
peak_time_initial_use_cosy <- 0.4404
other_time_initial_use_cosy <- 0.3843

morning_time_change_cosy <- 0.5071
afternoon_time_change_cosy <- 0.2926
peak_time_change_cosy <- -0.2242
other_time_change_cosy <- -0.1066

morning_time_length_cosy <- 6
afternoon_time_length_cosy <- 6
peak_time_length_cosy <- 6
other_time_length_cosy <- 30

morning_time_price_cosy <- 0.1151
afternoon_time_price_cosy <- 0.1151
peak_time_price_cosy <- 0.3406
other_time_price_cosy <- 0.2349

# _____________ Octopus electricity prices in 2023 __________
# convert to yearly
electricity_standing_charge_octopus <- 0.5803 * 365
gas_standing_charge_octopus <- 0.2936 * 365

# per kWh
electricity_unit_rate_octopus <- 0.245
gas_unit_rate_octopus <- 0.0604


# ------------ import all the data from different sources about social cost of carbon, carbon intensity, etc --------

# _____________ UK GDP deflator data __________________________
deflator_df <- read.csv(file.path(datapath, "input/GDP deflator.csv"))
# Keep only the desired columns
deflator_df <- deflator_df[, c(8, 9)]
# remove empty rows
deflator_df <- deflator_df[-c(1:9), ]
# Rename column X.6 to year
colnames(deflator_df)[1] <- "Year"
# Rename column X.7 to deflator
colnames(deflator_df)[2] <- "Deflator"
deflator_df$Year <- as.numeric(as.character(deflator_df$Year))
# where 2023 is 100 
gdp_deflator_2020 <- as.numeric(deflator_df %>% filter(Year == 2020) %>% pull(Deflator))
gdp_deflator_2021 <- as.numeric(deflator_df %>% filter(Year == 2021) %>% pull(Deflator))
gdp_deflator_2022 <- as.numeric(deflator_df %>% filter(Year == 2022) %>% pull(Deflator))
gdp_deflator_2023 <- as.numeric(deflator_df %>% filter(Year == 2023) %>% pull(Deflator))

# ____________________ Air quality data __________________________
air_quality_df <- read.csv(file.path(datapath, "input/Air quality.csv"))
# clean csv
air_quality_df <- as.data.frame(t(air_quality_df)) 
# Make row 2 the column names (the type of fuel)
colnames(air_quality_df) <- air_quality_df[2, ]
# Rename column V10 to year
colnames(air_quality_df)[10] <- "Year"
# Keep only the desired columns
air_quality_df <- air_quality_df[, -c(1:9, 11, 14:ncol(air_quality_df))]
# remove empty rows
air_quality_df <- air_quality_df[-c(1:3), ]
# convert to pounds 
air_quality_df$Gas_air_quality <- as.numeric(as.character(air_quality_df$Gas)) / 100
air_quality_df$Elec_air_quality <- as.numeric(as.character(air_quality_df$Electricity)) / 100
air_quality_df <- air_quality_df[, c("Gas_air_quality", "Elec_air_quality", "Year"), drop = FALSE]
air_quality_df$Year <- as.numeric(as.character(air_quality_df$Year))
air_quality_df$Gas_air_quality <- air_quality_df$Gas_air_quality  * gdp_deflator_2023 / gdp_deflator_2022
air_quality_df$Elec_air_quality <- air_quality_df$Elec_air_quality * gdp_deflator_2023 / gdp_deflator_2022

# ______________ UK carbon intensity data DEFRA kg/kWh carbon intensity for gas _____________
defra_df <- read.csv(file.path(datapath, "input/Defra gas and elec carbon intensity.csv"))
# Make row 2 the column names (the type of fuel)
colnames(defra_df) <- defra_df[6, ]
# remove irrelevant rows
defra_df <- defra_df[-c(1:22, 27:38), ]
# only keep row called kWh (Net CV)
defra_df <- defra_df[defra_df$Unit == "kWh (Net CV)", , drop = FALSE]
# only keep column called CO2e
defra_df <- defra_df[, "kg CO2e", drop = FALSE]  
years <- 2024:2043
defra_df <- data.frame(Year = years, Carbon_intensity_gas_heatpump = rep(defra_df$`kg CO2e`, length(years)))
# convert to tonnes
defra_df$Carbon_intensity_gas_heatpump <- as.numeric(as.character(defra_df$Carbon_intensity_gas_heatpump))
defra_df$Carbon_intensity_gas_heatpump <- defra_df$Carbon_intensity_gas_heatpump/1000

# ____________________ UK carbon intensity data for electricity ____________________
desnz_df <- read.csv(file.path(datapath, "input/DESNZ elec carbon intensity.csv"))
# domestic consumption based long run marginal carbon intensity
# Make row 12 the column names (the type of fuel)
colnames(desnz_df) <- desnz_df[12, ]
# remove irrelevant rows
desnz_df <- desnz_df[-c(1:12), ]
# only keep column called Domestic and Year
desnz_df <- desnz_df[, c("Domestic", "Year"), drop = FALSE]
desnz_df$Carbon_intensity_electricity_heatpump <- desnz_df$Domestic
desnz_df <- desnz_df[, c("Carbon_intensity_electricity_heatpump", "Year"), drop = FALSE]
desnz_df$Year <- as.numeric(as.character(desnz_df$Year))
# convert to tonnes
desnz_df$Carbon_intensity_electricity_heatpump <- as.numeric(as.character(desnz_df$Carbon_intensity_electricity_heatpump))
desnz_df$Carbon_intensity_electricity_heatpump <- desnz_df$Carbon_intensity_electricity_heatpump/1000

# ____________________ MCS cost data  ____________________
mcs_cost_df <- read.csv(file.path(datapath, "input/MCS cost data.csv"))
# Make row 2 the column names 
colnames(mcs_cost_df) <- mcs_cost_df[2, ]
# find average installation cost
mcs_cost_df <- mcs_cost_df[49:56, ]
# find average installation cost
mcs_cost_df$`Average installation cost (£)` <- as.numeric(gsub(",", "", mcs_cost_df$`Average installation cost (£)`))
total_installation_cost_heatpump <- mean(mcs_cost_df$`Average installation cost (£)`, na.rm = TRUE)
private_cost_heatpump <- total_installation_cost_heatpump - gov_subsidy_heatpump

# ____________________ NGESO carbon intensity data for ____________________
ngeso_df <- read.csv(file.path(datapath, "input/NGESO carbon intensity.csv"))

# ____________________ Retail price data energy ____________________
retail_prices_df <- read.csv(file.path(datapath, "input/Retail energy prices forecast.csv"))
# use retail prices forecast - its in 2022 currency and in pence
# Make row 8 the column names 
colnames(retail_prices_df) <- retail_prices_df[8, ]
# remove empty rows
retail_prices_df <- retail_prices_df[-c(1:8), ]
# convert air quality to 2023 currency
colnames(retail_prices_df) <- make.names(colnames(retail_prices_df), unique = TRUE)
retail_prices_df$Domestic.1 <- as.numeric(gsub(",", "", retail_prices_df$Domestic.1))
retail_prices_df$Domestic.4 <- as.numeric(gsub(",", "", retail_prices_df$Domestic.4))
retail_prices_df$Electricity_prices <- retail_prices_df$`Domestic.1`/100
retail_prices_df$Gas_prices <- retail_prices_df$Domestic.4/100
retail_prices_df$Gas_prices <- retail_prices_df$Gas_prices  * gdp_deflator_2023 / gdp_deflator_2022
retail_prices_df$Electricity_prices <- retail_prices_df$Electricity_prices * gdp_deflator_2023 / gdp_deflator_2022
retail_prices_df <- retail_prices_df[, c("Gas_prices", "Electricity_prices", "Year"), drop = FALSE]
retail_prices_df$Year <- as.numeric(as.character(retail_prices_df$Year))

# __________ UK social cost of carbon data ____________________
scc_hmg_df <- read.csv(file.path(datapath, "input/SCC HMG.csv"))
# Make row 3 the column names 
colnames(scc_hmg_df) <- scc_hmg_df[3, ]
# remove empty rows
scc_hmg_df <- scc_hmg_df[-c(1:3), ]
# keep important columns
scc_hmg_df$Social_cost_carbon_uk_gov <- scc_hmg_df$`Central Series`
scc_hmg_df <- scc_hmg_df[, c("Social_cost_carbon_uk_gov", "Year"), drop = FALSE]
scc_hmg_df$Year <- as.numeric(as.character(scc_hmg_df$Year))
scc_hmg_df$Social_cost_carbon_uk_gov <- as.numeric(as.character(scc_hmg_df$Social_cost_carbon_uk_gov))
scc_hmg_df$Social_cost_carbon_uk_gov <- scc_hmg_df$Social_cost_carbon_uk_gov * gdp_deflator_2023 / gdp_deflator_2020

# ____________ USA social cost of carbon data ________________
scc_iwf_df <- read.csv(file.path(datapath, "input/SCC IWG.csv"))
# Make row 3 the column names 
colnames(scc_iwf_df) <- scc_iwf_df[2, ]
# remove empty rows
scc_iwf_df <- scc_iwf_df[-c(1:2), ]
# use 3% discount rate
scc_iwf_df$Social_cost_carbon_usa_gov_dollars  <- scc_iwf_df$`3.0%_CO2`
scc_iwf_df$Year  <- scc_iwf_df$`year`
scc_iwf_df$exchange_rate <- 0.8
scc_iwf_df$Social_cost_carbon_usa_gov_dollars <- as.numeric(gsub(",", "", scc_iwf_df$Social_cost_carbon_usa_gov_dollars))
scc_iwf_df$Social_cost_carbon_usa_gov <- scc_iwf_df$Social_cost_carbon_usa_gov_dollars * scc_iwf_df$exchange_rate
scc_iwf_df <- scc_iwf_df[, c("Social_cost_carbon_usa_gov", "Year"), drop = FALSE]
scc_iwf_df$Year <- as.numeric(as.character(scc_iwf_df$Year))
scc_iwf_df$Social_cost_carbon_usa_gov <- scc_iwf_df$Social_cost_carbon_usa_gov * gdp_deflator_2023 / gdp_deflator_2021

# _________________ System price data _____________________________
system_price_df <- read.csv(file.path(datapath, "input/System price.csv"))
# Make row 3 the column names 
colnames(system_price_df) <- system_price_df[3, ]
system_price_df <- system_price_df[-c(1:3), ]
# Keep only the desired columns
system_price_df <- system_price_df[, c(1:4)]
colnames(system_price_df)[4] <-"2024"
colnames(system_price_df)[1] <- "Cosy_intervals"
# half hour intervals: 
# overnight cosy 4am-7am: cosy_intervals 9-14
# day cosy 1pm-4pm: cosy_intervals 27-32
# peak 4pm-7pm: cosy_intervals 33-38
# other times: cosy_intervals: 1-8, 15-26, 39-48
# units seem to be in 
system_price_df$cosy <- with(system_price_df, 
                                      ifelse(Cosy_intervals %in% 9:14, "off_peak_night",
                                      ifelse(Cosy_intervals %in% 27:32, "off_peak_afternoon",
                                      ifelse(Cosy_intervals %in% 33:38, "peak",
                                      "normal_rate"))))
# find mean prices of electricity for every kWh of electricity 
peak_time_price_2022_cosy <- (system_price_df %>%
  filter(cosy == "peak") %>% 
  summarise(avg_price = mean(`2022`, na.rm = TRUE)) %>% 
  pull(avg_price) )/1000
peak_time_price_2023_cosy <- (system_price_df %>%
  filter(cosy == "peak") %>% 
  summarise(avg_price = mean(`2023`, na.rm = TRUE)) %>% 
  pull(avg_price) )/1000
peak_time_price_2024_cosy <- (system_price_df %>%
  filter(cosy == "peak") %>% 
  summarise(avg_price = mean(as.numeric(`2024`), na.rm = TRUE)) %>%  
  pull(avg_price)  )/1000
off_peak_night_price_2022_cosy <- (system_price_df %>%
  filter(cosy == "off_peak_night") %>% 
  summarise(avg_price = mean(`2022`, na.rm = TRUE)) %>% 
  pull(avg_price)   )/1000
off_peak_night_price_2023_cosy <- (system_price_df %>%
  filter(cosy == "off_peak_night") %>% 
  summarise(avg_price = mean(`2023`, na.rm = TRUE)) %>% 
  pull(avg_price) )/1000
off_peak_night_price_2024_cosy <- (system_price_df %>%
  filter(cosy == "off_peak_night") %>% 
  summarise(avg_price = mean(as.numeric(`2024`), na.rm = TRUE)) %>%  
  pull(avg_price)  )/1000 
off_peak_afternoon_price_2022_cosy <- (system_price_df %>%
  filter(cosy == "off_peak_afternoon") %>% 
  summarise(avg_price = mean(`2022`, na.rm = TRUE)) %>% 
  pull(avg_price)   )/1000
off_peak_afternoon_price_2023_cosy <- (system_price_df %>%
  filter(cosy == "off_peak_afternoon") %>% 
  summarise(avg_price = mean(`2023`, na.rm = TRUE)) %>% 
  pull(avg_price)   )/1000 
off_peak_afternoon_price_2024_cosy <- (system_price_df %>%
  filter(cosy == "off_peak_afternoon") %>% 
  summarise(avg_price = mean(as.numeric(`2024`), na.rm = TRUE)) %>%  
  pull(avg_price)   )/1000
normal_price_2022_cosy <- (system_price_df %>%
  filter(cosy == "normal_rate") %>% 
  summarise(avg_price = mean(`2022`, na.rm = TRUE)) %>% 
  pull(avg_price)  )/1000
normal_price_2023_cosy <- (system_price_df %>%
  filter(cosy == "normal_rate") %>% 
  summarise(avg_price = mean(`2023`, na.rm = TRUE)) %>% 
  pull(avg_price)  )/1000
normal_price_2024_cosy <- (system_price_df %>%
  filter(cosy == "normal_rate") %>% 
  summarise(avg_price = mean(as.numeric(`2024`), na.rm = TRUE)) %>% 
  pull(avg_price) )/1000

# _______ UK emissions trading scheme average prices ____________________
uk_ets_df <- read.csv(file.path(datapath, "input/UK ETS.csv"))
# Make row 5 the column names 
colnames(uk_ets_df) <- uk_ets_df[5, ]
# remove empty rows
uk_ets_df <- uk_ets_df[-c(1:6), ]
uk_ets_df$Carbon_trading_permit_prices <- uk_ets_df$`Market Carbon Values`
# only keep column called Carbon_trading_permit_prices and Year
uk_ets_df <- uk_ets_df[, c("Carbon_trading_permit_prices", "Year"), drop = FALSE]
uk_ets_df$Year <- as.numeric(as.character(uk_ets_df$Year))

# ____________________ WattTime marginal carbon intensity ___________________________
watt_time_df <- read.csv(file.path(datapath, "input/WattTime marginal carbon intensity.csv"))
# in g/kWh so divide by 1000
watt_time_df <- as.data.frame(t(watt_time_df)) 
# Make row 8 the column names 
colnames(watt_time_df) <- watt_time_df[1, ]
# remove empty rows and columns
watt_time_df <- watt_time_df[-c(1:3), ]
watt_time_df <- watt_time_df[, -c(1)]
# Rename column .1 to year
colnames(watt_time_df)[1] <- "Year"
# Rename column Off Peak Morning
colnames(watt_time_df)[2] <- "Carbon_intensity_off_peak_morning_cosy"
watt_time_df$Carbon_intensity_off_peak_morning_cosy <- as.numeric(as.character(watt_time_df$Carbon_intensity_off_peak_morning_cosy))
watt_time_df$Carbon_intensity_off_peak_morning_cosy <- watt_time_df$Carbon_intensity_off_peak_morning_cosy/1000
# Rename column Off Peak Afternoon
colnames(watt_time_df)[3] <- "Carbon_intensity_off_peak_afternoon_cosy"
watt_time_df$Carbon_intensity_off_peak_afternoon_cosy <- as.numeric(as.character(watt_time_df$Carbon_intensity_off_peak_afternoon_cosy))
watt_time_df$Carbon_intensity_off_peak_afternoon_cosy <- watt_time_df$Carbon_intensity_off_peak_afternoon_cosy/1000
# Rename column Peak
colnames(watt_time_df)[4] <- "Carbon_intensity_peak_cosy"
watt_time_df$Carbon_intensity_peak_cosy <- as.numeric(as.character(watt_time_df$Carbon_intensity_peak_cosy))
watt_time_df$Carbon_intensity_peak_cosy <- watt_time_df$Carbon_intensity_peak_cosy/1000
# Rename column Normal Rate
colnames(watt_time_df)[5] <- "Carbon_intensity_normal_rate_cosy"
watt_time_df$Carbon_intensity_normal_rate_cosy <- as.numeric(as.character(watt_time_df$Carbon_intensity_normal_rate_cosy))
watt_time_df$Carbon_intensity_normal_rate_cosy <- watt_time_df$Carbon_intensity_normal_rate_cosy/1000
watt_time_df$Year <- as.numeric(as.character(watt_time_df$Year))
# extend it from 2024-2043
# Create a new data frame with extended years and repeat the last available values
extra_years <- 2025:2043
df_extra <- data.frame(
  Year = extra_years,
  Carbon_intensity_off_peak_morning_cosy = rep(NA, length(extra_years)), 
  Carbon_intensity_off_peak_afternoon_cosy = rep(NA, length(extra_years)),
  Carbon_intensity_peak_cosy = rep(NA, length(extra_years)),
  Carbon_intensity_normal_rate_cosy = rep(NA, length(extra_years)))
# Bind the original dataframe with the extended rows
watt_time_df <- bind_rows(watt_time_df, df_extra)



# ---------------------learning by doing numbers -----------------------------------
# calculations from MVPF climate paper Hahn, Hendren, Metcalfe and Sprung-Keyser 2024
# look at other code

lbd_environmental_heatpump <- 3192.62
lbd_price_heatpump <- 1697.59

# -------------------------------------------------------------------------------------------------------------------------

# From 2024-2043 - we are working over a 20 year time horizon
# merge all the dataframes together

whole_df <- left_join(defra_df, air_quality_df, by = "Year")
whole_df <- left_join(whole_df, desnz_df, by = "Year")
whole_df <- left_join(whole_df, retail_prices_df, by = "Year")
whole_df <- left_join(whole_df, scc_hmg_df, by = "Year")
whole_df <- left_join(whole_df, scc_iwf_df, by = "Year")
whole_df <- left_join(whole_df, uk_ets_df, by = "Year")
whole_df <- left_join(whole_df, watt_time_df, by = "Year")
whole_df[] <- lapply(whole_df, function(x) as.numeric(as.character(x)))


# fill in the rest of the years for carbon intensity for cosy - use DESNZ carbon intensity for electricity for the future
# Extract reference values for all carbon intensity types and electricity heatpump
reference_values <- whole_df %>%
  filter(Year == base_year) %>%
  select(  Carbon_intensity_normal_rate_cosy, 
    Carbon_intensity_off_peak_morning_cosy,
    Carbon_intensity_off_peak_afternoon_cosy,
    Carbon_intensity_peak_cosy,
    Carbon_intensity_electricity_heatpump)
# Apply the transformation for all years >= 2025
whole_df <- whole_df %>%
  mutate(
    Carbon_intensity_normal_rate_cosy = ifelse(
      Year >= 2025,
      reference_values$Carbon_intensity_normal_rate_cosy * Carbon_intensity_electricity_heatpump / reference_values$Carbon_intensity_electricity_heatpump,
      Carbon_intensity_normal_rate_cosy ),
    Carbon_intensity_off_peak_morning_cosy = ifelse(
      Year >= 2025,
      reference_values$Carbon_intensity_off_peak_morning_cosy * Carbon_intensity_electricity_heatpump / reference_values$Carbon_intensity_electricity_heatpump,
      Carbon_intensity_off_peak_morning_cosy ),
    Carbon_intensity_off_peak_afternoon_cosy = ifelse(
      Year >= 2025,
      reference_values$Carbon_intensity_off_peak_afternoon_cosy * Carbon_intensity_electricity_heatpump / reference_values$Carbon_intensity_electricity_heatpump,
      Carbon_intensity_off_peak_afternoon_cosy  ),
    Carbon_intensity_peak_cosy = ifelse(
      Year >= 2025,
      reference_values$Carbon_intensity_peak_cosy * Carbon_intensity_electricity_heatpump / reference_values$Carbon_intensity_electricity_heatpump,
      Carbon_intensity_peak_cosy ) )


############################## Intermediate Calculations ###################################################


# ---------------------- individual wtp for heat pump -------------------
# price of heatpump - marginal consumers wtp- assume half of subsidy since we do not know what price they are marginal at
marginal_wtp_heatpump <- percent_marginal_consumers * 0.5 * gov_subsidy_heatpump
# inframarginal consumers would have been wtp the entire thing since they would have bought it anyway but now gain
inframarginal_wtp_heatpump <- (1-percent_marginal_consumers) * gov_subsidy_heatpump


# _____ 1. using octopus energy prices _____________
whole_df$Gas_price_change_octopus_heatpump <- gas_unit_rate_octopus * gas_change_heatpump
whole_df$Electricity_price_change_octopus_heatpump <- electricity_unit_rate_octopus * electricity_change_heatpump
# if they continue having gas at home in some capacity
whole_df$Change_payments_octopus <- whole_df$Electricity_price_change_octopus_heatpump + whole_df$Gas_price_change_octopus_heatpump 
# if they cut off their gas connection entirely
whole_df$Change_payments_octopus_no_standing <- whole_df$Change_payments_octopus - gas_standing_charge_octopus

# __________ 2. using hmg prices ___________________
whole_df$Gas_price_change_heatpump <- whole_df$Gas_prices * gas_change_heatpump
whole_df$Electricity_price_change_heatpump <- whole_df$Electricity_prices * electricity_change_heatpump
# if they continue having gas at home in some capacity
whole_df$Change_payments_hmg <- whole_df$Electricity_price_change_heatpump + whole_df$Gas_price_change_heatpump 
# if they cut off their gas connection entirely
whole_df$Change_payments_hmg_no_standing <- whole_df$Change_payments_hmg - gas_standing_charge_octopus

whole_df <- whole_df %>%
  mutate(Change_energy_payments = case_when(
    energy_prices == "octopus" & gas_connection == "yes" ~ Change_payments_octopus,
    energy_prices == "octopus" & gas_connection == "no" ~ Change_payments_octopus_no_standing,
    energy_prices == "hmg" & gas_connection == "yes" ~ Change_payments_hmg,
    energy_prices == "hmg" & gas_connection == "no" ~ Change_payments_hmg_no_standing ))

# Apply the discounting formula
whole_df <- whole_df %>%
  mutate(Discounted_change_energy_payments = Change_energy_payments / (1 + discount_rate)^(Year - base_year), )


# ---------------------- environmental wtp for heat pump -------------------

# ------------ carbon wtp ----------------
# find the change in co2 due to gas usage using carbon intensity
whole_df$Gas_carbon_change_heatpump <- whole_df$Carbon_intensity_gas_heatpump * gas_change_heatpump
# find the change in co2 due to electricity usage using carbon intensity
whole_df$Electricity_carbon_change_heatpump <- whole_df$Carbon_intensity_electricity_heatpump * electricity_change_heatpump
# find overall change in co2 due to heat pumps 
whole_df$Carbon_quantity_change_heatpump <- whole_df$Gas_carbon_change_heatpump + whole_df$Electricity_carbon_change_heatpump
# convert to a monetary value using SCC
whole_df$Carbon_change_usa_heatpump <- whole_df$Carbon_quantity_change_heatpump * whole_df$Social_cost_carbon_usa_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)
whole_df$Carbon_change_uk_heatpump <- whole_df$Carbon_quantity_change_heatpump * whole_df$Social_cost_carbon_uk_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)

# ------------ air quality wtp ----------------
# find the change in air quality due to gas usage 
whole_df$Gas_airquality_change_heatpump <- whole_df$Gas_air_quality * gas_change_heatpump
# find the change in air quality due to electricity usage
whole_df$Electricity_airquality_change_heatpump <- whole_df$Elec_air_quality * electricity_change_heatpump
# find overall change in air quality due to heat pumps 
whole_df$Airquality_change_heatpump <- (whole_df$Electricity_airquality_change_heatpump + whole_df$Gas_airquality_change_heatpump)


# --------- conditional uk/usa social cost of carbon total ---------
whole_df <- whole_df %>%
  mutate(Environmental_wtp_heatpump = case_when(
      social_cost_of_carbon == "uk"  ~ Airquality_change_heatpump + Carbon_change_uk_heatpump,
      social_cost_of_carbon == "usa" ~ Airquality_change_heatpump + Carbon_change_usa_heatpump  ))

# Apply the discounting formula
whole_df <- whole_df %>%
  mutate(Discounted_environmental_wtp_heatpump = Environmental_wtp_heatpump / (1 + discount_rate)^(Year - base_year), )

whole_df <- whole_df %>%
  mutate(Discounted_airquality_change_heatpump = Airquality_change_heatpump / (1 + discount_rate)^(Year - base_year), )

whole_df <- whole_df %>%
  mutate(Discounted_carbon_change_uk_heatpump = Carbon_change_uk_heatpump / (1 + discount_rate)^(Year - base_year), )


# -------------------------------------- fiscal externality ---------------------------------------------------

# ------------ co2 externality ------------
# to get the global fiscal externality of carbon emissions, we multiply uk_gdp_as_proportion_of_global by 
# uk_tax_as_proportion_of_gdp since the UK government will only bear this cost as part of the social cost of carbon
whole_df <- whole_df %>%
  mutate(Environmental_gov_rev_heatpump = case_when(
    social_cost_of_carbon == "uk"  ~ uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp * Carbon_change_uk_heatpump,
    social_cost_of_carbon == "usa" ~ uk_gdp_as_proportion_of_global *  uk_tax_as_proportion_of_gdp * Carbon_change_usa_heatpump  ))
whole_df <- whole_df %>%
  mutate(Discounted_environmental_gov_rev_heatpump = Environmental_gov_rev_heatpump / (1 + discount_rate)^(Year - base_year), )

# ------------ vat externality ------------
# 5% * (electricity unit rate * electricity change + gas unit rate * gas change) 
whole_df$Vat_elec_gas_change_heatpump <- 0.05 * (whole_df$Electricity_prices * electricity_change_heatpump + whole_df$Gas_prices * gas_change_heatpump)
whole_df <- whole_df %>%
  mutate(Discounted_vat_elec_gas_change_heatpump = Vat_elec_gas_change_heatpump / (1 + discount_rate)^(Year - base_year), )

# 20% tax on boilers. 0% tax on heat pumps
# buy one less boiler and one more heat pump - assume one time loss since the lifetime of these is about 20 years
Vat_boiler_change_heatpump <- -0.2 * implied_before_tax_cost_boiler

# ----------- trading permit externality -----
# there won't be one since permits get sold anyway and so the government still earns the same (heat pump might affect the demand for electricity
# and therefore the demand for permits but this effect will be very small)

# ____________________________________________________ cosy _____________________________________________________

# ---------------------- individual wtp for cosy -------------------

# ------ initial payments with cosy if behavior unchanged
# price difference * quantity change in consumption in that period * 365 days
morning_time_payment_cosy <- (morning_time_price_cosy - other_time_price_cosy) * morning_time_initial_use_cosy * morning_time_length_cosy * 365
afternoon_time_payment_cosy <- (afternoon_time_price_cosy - other_time_price_cosy) * afternoon_time_initial_use_cosy * afternoon_time_length_cosy * 365
peak_time_payment_cosy <- (peak_time_price_cosy - other_time_price_cosy) * peak_time_initial_use_cosy * peak_time_length_cosy * 365
# since price doesn't change for consumption during these periods
other_time_payment_cosy <- (other_time_price_cosy - other_time_price_cosy) * other_time_initial_use_cosy * other_time_length_cosy * 365
tot <- other_time_payment_cosy + peak_time_payment_cosy + afternoon_time_payment_cosy + morning_time_payment_cosy

# ------ change payments with cosy since people change when they consume
# price difference * quantity change in consumption in that period * 365 days
morning_time_payment_change_cosy <- morning_time_price_cosy * morning_time_change_cosy * morning_time_length_cosy * 365
afternoon_time_payment_change_cosy <- afternoon_time_price_cosy * afternoon_time_change_cosy * afternoon_time_length_cosy * 365
peak_time_payment_change_cosy <- peak_time_price_cosy * peak_time_change_cosy * peak_time_length_cosy * 365
other_time_payment_change_cosy <- other_time_price_cosy * other_time_change_cosy * other_time_length_cosy * 365

# ------ change payments with cosy 
total_change_payment_cosy <-    morning_time_payment_change_cosy + afternoon_time_payment_change_cosy + 
  peak_time_payment_change_cosy + other_time_payment_change_cosy + morning_time_payment_cosy + 
  afternoon_time_payment_cosy + peak_time_payment_cosy + other_time_payment_cosy

# Apply the discounting formula
whole_df <- whole_df %>% mutate(Discounted_total_change_payment_cosy = total_change_payment_cosy / (1 + discount_rate)^(Year - base_year), )

# ----- change electricity vat with cosy
# 5% * (electricity unit rate * electricity change
morning_time_vat_change_cosy <- 0.05 * morning_time_price_cosy * morning_time_change_cosy * morning_time_length_cosy * 365
afternoon_time_vat_change_cosy <- 0.05 * afternoon_time_price_cosy * afternoon_time_change_cosy * afternoon_time_length_cosy * 365
peak_time_vat_change_cosy <- 0.05 * peak_time_price_cosy * peak_time_change_cosy * peak_time_length_cosy * 365
other_time_vat_change_cosy <- 0.05 * other_time_price_cosy * other_time_change_cosy * other_time_length_cosy * 365
total_vat_change_cosy <- morning_time_vat_change_cosy + afternoon_time_vat_change_cosy + peak_time_vat_change_cosy + other_time_vat_change_cosy

# Apply the discounting formula
whole_df <- whole_df %>% mutate(Discounted_total_vat_change_cosy = total_vat_change_cosy / (1 + discount_rate)^(Year - base_year), )


########### ------------ cosy environmental wtp --------------------------

# change in electricity every year
change_electricity_morning_cosy <- morning_time_change_cosy * morning_time_length_cosy * 365
change_electricity_afternoon_cosy <- afternoon_time_change_cosy * afternoon_time_length_cosy * 365
change_electricity_peak_cosy <- peak_time_change_cosy * peak_time_length_cosy * 365
change_electricity_other_cosy <- other_time_change_cosy * other_time_length_cosy * 365

# convert to change in co2 tonnes using carbon intensities
whole_df$Change_carbon_morning_cosy <- change_electricity_morning_cosy * whole_df$Carbon_intensity_off_peak_morning_cosy /1000
whole_df$Change_carbon_afternoon_cosy <- change_electricity_afternoon_cosy * whole_df$Carbon_intensity_off_peak_afternoon_cosy /1000
whole_df$Change_carbon_peak_cosy <- change_electricity_peak_cosy * whole_df$Carbon_intensity_peak_cosy /1000
whole_df$Change_carbon_other_cosy <- change_electricity_other_cosy * whole_df$Carbon_intensity_normal_rate_cosy /1000

whole_df$Total_carbon_change_cosy <- whole_df$Change_carbon_morning_cosy +whole_df$Change_carbon_afternoon_cosy +
                                      whole_df$Change_carbon_peak_cosy + whole_df$Change_carbon_other_cosy 
# convert to a monetary value using SCC
whole_df$Total_carbon_change_usa_cosy <- whole_df$Total_carbon_change_cosy * whole_df$Social_cost_carbon_usa_gov  * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)
whole_df$Total_carbon_change_uk_cosy <- whole_df$Total_carbon_change_cosy * whole_df$Social_cost_carbon_uk_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)

# --------- conditional uk/usa social cost of carbon total ---------
whole_df <- whole_df %>%
  mutate(Environmental_wtp_cosy = case_when(
    social_cost_of_carbon == "uk"  ~ Total_carbon_change_uk_cosy,
    social_cost_of_carbon == "usa" ~ Total_carbon_change_usa_cosy  ))
# Apply the discounting formula
whole_df <- whole_df %>% mutate(Discounted_environmental_wtp_cosy = Environmental_wtp_cosy / (1 + discount_rate)^(Year - base_year), )


# -------------------------------------- fiscal externality cosy ----------------------------------------------

# long term gov revenue change
whole_df$Discounted_environmental_gov_wtp_cosy <- whole_df$Discounted_environmental_wtp_cosy * uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp


############################## Final MVPF Calculations ###################################################

# ------------------------ heatpump ----------------------------

# total wtp by consumers
Final_consumer_wtp_heatpump <- marginal_wtp_heatpump + inframarginal_wtp_heatpump
# environmental wtp - sum over 20 years - multiply by -1 since it is a benefit 
Discounted_environmental_wtp_heatpump <- sum(whole_df$Discounted_environmental_wtp_heatpump) * -1
Final_environmental_wtp_heatpump <- Discounted_environmental_wtp_heatpump * percent_marginal_consumers 
# total wtp 
Numerator <- Final_environmental_wtp_heatpump + Final_consumer_wtp_heatpump


# gov transfer = price of subsidy
Final_gov_transfer <- gov_subsidy_heatpump
# vat change from change in gas and electricity use
Discounted_vat_elec_gas_change_heatpump <- sum(whole_df$Discounted_vat_elec_gas_change_heatpump)
# vat change from boiler and heatpump
Final_vat_change_heatpump <- (Discounted_vat_elec_gas_change_heatpump + Vat_boiler_change_heatpump) * percent_marginal_consumers
# gov revenue change due to co2 effects long term
Discounted_environmental_gov_rev_heatpump <- sum(whole_df$Discounted_environmental_gov_rev_heatpump) * -1
Final_environmental_gov_rev_heatpump <- Discounted_environmental_gov_rev_heatpump * percent_marginal_consumers
# total fiscal cost
Demominator <- Final_gov_transfer - Final_vat_change_heatpump - Final_environmental_gov_rev_heatpump

# MVPF heatpump
MVPF <- Numerator / Demominator

MVPF_with_LBD <- (Numerator + lbd_environmental_heatpump + lbd_price_heatpump) / Demominator



# ------------------------ cosy and heatpump ----------------------------

# environmental wtp - sum over 20 years 
Discounted_environmental_wtp_cosy <- sum(whole_df$Discounted_environmental_wtp_cosy) * -1
Final_environmental_wtp_cosy <- (Discounted_environmental_wtp_cosy + Discounted_environmental_wtp_heatpump) * percent_marginal_consumers
# total wtp 
Numerator_cosy <- Final_environmental_wtp_cosy + Final_consumer_wtp_heatpump

# most fiscal externalities same as before
Discounted_environmental_gov_wtp_cosy <- sum(whole_df$Discounted_environmental_gov_wtp_cosy) * -1
Discounted_total_vat_change_cosy <- sum(whole_df$Discounted_total_vat_change_cosy) 
Final_environmental_gov_rev_cosy <- (Discounted_environmental_gov_rev_heatpump + Discounted_environmental_gov_wtp_cosy + Discounted_total_vat_change_cosy) * percent_marginal_consumers
# total fiscal cost
Demominator_cosy <- Final_gov_transfer - Final_vat_change_heatpump - Final_environmental_gov_rev_cosy

# MVPF heatpump + cosy
MVPF_cosy <- Numerator_cosy / Demominator_cosy


# _________________________________ social and government cost per tonne ___________________________________________________

# ---------------- heatpump -----------

# direct change of energy payments due to change in gas and electricity usage from heatpump
Discounted_change_energy_payments <- sum(whole_df$Discounted_change_energy_payments)
# change in consumer payments if consumers buy heatpumps and don't buy boilers
private_cost_heatpump_not_boiler <- private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments

# change in air quality effect
Discounted_airquality_change_heatpump <- sum(whole_df$Discounted_airquality_change_heatpump) * -1
Final_airquality_change_heatpump <- Discounted_airquality_change_heatpump * percent_marginal_consumers

# cost to consumers, societal cost of air pollution, fiscal revenue change
social_cost_heatpump <- private_cost_heatpump_not_boiler - Final_airquality_change_heatpump - Final_vat_change_heatpump - Final_environmental_gov_rev_heatpump

# divide it by co2 tonnes abated
Carbon_tonnes_abated <- sum(whole_df$Carbon_quantity_change_heatpump) * -1 
social_cost_per_tonne_heatpump <-  social_cost_heatpump / Carbon_tonnes_abated
 
# government cost  
government_cost_heatpump <- Final_gov_transfer - Final_vat_change_heatpump - Final_environmental_gov_rev_heatpump
government_cost_per_tonne_heatpump <- government_cost_heatpump / (Carbon_tonnes_abated * percent_marginal_consumers)

# resource cost  
resource_cost_heatpump <- private_cost_heatpump_not_boiler
resource_cost_per_tonne_heatpump <- resource_cost_heatpump / Carbon_tonnes_abated

# ------------------ cosy --------------

# direct change in energy payments if they have cosy
Discounted_total_change_payment_cosy <- sum(whole_df$Discounted_total_change_payment_cosy)
Final_total_change_payment_cosy <- Discounted_total_change_payment_cosy * percent_marginal_consumers

# cost to consumers of having a heatpump and not a boiler and also being on the cosy tariff
private_cost_cosy <- private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments + Discounted_total_change_payment_cosy

# cost to consumers, societal cost of air pollution, fiscal revenue change
social_cost_cosy <- private_cost_cosy - Final_airquality_change_heatpump - Final_vat_change_heatpump - Final_environmental_gov_rev_cosy
# divide it by co2 tonnes abated
social_cost_per_tonne_cosy <- social_cost_cosy / Carbon_tonnes_abated
  
# government cost  
government_cost_cosy <- Final_gov_transfer - Final_vat_change_heatpump - Final_environmental_gov_rev_cosy
government_cost_per_tonne_cosy <- government_cost_heatpump / (Carbon_tonnes_abated * percent_marginal_consumers)

resource_cost_cosy <- private_cost_cosy
resource_cost_per_tonne_cosy <- resource_cost_cosy / Carbon_tonnes_abated 

# _________________________________ marginal MVPF ___________________________________________________


consumer_transfer <- 1
gov_spending <- 1
elasticity <- 1.2
Numerator_first_pound <- consumer_transfer + (Discounted_environmental_wtp_heatpump*elasticity/total_installation_cost_heatpump)
Denominator_first_pound <- gov_spending + ((Discounted_environmental_gov_rev_heatpump - Discounted_vat_elec_gas_change_heatpump - Vat_boiler_change_heatpump)*elasticity/total_installation_cost_heatpump)
  
MVPF_first_pound <- Numerator_first_pound/Denominator_first_pound

Numerator_first_pound_with_LBD <- consumer_transfer + ((Discounted_environmental_wtp_heatpump + lbd_environmental_heatpump + lbd_price_heatpump)*elasticity/total_installation_cost_heatpump)
Denominator_first_pound_with_LBD <- gov_spending + ((Discounted_environmental_gov_rev_heatpump - Discounted_vat_elec_gas_change_heatpump - Vat_boiler_change_heatpump)*elasticity/total_installation_cost_heatpump)

MVPF_first_pound_with_LBD <- Numerator_first_pound_with_LBD / Denominator_first_pound_with_LBD


Discounted_environmental_wtp_cosy + Discounted_environmental_wtp_heatpump
# total wtp 
Numerator_cosy <- Final_environmental_wtp_cosy + Final_consumer_wtp_heatpump

# most fiscal externalities same as before
Discounted_environmental_gov_rev_heatpump + Discounted_environmental_gov_wtp_cosy + Discounted_total_vat_change_cosy
# total fiscal cost
Demominator_cosy <- Final_gov_transfer - Final_vat_change_heatpump - Final_environmental_gov_rev_cosy

# MVPF heatpump + cosy
MVPF_cosy <- Numerator_cosy / Demominator_cosy


Numerator_cosy_first_pound <- consumer_transfer + ((Discounted_environmental_wtp_heatpump + Discounted_environmental_wtp_cosy) * elasticity/total_installation_cost_heatpump)
Denominator_cosy_first_pound <- gov_spending + ((Discounted_environmental_gov_rev_heatpump + Discounted_environmental_gov_wtp_cosy + Discounted_total_vat_change_cosy) * elasticity/total_installation_cost_heatpump)

MVPF_with_cosy_first_pound <- Numerator_cosy_first_pound/Denominator_cosy_first_pound

Numerator_cosy_first_pound_with_LBD <- consumer_transfer + ((Discounted_environmental_wtp_heatpump + Discounted_environmental_wtp_cosy  + lbd_environmental_heatpump + lbd_price_heatpump) * elasticity/total_installation_cost_heatpump)
Denominator_cosy_first_pound_with_LBD <- gov_spending + ((Discounted_environmental_gov_rev_heatpump + Discounted_environmental_gov_wtp_cosy + Discounted_total_vat_change_cosy) * elasticity/total_installation_cost_heatpump)

MVPF_with_cosy_first_pound_with_LBD <- Numerator_cosy_first_pound_with_LBD / Denominator_cosy_first_pound_with_LBD


################################### plot graph ##########################################


# Data 
categories <- factor(c('Transfers', 'CO2 benefits', 'Air pollution', 'Total benefits', 
                       'Subsidy cost', 'Lost VAT (Boiler Purchase)', 'Extra VAT (Energy)', 
                       'Climate change FE', 'Total Govt Cost'),
                     levels = c('Transfers', 'CO2 benefits', 'Air pollution', 'Total benefits', 
                                'Subsidy cost', 'Lost VAT (Boiler Purchase)', 'Extra VAT (Energy)', 
                                'Climate change FE', 'Total Govt Cost'))



# standardize all values so it is per 1 of subsidy spending
consumer_transfer_heatpump_graph <- Final_consumer_wtp_heatpump / gov_subsidy_heatpump

Discounted_carbon_change_uk_heatpump <- sum(whole_df$Discounted_carbon_change_uk_heatpump) * -1
Final_discounted_carbon_change_uk_heatpump <- Discounted_carbon_change_uk_heatpump * percent_marginal_consumers 
carbon_benefits_consumers_graph <- Final_discounted_carbon_change_uk_heatpump / gov_subsidy_heatpump

Discounted_airquality_change_heatpump <- sum(whole_df$Discounted_airquality_change_heatpump) * -1
Final_discounted_airquality_change_heatpump <- Discounted_airquality_change_heatpump * percent_marginal_consumers 
air_pollution_consumers_graph <- Final_discounted_airquality_change_heatpump / gov_subsidy_heatpump

total_benefits_graph <- consumer_transfer_heatpump_graph + carbon_benefits_consumers_graph + air_pollution_consumers_graph

subsidy_cost_graph <- Final_gov_transfer / gov_subsidy_heatpump 

energy_vat_graph <- Discounted_vat_elec_gas_change_heatpump * percent_marginal_consumers  * -1  / gov_subsidy_heatpump

heatpump_vat_graph <- Vat_boiler_change_heatpump * percent_marginal_consumers * -1 / gov_subsidy_heatpump

carbon_gov_graph <- Final_environmental_gov_rev_heatpump  * -1  / gov_subsidy_heatpump

total_cost_gov_graph <- subsidy_cost_graph + heatpump_vat_graph + energy_vat_graph + carbon_gov_graph


# Values for the bars 
values <- c(consumer_transfer_heatpump_graph, carbon_benefits_consumers_graph, air_pollution_consumers_graph, total_benefits_graph, subsidy_cost_graph, heatpump_vat_graph, energy_vat_graph, carbon_gov_graph, total_cost_gov_graph)

# Colors for each category
colors <- c('#87B6F8', '#87B6F8', '#87B6F8', '#2E354A', '#D5AFF2', '#D5AFF2', '#D5AFF2', '#D5AFF2', '#4B2C6F')

# Initialize ymin and ymax with updated values 
ymin <- c(0, consumer_transfer_heatpump_graph, consumer_transfer_heatpump_graph + carbon_benefits_consumers_graph, 0, 0, subsidy_cost_graph, subsidy_cost_graph + heatpump_vat_graph, subsidy_cost_graph + heatpump_vat_graph - energy_vat_graph, 0)
ymax <- c(consumer_transfer_heatpump_graph, consumer_transfer_heatpump_graph + carbon_benefits_consumers_graph, consumer_transfer_heatpump_graph + carbon_benefits_consumers_graph + air_pollution_consumers_graph, total_benefits_graph, subsidy_cost_graph, subsidy_cost_graph + heatpump_vat_graph, subsidy_cost_graph + heatpump_vat_graph - energy_vat_graph, subsidy_cost_graph + heatpump_vat_graph - energy_vat_graph - carbon_gov_graph, total_cost_gov_graph)

# Create a data frame
data <- data.frame(categories, values, colors, ymin, ymax)

# Custom labels with CO2 subscript
custom_labels <- c('Transfers', 
                   expression(CO[2]~benefits), 
                   'Air pollution', 
                   'Total benefits', 
                   'Subsidy cost', 
                   'Lost VAT\n(Boiler Purchase)', 
                   'Extra VAT\n(Energy)', 
                   'Climate change FE', 
                   'Total Govt Cost')

# Create the waterfall chart
p <- ggplot(data) +
  geom_rect(aes(xmin = as.numeric(categories) - 0.4, xmax = as.numeric(categories) + 0.4, ymin = ymin, ymax = ymax, fill = colors)) +
  scale_fill_identity() +
  geom_text(aes(x = categories, y = ymax + 0.02, label = round(values, 3)), vjust = -0.3) +  # Adjust text positioning
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 0.5),  # Center the £ vertically
        plot.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
        panel.background = element_rect(fill = 'transparent', color = NA),
        plot.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "£") +  # Adding £ as the y-axis title
  scale_x_discrete(labels = custom_labels) +  # Use custom labels
  coord_cartesian(ylim = c(0, max(ymax) + 0.02))  # Adjust y-axis limits to provide more space at the top


# Save the plot as a PNG file
ggsave("MVPF_HP_Cosy_updated.png", plot = p, width = 10, height = 6, bg = 'transparent')




# ____________________________________ MVPF by temperature _______________________________________________

# --------- change in electricity and gas usage by temperature -------------

df_temp = read.csv(file.path(datapath, "scratch/gas_electricity_by_temperature.csv"))
df_temp <- df_temp %>%
  select(daily_avg_air_temperature_celsius, lhs, Estimate, Std..Error) %>%
  pivot_wider(
    names_from = lhs,
    values_from = c(Estimate, Std..Error)  )


df_temp <- df_temp %>%
  filter(daily_avg_air_temperature_celsius >= 0, daily_avg_air_temperature_celsius <= 21) %>%
  rename(
    temp = daily_avg_air_temperature_celsius,
    electricity_change_heatpump = Estimate_elec_consumption,
    gas_change_heatpump = Estimate_gas_consumption,
    electricity_change_heatpump_se = Std..Error_elec_consumption,
    gas_change_heatpump_se = Std..Error_gas_consumption)

# Compute 95% confidence intervals for annual changes
df_temp <- df_temp %>%
  mutate(
    gas_change_heatpump_lower_bound = (gas_change_heatpump + 1.96 * gas_change_heatpump_se) * 52,
    gas_change_heatpump_upper_bound = (gas_change_heatpump - 1.96 * gas_change_heatpump_se) * 52,
    electricity_change_heatpump_lower_bound = (electricity_change_heatpump + 1.96 * electricity_change_heatpump_se) * 52,
    electricity_change_heatpump_upper_bound = (electricity_change_heatpump - 1.96 * electricity_change_heatpump_se) * 52,
    
    # Also multiply point estimates by 52 to make them annual
    gas_change_heatpump = gas_change_heatpump * 52,
    electricity_change_heatpump = electricity_change_heatpump * 52
  )


# gives 1.27
#df_temp$electricity_change_heatpump <- 3080.0
#df_temp$gas_change_heatpump <- -9350.7
whole_df_temp <- merge(whole_df, df_temp, by = NULL)

# ---------------------- wtp ------------------------------------------
# __________  using hmg prices ___________________
whole_df_temp$Gas_price_change_heatpump <- whole_df_temp$Gas_prices * whole_df_temp$gas_change_heatpump
whole_df_temp$Electricity_price_change_heatpump <- whole_df_temp$Electricity_prices * whole_df_temp$electricity_change_heatpump
# if they continue having gas at home in some capacity
whole_df_temp$Change_payments_hmg <- whole_df_temp$Electricity_price_change_heatpump + whole_df_temp$Gas_price_change_heatpump 
# if they cut off their gas connection entirely
whole_df_temp$Change_payments_hmg_no_standing <- whole_df_temp$Change_payments_hmg - gas_standing_charge_octopus

whole_df_temp <- whole_df_temp %>%
  mutate(Change_energy_payments = case_when(
    energy_prices == "hmg" & gas_connection == "yes" ~ Change_payments_hmg,
    energy_prices == "hmg" & gas_connection == "no" ~ Change_payments_hmg_no_standing ))

# Apply the discounting formula
whole_df_temp <- whole_df_temp %>%
  mutate(Discounted_change_energy_payments = Change_energy_payments / (1 + discount_rate)^(Year - base_year), )


# ---------------------- environmental wtp for heat pump -------------------

# ------------ carbon wtp ----------------
# find the change in co2 due to gas usage using carbon intensity
whole_df_temp$Gas_carbon_change_heatpump <- whole_df_temp$Carbon_intensity_gas_heatpump * whole_df_temp$gas_change_heatpump
# find the change in co2 due to electricity usage using carbon intensity
whole_df_temp$Electricity_carbon_change_heatpump <- whole_df_temp$Carbon_intensity_electricity_heatpump * whole_df_temp$electricity_change_heatpump
# find overall change in co2 due to heat pumps 
whole_df_temp$Carbon_quantity_change_heatpump <- whole_df_temp$Gas_carbon_change_heatpump + whole_df_temp$Electricity_carbon_change_heatpump
# convert to a monetary value using SCC
whole_df_temp$Carbon_change_usa_heatpump <- whole_df_temp$Carbon_quantity_change_heatpump * whole_df_temp$Social_cost_carbon_usa_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)
whole_df_temp$Carbon_change_uk_heatpump <- whole_df_temp$Carbon_quantity_change_heatpump * whole_df_temp$Social_cost_carbon_uk_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)

# ------------ air quality wtp ----------------
# find the change in air quality due to gas usage 
whole_df_temp$Gas_airquality_change_heatpump <- whole_df_temp$Gas_air_quality * whole_df_temp$gas_change_heatpump
# find the change in air quality due to electricity usage
whole_df_temp$Electricity_airquality_change_heatpump <- whole_df_temp$Elec_air_quality * whole_df_temp$electricity_change_heatpump
# find overall change in air quality due to heat pumps 
whole_df_temp$Airquality_change_heatpump <- (whole_df_temp$Electricity_airquality_change_heatpump + whole_df_temp$Gas_airquality_change_heatpump)


# --------- conditional uk/usa social cost of carbon total ---------
whole_df_temp <- whole_df_temp %>%
  mutate(Environmental_wtp_heatpump = case_when(
    social_cost_of_carbon == "uk"  ~ Airquality_change_heatpump + Carbon_change_uk_heatpump,
    social_cost_of_carbon == "usa" ~ Airquality_change_heatpump + Carbon_change_usa_heatpump  ))

# Apply the discounting formula
whole_df_temp <- whole_df_temp %>%
  mutate(Discounted_environmental_wtp_heatpump = Environmental_wtp_heatpump / (1 + discount_rate)^(Year - base_year), )

whole_df_temp <- whole_df_temp %>%
  mutate(Discounted_airquality_change_heatpump = Airquality_change_heatpump / (1 + discount_rate)^(Year - base_year), )

whole_df_temp <- whole_df_temp %>%
  mutate(Discounted_carbon_change_uk_heatpump = Carbon_change_uk_heatpump / (1 + discount_rate)^(Year - base_year), )


# -------------------------------------- fiscal externality ---------------------------------------------------

# ------------ co2 externality ------------
# to get the global fiscal externality of carbon emissions, we multiply uk_gdp_as_proportion_of_global by 
# uk_tax_as_proportion_of_gdp since the UK government will only bear this cost as part of the social cost of carbon
whole_df_temp <- whole_df_temp %>%
  mutate(Environmental_gov_rev_heatpump = case_when(
    social_cost_of_carbon == "uk"  ~ uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp * Carbon_change_uk_heatpump,
    social_cost_of_carbon == "usa" ~ uk_gdp_as_proportion_of_global *  uk_tax_as_proportion_of_gdp * Carbon_change_usa_heatpump  ))
whole_df_temp <- whole_df_temp %>%
  mutate(Discounted_environmental_gov_rev_heatpump = Environmental_gov_rev_heatpump / (1 + discount_rate)^(Year - base_year), )

# ------------ vat externality ------------
# 5% * (electricity unit rate * electricity change + gas unit rate * gas change) 
whole_df_temp$Vat_elec_gas_change_heatpump <- 0.05 * (whole_df_temp$Electricity_prices * whole_df_temp$electricity_change_heatpump + whole_df_temp$Gas_prices * whole_df_temp$gas_change_heatpump)
whole_df_temp <- whole_df_temp %>%
  mutate(Discounted_vat_elec_gas_change_heatpump = Vat_elec_gas_change_heatpump / (1 + discount_rate)^(Year - base_year), )

# 20% tax on boilers. 0% tax on heat pumps
# buy one less boiler and one more heat pump - assume one time loss since the lifetime of these is about 20 years
Vat_boiler_change_heatpump <- -0.2 * implied_before_tax_cost_boiler


sum <- whole_df_temp %>%
  group_by(temp) %>%
  summarise(
    mean_change_elec = mean(Electricity_price_change_heatpump),
    mean_gas_price_change = mean(Gas_price_change_heatpump)
  )



############################ for the bounds ##################################
# -- PRICE CHANGE using bounds --
whole_df_temp$Gas_price_change_lower <- whole_df_temp$Gas_prices * whole_df_temp$gas_change_heatpump_lower_bound
whole_df_temp$Gas_price_change_upper <- whole_df_temp$Gas_prices * whole_df_temp$gas_change_heatpump_upper_bound

whole_df_temp$Elec_price_change_lower <- whole_df_temp$Electricity_prices * whole_df_temp$electricity_change_heatpump_lower_bound
whole_df_temp$Elec_price_change_upper <- whole_df_temp$Electricity_prices * whole_df_temp$electricity_change_heatpump_upper_bound

# -- COMBINED PAYMENTS --
whole_df_temp$Change_payments_lower <- whole_df_temp$Gas_price_change_lower + whole_df_temp$Elec_price_change_lower
whole_df_temp$Change_payments_upper <- whole_df_temp$Gas_price_change_upper + whole_df_temp$Elec_price_change_upper

whole_df_temp$Change_payments_lower_no_standing <- whole_df_temp$Change_payments_lower - gas_standing_charge_octopus
whole_df_temp$Change_payments_upper_no_standing <- whole_df_temp$Change_payments_upper - gas_standing_charge_octopus

# -- CHOOSE ACCORDING TO GAS CONNECTION --
whole_df_temp <- whole_df_temp %>%
  mutate(
    Change_energy_payments_lower = case_when(
      energy_prices == "hmg" & gas_connection == "yes" ~ Change_payments_lower,
      energy_prices == "hmg" & gas_connection == "no" ~ Change_payments_lower_no_standing
    ),
    Change_energy_payments_upper = case_when(
      energy_prices == "hmg" & gas_connection == "yes" ~ Change_payments_upper,
      energy_prices == "hmg" & gas_connection == "no" ~ Change_payments_upper_no_standing
    ),
    Discounted_change_energy_payments_lower = Change_energy_payments_lower / (1 + discount_rate)^(Year - base_year),
    Discounted_change_energy_payments_upper = Change_energy_payments_upper / (1 + discount_rate)^(Year - base_year)
  )
# -- CARBON CHANGE using bounds --
whole_df_temp$Carbon_gas_lower <- whole_df_temp$Carbon_intensity_gas_heatpump * whole_df_temp$gas_change_heatpump_lower_bound
whole_df_temp$Carbon_gas_upper <- whole_df_temp$Carbon_intensity_gas_heatpump * whole_df_temp$gas_change_heatpump_upper_bound

whole_df_temp$Carbon_elec_lower <- whole_df_temp$Carbon_intensity_electricity_heatpump * whole_df_temp$electricity_change_heatpump_lower_bound
whole_df_temp$Carbon_elec_upper <- whole_df_temp$Carbon_intensity_electricity_heatpump * whole_df_temp$electricity_change_heatpump_upper_bound

whole_df_temp$Carbon_total_lower <- whole_df_temp$Carbon_gas_lower + whole_df_temp$Carbon_elec_lower
whole_df_temp$Carbon_total_upper <- whole_df_temp$Carbon_gas_upper + whole_df_temp$Carbon_elec_upper

# -- CARBON MONETARY VALUE --
whole_df_temp$Carbon_change_uk_lower <- whole_df_temp$Carbon_total_lower * whole_df_temp$Social_cost_carbon_uk_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)
whole_df_temp$Carbon_change_uk_upper <- whole_df_temp$Carbon_total_upper * whole_df_temp$Social_cost_carbon_uk_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)

whole_df_temp$Carbon_change_usa_lower <- whole_df_temp$Carbon_total_lower * whole_df_temp$Social_cost_carbon_usa_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)
whole_df_temp$Carbon_change_usa_upper <- whole_df_temp$Carbon_total_upper * whole_df_temp$Social_cost_carbon_usa_gov * (1 - uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp)

# -- AIR QUALITY using bounds --
whole_df_temp$Airquality_gas_lower <- whole_df_temp$Gas_air_quality * whole_df_temp$gas_change_heatpump_lower_bound
whole_df_temp$Airquality_gas_upper <- whole_df_temp$Gas_air_quality * whole_df_temp$gas_change_heatpump_upper_bound

whole_df_temp$Airquality_elec_lower <- whole_df_temp$Elec_air_quality * whole_df_temp$electricity_change_heatpump_lower_bound
whole_df_temp$Airquality_elec_upper <- whole_df_temp$Elec_air_quality * whole_df_temp$electricity_change_heatpump_upper_bound

whole_df_temp$Airquality_total_lower <- whole_df_temp$Airquality_gas_lower + whole_df_temp$Airquality_elec_lower
whole_df_temp$Airquality_total_upper <- whole_df_temp$Airquality_gas_upper + whole_df_temp$Airquality_elec_upper

# -- TOTAL ENVIRONMENTAL WTP --
whole_df_temp <- whole_df_temp %>%
  mutate(
    Environmental_wtp_lower = case_when(
      social_cost_of_carbon == "uk" ~ Airquality_total_lower + Carbon_change_uk_lower,
      social_cost_of_carbon == "usa" ~ Airquality_total_lower + Carbon_change_usa_lower
    ),
    Environmental_wtp_upper = case_when(
      social_cost_of_carbon == "uk" ~ Airquality_total_upper + Carbon_change_uk_upper,
      social_cost_of_carbon == "usa" ~ Airquality_total_upper + Carbon_change_usa_upper
    ),
    Discounted_environmental_wtp_lower = Environmental_wtp_lower / (1 + discount_rate)^(Year - base_year),
    Discounted_environmental_wtp_upper = Environmental_wtp_upper / (1 + discount_rate)^(Year - base_year),
    
    Discounted_airquality_change_lower = Airquality_total_lower / (1 + discount_rate)^(Year - base_year), 
   Discounted_airquality_change_upper = Airquality_total_upper / (1 + discount_rate)^(Year - base_year) )
   



# -- GOV REVENUE from CO2 --
whole_df_temp <- whole_df_temp %>%
  mutate(
    Environmental_gov_rev_lower = case_when(
      social_cost_of_carbon == "uk" ~ uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp * Carbon_change_uk_lower,
      social_cost_of_carbon == "usa" ~ uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp * Carbon_change_usa_lower
    ),
    Environmental_gov_rev_upper = case_when(
      social_cost_of_carbon == "uk" ~ uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp * Carbon_change_uk_upper,
      social_cost_of_carbon == "usa" ~ uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp * Carbon_change_usa_upper
    ),
    Discounted_environmental_gov_rev_lower = Environmental_gov_rev_lower / (1 + discount_rate)^(Year - base_year),
    Discounted_environmental_gov_rev_upper = Environmental_gov_rev_upper / (1 + discount_rate)^(Year - base_year)
  )

# -- VAT change --
whole_df_temp$Vat_change_lower <- 0.05 * (whole_df_temp$Electricity_prices * whole_df_temp$electricity_change_heatpump_lower_bound + whole_df_temp$Gas_prices * whole_df_temp$gas_change_heatpump_lower_bound)
whole_df_temp$Vat_change_upper <- 0.05 * (whole_df_temp$Electricity_prices * whole_df_temp$electricity_change_heatpump_upper_bound + whole_df_temp$Gas_prices * whole_df_temp$gas_change_heatpump_upper_bound)

whole_df_temp <- whole_df_temp %>%
  mutate(
    Discounted_vat_lower = Vat_change_lower / (1 + discount_rate)^(Year - base_year),
    Discounted_vat_upper = Vat_change_upper / (1 + discount_rate)^(Year - base_year)
  )


# ------------------------ heatpump ----------------------------

# ---- Summarise by temperature ----
MVPF_by_temp <- whole_df_temp %>%
  group_by(temp) %>%
  summarise(
    Discounted_environmental_wtp_heatpump = sum(Discounted_environmental_wtp_heatpump) * -1 * percent_marginal_consumers,
    Final_consumer_wtp_heatpump = marginal_wtp_heatpump + inframarginal_wtp_heatpump,
    Numerator = Discounted_environmental_wtp_heatpump + Final_consumer_wtp_heatpump,
    Discounted_vat_elec_gas_change_heatpump = sum(Discounted_vat_elec_gas_change_heatpump),
    Final_vat_change_heatpump = (Discounted_vat_elec_gas_change_heatpump + Vat_boiler_change_heatpump) * percent_marginal_consumers,
    Discounted_environmental_gov_rev_heatpump = sum(Discounted_environmental_gov_rev_heatpump) * -1 * percent_marginal_consumers,
    Final_gov_transfer = gov_subsidy_heatpump,
    Denominator = Final_gov_transfer - Final_vat_change_heatpump - Discounted_environmental_gov_rev_heatpump,
    MVPF = Numerator / Denominator,

    Discounted_change_energy_payments = sum(Discounted_change_energy_payments),
    private_cost_heatpump_not_boiler = private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments,
    Discounted_airquality_change_heatpump = sum(Discounted_airquality_change_heatpump) * -1,
    Final_airquality_change_heatpump = Discounted_airquality_change_heatpump * percent_marginal_consumers,
    social_cost_heatpump = private_cost_heatpump_not_boiler - Final_airquality_change_heatpump - Final_vat_change_heatpump - Final_environmental_gov_rev_heatpump,
    Carbon_tonnes_abated = sum(Carbon_quantity_change_heatpump) * -1 ,
    social_cost_per_tonne_heatpump =  social_cost_heatpump / Carbon_tonnes_abated,
    government_cost_heatpump = Final_gov_transfer - Final_vat_change_heatpump - Final_environmental_gov_rev_heatpump,
    government_cost_per_tonne_heatpump = government_cost_heatpump / (Carbon_tonnes_abated * percent_marginal_consumers),
    resource_cost_heatpump = private_cost_heatpump_not_boiler,
    resource_cost_per_tonne_heatpump = resource_cost_heatpump / Carbon_tonnes_abated
  )


MVPF_by_temp <- whole_df_temp %>%
  group_by(temp) %>%
  summarise(
    Discounted_environmental_wtp_heatpump = sum(Discounted_environmental_wtp_heatpump) * -1 * percent_marginal_consumers,
    Final_consumer_wtp_heatpump = marginal_wtp_heatpump + inframarginal_wtp_heatpump,
    Numerator = Discounted_environmental_wtp_heatpump + Final_consumer_wtp_heatpump,
    Discounted_vat_elec_gas_change_heatpump = sum(Discounted_vat_elec_gas_change_heatpump),
    Final_vat_change_heatpump = (Discounted_vat_elec_gas_change_heatpump + Vat_boiler_change_heatpump) * percent_marginal_consumers,
    Discounted_environmental_gov_rev_heatpump = sum(Discounted_environmental_gov_rev_heatpump) * -1 * percent_marginal_consumers,
    Final_gov_transfer = gov_subsidy_heatpump,
    Denominator = Final_gov_transfer - Final_vat_change_heatpump - Discounted_environmental_gov_rev_heatpump,
    MVPF = Numerator / Denominator,
    
    Discounted_change_energy_payments = sum(Discounted_change_energy_payments),
    private_cost_heatpump_not_boiler = private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments,
    Discounted_airquality_change_heatpump = sum(Discounted_airquality_change_heatpump) * -1,
    Final_airquality_change_heatpump = Discounted_airquality_change_heatpump * percent_marginal_consumers,
    social_cost_heatpump = private_cost_heatpump_not_boiler - Final_airquality_change_heatpump - Final_vat_change_heatpump - Discounted_environmental_gov_rev_heatpump,
    Carbon_tonnes_abated = sum(Carbon_quantity_change_heatpump) * -1,
    social_cost_per_tonne_heatpump = social_cost_heatpump / Carbon_tonnes_abated,
    government_cost_heatpump = Final_gov_transfer - Final_vat_change_heatpump - Discounted_environmental_gov_rev_heatpump,
    government_cost_per_tonne_heatpump = government_cost_heatpump / (Carbon_tonnes_abated * percent_marginal_consumers),
    resource_cost_heatpump = private_cost_heatpump_not_boiler,
    resource_cost_per_tonne_heatpump = resource_cost_heatpump / Carbon_tonnes_abated,
    
    # -------- Lower Bound --------
    Discounted_env_wtp_lower = sum(Discounted_environmental_wtp_lower) * -1 * percent_marginal_consumers,
    Final_consumer_wtp = marginal_wtp_heatpump + inframarginal_wtp_heatpump,
    Numerator_lower = Discounted_env_wtp_lower + Final_consumer_wtp,
    Discounted_vat_lower = sum(Discounted_vat_lower),
    Final_vat_lower = (Discounted_vat_lower + Vat_boiler_change_heatpump) * percent_marginal_consumers,
    Discounted_gov_rev_lower = sum(Discounted_environmental_gov_rev_lower) * -1 * percent_marginal_consumers,
    Denominator_lower = Final_gov_transfer - Final_vat_lower - Discounted_gov_rev_lower,
    MVPF_lower = Numerator_lower / Denominator_lower,
    
    Discounted_change_energy_payments_lower = sum(Discounted_change_energy_payments_lower),
    Discounted_airquality_change_lower = sum(Discounted_airquality_change_lower) * -1,
    Final_airquality_change_lower = Discounted_airquality_change_lower * percent_marginal_consumers,
    social_cost_lower = (private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments_lower) - Final_airquality_change_lower - Final_vat_lower - Discounted_gov_rev_lower,
    social_cost_per_tonne_lower = social_cost_lower / (sum(Carbon_total_lower) * -1),
    government_cost_lower = Final_gov_transfer - Final_vat_lower - Discounted_gov_rev_lower,
    government_cost_per_tonne_lower = government_cost_lower / (sum(Carbon_total_lower) * -1 * percent_marginal_consumers),
    resource_cost_lower = private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments_lower,
    resource_cost_per_tonne_lower = resource_cost_lower / (sum(Carbon_total_lower) * -1),
    
    # -------- Upper Bound --------
    Discounted_env_wtp_upper = sum(Discounted_environmental_wtp_upper) * -1 * percent_marginal_consumers,
    Numerator_upper = Discounted_env_wtp_upper + Final_consumer_wtp,
    Discounted_vat_upper = sum(Discounted_vat_upper),
    Final_vat_upper = (Discounted_vat_upper + Vat_boiler_change_heatpump) * percent_marginal_consumers,
    Discounted_gov_rev_upper = sum(Discounted_environmental_gov_rev_upper) * -1 * percent_marginal_consumers,
    Denominator_upper = Final_gov_transfer - Final_vat_upper - Discounted_gov_rev_upper,
    MVPF_upper = Numerator_upper / Denominator_upper,
    
    Discounted_change_energy_payments_upper = sum(Discounted_change_energy_payments_upper),
    Discounted_airquality_change_upper = sum(Discounted_airquality_change_upper) * -1,
    Final_airquality_change_upper = Discounted_airquality_change_upper * percent_marginal_consumers,
    social_cost_upper = (private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments_upper) - Final_airquality_change_upper - Final_vat_upper - Discounted_gov_rev_upper,
    social_cost_per_tonne_upper = social_cost_upper / (sum(Carbon_total_upper) * -1),
    government_cost_upper = Final_gov_transfer - Final_vat_upper - Discounted_gov_rev_upper,
    government_cost_per_tonne_upper = government_cost_upper / (sum(Carbon_total_upper) * -1 * percent_marginal_consumers),
    resource_cost_upper = private_cost_heatpump - private_cost_gas_boiler + Discounted_change_energy_payments_upper,
    resource_cost_per_tonne_upper = resource_cost_upper / (sum(Carbon_total_upper) * -1)
  )



ggplot(MVPF_by_temp, aes(x = temp, y = MVPF)) +
  geom_line(color = "#8B5FBF", size = 2) +
  geom_point(color = "#8B5FBF") +
  labs(
    x = "Outdoor Temperature (Celcius)",
    y = "MVPF"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.y = element_text(
      angle = 0,
      vjust = 1,
      hjust = 1,
      margin = margin(r = 10), # positive right margin adds spacing
      size = 18
    ) )

ggplot(MVPF_by_temp, aes(x = temp, y = MVPF)) +
  geom_ribbon(aes(ymin = MVPF_lower, ymax = MVPF_upper), fill = "#8B5FBF", alpha = 0.2) +
  geom_smooth(se = FALSE, color = "#8B5FBF", size = 2, method = "loess") +
  geom_point(color = "#8B5FBF") +
  labs(
    x = "Outdoor Temperature (Celsius)",
    y = "MVPF"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.y = element_text(
      angle = 0,
      vjust = 1,
      hjust = 1,
      margin = margin(r = 10),
      size = 18
    )
  )


scale_factor <- max(
  MVPF_by_temp$government_cost_per_tonne_heatpump, 
  MVPF_by_temp$resource_cost_per_tonne_heatpump, 
  na.rm = TRUE
) / max(MVPF_by_temp$MVPF, na.rm = TRUE)

p_MVPF_temp <- 
  ggplot(MVPF_by_temp, aes(x = temp)) +
  # MVPF layer
  geom_ribbon(aes(ymin = MVPF_lower, ymax = MVPF_upper), fill = "#8B5FBF", alpha = 0.2) +
  geom_smooth(aes(y = MVPF), se = FALSE, color = "#8B5FBF", size = 1, method = "loess") +
  geom_point(aes(y = MVPF), color = "#8B5FBF") +
  
  # resource cost layer (scaled down)
  geom_ribbon(aes(
    ymin = government_cost_per_tonne_lower / scale_factor,
    ymax = government_cost_per_tonne_upper / scale_factor
  ), fill = "#87B6F8", alpha = 0.2) +
  geom_smooth(aes(y = government_cost_per_tonne_heatpump / scale_factor), se = FALSE, color = "#87B6F8", size = 1, method = "loess") +
  geom_point(aes(y = government_cost_per_tonne_heatpump / scale_factor), color = "#87B6F8") +
  
  # gov cost layer (scaled down)
  geom_ribbon(aes(
    ymin = resource_cost_per_tonne_lower / scale_factor,
    ymax = resource_cost_per_tonne_upper / scale_factor
  ), fill = "#D5AFF2", alpha = 0.2) +
  geom_smooth(aes(y = resource_cost_per_tonne_heatpump / scale_factor), se = FALSE, color = "#D5AFF2", size = 1, method = "loess") +
  geom_point(aes(y = resource_cost_per_tonne_heatpump / scale_factor), color = "#D5AFF2") +
  
  scale_y_continuous(
    name = "MVPF",
    sec.axis = sec_axis(~ . * scale_factor, name = "Cost per tonne", labels = scales::dollar_format(prefix = "£"))
  )  +
  labs(x = "Average Weekly Temperature in Degrees (°C)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.y = element_text(angle = 0, vjust = 0.95, hjust = 1, margin = margin(r = 10)),
    axis.title.y.right = element_text(angle = 0, vjust = 0.95, margin = margin(l = -50))
  ) +
  geom_text(x = 2, y = 2, label = "MVPF", color = "#8B5FBF", vjust = -1, alpha = 1) +
  geom_text(x = 5, y = 0.5, label = "Government Cost per tonne", color = "#87B6F8", vjust = -1, alpha = 1) +
  geom_text(x = 14, y = -0.1, label = "Resource Cost per tonne", color = "#D5AFF2", vjust = -1, alpha = 1) 

ggsave("graphs/MVPF_temp.png",
       width = 16, height = 8, units = "cm")
                     
                     
p_MVPF_temp + 
  labs(x = "Average Weekly Temperature in Degrees (°C)",
      title = "How welfare impacts of the BUS change with temperature") 

ggsave("graphs/MVPF_temp_blog_version.png",
       width = 18, height = 7, units = "cm")