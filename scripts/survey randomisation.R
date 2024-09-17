# cosy survey randomisation

library(data.table)
library(dplyr)

accounts_to_exclude <- fread("data/input/Cosy AN's sent NPS - Sheet1.csv")
cosy_account_numbers <- fread("data/input/cosy - account numbers.csv")

# set seed for reproduction
set.seed(123456)

# select 1000s accounts
cosy_account_numbers_filtered <- cosy_account_numbers %>%
  filter(!account_number %in% accounts_to_exclude$account_number)

selection <- sample(cosy_account_numbers_filtered$account_number, 1000)

# write output
fwrite(data.frame(account_number = selection),
       "data/output/account_number_survey.csv")
  
# check
checks <- selection[selection %in% accounts_to_exclude$account_number]
