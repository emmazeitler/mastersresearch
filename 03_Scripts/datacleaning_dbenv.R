## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(dplyr)

db_data <- read_csv("02_Clean_data/dbenv_clean.csv")

## --------------- CHECK DATA STRUCTURE ----------------------------------------

str(db_data)
summary(db_data)

## --------------- CHANGE DATA TYPES FOR ANALYSES ------------------------------

# Turn experiment groupings into factors
db_data$block_id <- as.factor(db_data$block_id)
db_data$burn_season <- as.factor(db_data$burn_season)
db_data$env_type <- as.factor(db_data$env_type)

#Scat removed without 0s (nonremoval event)
remno_adj <- db_data %>% 
  select(date, block_id, burn_season, env_type, rem_no) %>% 
  filter(rem_no > 0)

## --------------- FILTER MISSING DATA ----------------------------------------

# Filter NAs
db_data$latency2 = gsub("NA", "", db_data$latency) %>% as.numeric()
db_data$hour2 = gsub("NA", "", db_data$hour) %>% as.numeric()

?write.csv

write.csv(db_data, file = "db_env_use.csv")
