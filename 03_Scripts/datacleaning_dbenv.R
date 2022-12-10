## --------------- HEADER ------------------------------------------------------
## Script: datacleaning_dbenv.R
## Author: Emma Zeitler, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-10-16
## Contact: ezeitler@ufl.edu
## Purpose of script: This script cleans the dataset associated with the dung
## beetle x burn environment experiment. 

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)

db_data <- read_csv("02_Clean_data/dbenv_clean.csv")

## --------------- CHECK DATA STRUCTURE ----------------------------------------

str(db_data)
summary(db_data)

## --------------- CHANGE DATA TYPES FOR ANALYSES ------------------------------

# Turn experiment groupings into factors
db_data$block_id <- as.factor(db_data$block_id)
db_data$burn_season <- as.factor(db_data$burn_season)
db_data$env_type <- as.factor(db_data$env_type)


## --------------- FILTER MISSING DATA ----------------------------------------

# Filter NAs
db_data$latency2 = gsub("NA", "", db_data$latency) %>% as.numeric()
db_data$hour2 = gsub("NA", "", db_data$hour) %>% as.numeric()

## --------------- CORRECT DATES -----------------------------------------------

db_data$date <- mdy(db_data$date)

write.csv(db_data, file = "dbenv_use.csv")


