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
library(lubridate)

db_data <- read_csv("01_Raw_data/DB_Fire_raw.csv")

## --------------- CHECK DATA STRUCTURE ----------------------------------------

str(db_data)
summary(db_data)


## --------------- SELECT VARIABLES THAT ARE RELEVANT ----------------------------
names(db_data)

db_data <- db_data %>% 
  select(-Notes, 
         -'Removal type', 
         -'Weather conditions', 
         -'Test Duration (h)', 
         -'Start Time',
         -'Time')

## --------------- RENAME COLUMNS ----------------------------------------

db_data <- db_data %>% 
  rename(date = 'Date',
         block_id = `Block ID`,
         burn_season = `Burn Treatment`,
         pair_id = `Pair ID`,
         env_type = `Burn/Scrub`,
         rem_no = `Number of pellets taken`,
         rem_event = Removal,
         latency = `Time until first removal (h)`,
         hour = `Time at first removal`)

## --------------- CHANGE DATA TYPES FOR ANALYSES ------------------------------

db_data$burn_season[db_data$burn_season =="F"] <- "Fall"
db_data$burn_season[db_data$burn_season =="SP"] <- "Spring"
db_data$burn_season[db_data$burn_season =="SU"] <- "Summer"
db_data$burn_season[db_data$burn_season =="W"] <- "Winter"

## --------------- CHANGE DATA TYPES FOR ANALYSES ------------------------------

# Turn experiment groupings into factors
db_data$block_id <- as.factor(db_data$block_id)
db_data$burn_season <- as.factor(db_data$burn_season)
db_data$env_type <- as.factor(db_data$env_type)
db_data$pair_id <- as.factor(db_data$ pair_id)

## --------------- CORRECT LATENCY & REMOVAL NUMBER ----------------------------------------

db_data$latency <- as.numeric(db_data$latency)
lat <- db_data[, c("latency")]
lat[is.na(lat)] <- 24
db_data[, c("latency")] <- lat

db_data$rem_no <- as.numeric(db_data$rem_no)
rem_no <- db_data[, c("rem_no")]
rem_no[is.na(rem_no)] <- 0
db_data[, c("rem_no")] <- rem_no
## --------------- CORRECT DATES -----------------------------------------------

db_data$date <- mdy(db_data$date)

## --------------- DELETE UNPAIRED -----------------------------------------------

db_data <- db_data %>% 
  drop_na(pair_id)

## --------------- CREATE DF FOR REMOVAL EVENT AND LATENCY -------------------

burn_rem <- db_data %>% 
  select(pair_id, env_type, rem_event) %>% 
  filter(env_type == "BURN") %>% 
  select(-env_type) %>% 
  rename(burn = rem_event)

scrub_rem <- db_data %>% 
  select(pair_id, env_type, rem_event) %>% 
  filter(env_type == "SCRUB") %>% 
  select(-env_type) %>% 
  rename(scrub = rem_event)
  
norem <- burn_rem %>% 
  merge(scrub_rem, by="pair_id") %>% 
  unite("norempair", 2:3, sep = "") 

norem$norempair[norem$norempair == '11'] <- '0'
norem$norempair[norem$norempair == '01'] <- '0'
norem$norempair[norem$norempair == '10'] <- '0'
norem$norempair[norem$norempair == '00'] <- '1'

db_data <- merge(db_data, norem, by="pair_id")

## --------------- SAVE CSV -----------------------------------------------
write.csv(db_data, file = "02_Clean_data/dbenv_use.csv")
