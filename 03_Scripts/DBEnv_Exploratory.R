library(tidyverse)
library(lubridate)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read.csv("02_Clean_data/Dungbeetle_Env_clean.csv")

# Turn experiment groupings into factors
db_data$block_id <- as.factor(db_data$block_id)
db_data$burn_season <- as.factor(db_data$burn_season)
db_data$env_type <- as.factor(db_data$env_type)

# Filter NAs
db_data$latency2 = gsub("NA", "", db_data$latency) %>% as.numeric()
db_data$hour2 = gsub("NA", "", db_data$hour) %>% as.numeric()

remno_adj <- db_data %>% 
  select(date, block_id, burn_season, env_type, rem_no) %>% 
  filter(rem_no > 0)

#Histograms 
ggplot(db_data, aes(x = rem_event)) + 
  geom_histogram()

ggplot(db_data, aes(x = rem_no)) +
  geom_histogram() 

ggplot(remno_adj, aes(x = rem_no)) +
  geom_histogram() 
