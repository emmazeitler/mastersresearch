library(tidyverse)
library(lubridate)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read.csv("02_Clean_data/Dungbeetle_Env_clean.csv")

str(db_data)
summary(db_data)

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

#### Histograms #### 

# Removal event 
ggplot(db_data, aes(x = rem_event)) + 
  geom_histogram()

ggplot(db_data, aes(x = rem_event)) + 
  geom_histogram() +
  facet_wrap(~env_type)

# Removal number
ggplot(db_data, aes(x = rem_no)) +
  geom_histogram()+ 
  geom_vline(aes(xintercept= mean(rem_no)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(rem_no)), color = "orange", size = 2)

ggplot(db_data, aes(x = rem_no)) +
  geom_histogram() +
  facet_wrap(~env_type)

ggplot(remno_adj, aes(x = rem_no)) +
  geom_histogram()+
  geom_vline(aes(xintercept= mean(rem_no)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(rem_no)), color = "orange", size = 2) # Without 0s

ggplot(remno_adj, aes(x = rem_no)) +
  geom_histogram() +
  facet_wrap(~env_type)

# Latency 

ggplot(db_data, aes(x = latency2)) +
  geom_histogram() +
  geom_vline(aes(xintercept= mean(latency2)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(latency2)), color = "orange", size = 2)

ggplot(db_data, aes(x = latency2)) +
  geom_histogram() +
  facet_wrap(~env_type)

# Hour

ggplot(db_data, aes(x = hour2)) +
  geom_histogram() + 
  geom_vline(aes(xintercept= mean(hour2)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(hour2)), color = "orange", size = 2)
  

ggplot(db_data, aes(x = hour2)) +
  geom_histogram() +
  facet_wrap(~env_type) 

#### Boxplots ####

ggplot(db_data, aes(y = rem_no)) +
  geom_boxplot() +
  facet_wrap(~env_type)

ggplot(db_data, aes(y = latency2)) +
  geom_boxplot() +
  facet_wrap(~env_type) 

ggplot(db_data, aes(y = hour2)) +
  geom_boxplot() +
  facet_wrap(~env_type) 

#### Summaries ####

db_data %>% 
  group_by(env_type) %>% 
  summarize(mean=mean(rem_no, na.rm=TRUE),
            median=median(rem_no, na.rm = TRUE))

db_data %>% 
  group_by(env_type) %>% 
  summarize(mean=mean(latency2, na.rm=TRUE),
            median=median(latency2, na.rm = TRUE))

db_data %>% 
  group_by(env_type) %>% 
  summarize(mean=mean(hour2, na.rm=TRUE),
            median=median(latency2, na.rm = TRUE))





