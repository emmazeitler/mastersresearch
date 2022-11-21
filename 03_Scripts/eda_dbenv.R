
## --------------- HEADER ------------------------------------------------------
## Script eda_db_env.R
## Author: Emma Zeitler, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-10-16
## Contact: ezeitler@ufl.edu
## Purpose of script: This script is an exploratory data analysis of experiments 
## testing the effect of landscape changes associated with prescribed fire on 
## dung beetle dung removal. 

## --------------- SET—UP WORKSPACE --------------------------------------------

library(tidyverse)
library(lubridate)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)
library(outliers)

db_data <- read.csv("02_Clean_data/Dungbeetle_Env_clean.csv")

## --------------- CHECK DATA STRUCTURE ----------------------------------------
str(db_data)
summary(db_data)

# Turn experiment groupings into factors
db_data$block_id <- as.factor(db_data$block_id)
db_data$burn_season <- as.factor(db_data$burn_season)
db_data$env_type <- as.factor(db_data$env_type)

remno_adj <- db_data %>% 
  select(date, block_id, burn_season, env_type, rem_no) %>% 
  filter(rem_no > 0)

## --------------- FILTER MISSING DATA ----------------------------------------

# Filter NAs
db_data$latency2 = gsub("NA", "", db_data$latency) %>% as.numeric()
db_data$hour2 = gsub("NA", "", db_data$hour) %>% as.numeric()

db_data <- head(db_data, - 1)

remno_adj <- db_data %>% 
  select(date, block_id, burn_season, env_type, rem_no) %>% 
  filter(rem_no > 0)

## --------------- HISTOGRAMS ------------------------------------------------

# Removal event 
ggplot(db_data, aes(x = rem_event, fill = env_type)) + 
  geom_histogram() +
  scale_fill_manual(values=c("#EE7733","#117733"))

ggplot(db_data, aes(x = rem_event, fill = env_type)) + 
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))

# Removal number
ggplot(db_data, aes(x = rem_no, fill = env_type)) +
  geom_histogram()+ 
  geom_vline(aes(xintercept= mean(rem_no)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(rem_no)), color = "orange", size = 2)+
  scale_fill_manual(values=c("#EE7733","#117733"))

ggplot(db_data, aes(x = rem_no, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))

ggplot(remno_adj, aes(x = rem_no, fill = env_type)) +
  geom_histogram()+
  scale_fill_manual(values=c("#EE7733","#117733"))
  # geom_vline(aes(xintercept= mean(rem_no)), color = "blue", size = 2) +
  # geom_vline(aes(xintercept= median(rem_no)), color = "orange", size = 2) # Without 0s

ggplot(remno_adj, aes(x = rem_no, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))

# Latency 

ggplot(db_data, aes(x = latency2, fill = env_type)) +
  geom_histogram() +
  geom_vline(aes(xintercept= mean(latency2)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(latency2)), color = "orange", size = 2)+
  scale_fill_manual(values=c("#EE7733","#117733"))

ggplot(db_data, aes(x = latency2, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))

# Hour
ggplot(db_data, aes( x=hour2, fill=env_type)) +
  geom_histogram()+
  scale_fill_manual(values=c("#EE7733","#117733"))

ggplot(db_data, aes(x = hour2, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))

## --------------- SUMMARIES ----------------------------------------

sum_remno <- db_data %>% 
  group_by(env_type) %>% 
  summarize(min=min(rem_no, na.rm=TRUE),
            max=max(rem_no, na.rm=TRUE),
            mean=mean(rem_no, na.rm=TRUE),
            median=median(rem_no, na.rm = TRUE),
            sd=sd(rem_no, na.rm = TRUE))

sum_lat <- db_data %>% 
  group_by(env_type) %>% 
  summarize(min=min(latency2, na.rm=TRUE),
            max=max(latency2, na.rm=TRUE),
            mean=mean(latency2, na.rm=TRUE),
            median=median(latency2, na.rm=TRUE),
            sd=sd(latency2, na.rm=TRUE))

sum_hour <- db_data %>% 
  group_by(env_type) %>% 
  summarize(min=min(hour2, na.rm=TRUE),
            max=max(hour2, na.rm=TRUE),
            mean=mean(hour2, na.rm=TRUE),
            median=median(latency2, na.rm = TRUE),
            sd=sd(hour2, na.rm=TRUE))

## --------------- FILTER OUTLIERS ----------------------------------------

grubbs.test(db_data$rem_no)
 
grubbs.test(db_data$latency2)

