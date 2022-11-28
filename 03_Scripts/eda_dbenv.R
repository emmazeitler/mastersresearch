
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
library(dplyr)
library(lubridate)
library(outliers)
library(ggplot2)

db_data <- read.csv("02_Clean_data/dbenv_use.csv")

## --------------- CHECK DATA STRUCTURE ----------------------------------------

str(db_data)
summary(db_data)

## --------------- HISTOGRAMS ------------------------------------------------

# Removal event 
ggplot(db_data, aes(x = rem_event, fill = env_type)) + 
  geom_histogram() +
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Ocurrence of removal event (binary)") +
  ylab("Count")

ggplot(db_data, aes(x = rem_event, fill = env_type)) + 
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Ocurrence of removal event (binary)") +
  ylab("Count")

# Removal number
ggplot(db_data, aes(x = rem_no, fill = env_type)) +
  geom_histogram()+ 
  geom_vline(aes(xintercept= mean(rem_no)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(rem_no)), color = "orange", size = 2)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Count of scat removed") +
  ylab("Frequency")

ggplot(db_data, aes(x = rem_no, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Count of scat removed") +
  ylab("Frequency")

#Without 0s

ggplot(remno_adj, aes(x = rem_no, fill = env_type)) +
  geom_histogram()+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Count of scat removed") +
  ylab("Frequency") +
  xlim(0,16)

ggplot(remno_adj, aes(x = rem_no, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Count of scat removed") +
  ylab("Frequency")+
  xlim(0,16)

# Latency 

ggplot(db_data, aes(x = latency2, fill = env_type)) +
  geom_histogram() +
  geom_vline(aes(xintercept= mean(latency2)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(latency2)), color = "orange", size = 2)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Time until first removal (hrs)") +
  ylab("Frequency")

ggplot(db_data, aes(x = latency2, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Time until first removal (hrs)") +
  ylab("Frequency")

# Hour
ggplot(db_data, aes( x=hour2, fill=env_type)) +
  geom_histogram()+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Hour of first removal (24-hour clock)") +
  ylab("Frequency")

ggplot(db_data, aes(x = hour2, fill = env_type)) +
  geom_histogram() +
  facet_wrap(~env_type)+
  scale_fill_manual(values=c("#EE7733","#117733"))+
  xlab("Hour of first removal (24-hour clock)") +
  ylab("Frequency")

## --------------- SUMMARIES ----------------------------------------

sum_remno <- db_data %>% 
  group_by(env_type) %>% 
  summarize(min=min(rem_no, na.rm=TRUE),
            max=max(rem_no, na.rm=TRUE),
            mean=mean(rem_no, na.rm=TRUE),
            median=median(rem_no, na.rm = TRUE),
            sd=sd(rem_no, na.rm = TRUE))


write.table(sum_remno, file = "dbenv_sum_remno", sep=",", quote = FALSE)

sum_remno_noz <- remno_adj %>% 
  group_by(env_type) %>% 
  summarize(min=min(rem_no, na.rm=TRUE),
            max=max(rem_no, na.rm=TRUE),
            mean=mean(rem_no, na.rm=TRUE),
            median=median(rem_no, na.rm = TRUE),
            sd=sd(rem_no, na.rm = TRUE))

write.table(sum_remno_noz, file = "dbenv_sum_nozerosremno", sep=",", quote = FALSE)

sum_lat <- db_data %>% 
  group_by(env_type) %>% 
  summarize(min=min(latency2, na.rm=TRUE),
            max=max(latency2, na.rm=TRUE),
            mean=mean(latency2, na.rm=TRUE),
            median=median(latency2, na.rm=TRUE),
            sd=sd(latency2, na.rm=TRUE))

write.table(sum_lat, file = "dbenv_sum_lat", sep=",", quote = FALSE)

sum_hour <- db_data %>% 
  group_by(env_type) %>% 
  summarize(min=min(hour2, na.rm=TRUE),
            max=max(hour2, na.rm=TRUE),
            mean=mean(hour2, na.rm=TRUE),
            median=median(latency2, na.rm = TRUE),
            sd=sd(hour2, na.rm=TRUE))

write.table(sum_hour, file = "dbenv_sum_hour", sep=",", quote = FALSE)

## --------------- CHECK/FILTER OUTLIERS ----------------------------------------

grubbs.test(db_data$rem_no)
 
grubbs.test(db_data$latency2)


