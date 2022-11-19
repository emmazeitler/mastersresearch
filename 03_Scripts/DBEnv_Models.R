## --------------- HEADER ------------------------------------------------------
## Script glmanalysis_db_env.R
## Author: Emma Zeitler, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-10-16
## Contact: ezeitler@ufl.edu
## Purpose of script: This script creates and analyzes generalized linear models
## to describe experiments the effects of landscape changes associated with  
## prescribed fire on dung beetle dung removal. 

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

read_csv("02_Clean_data/Dungbeetle_Env_clean.csv")

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

## --------------- CREATE GLMS / CHECK RESIDUALS / CHECK STATISTICS -------------

#Probability of removal event
modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type + (1|block_id) + (1|burn_season), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)

emmeans(modRem1, ~env_type, type = "response")

#Amount of Dung Removed 
modRemNo <- glmmTMB(data = db_data, rem_no ~ env_type + (1|block_id) + (1|burn_season), family=nbinom2)

resRemNo <- simulateResiduals(fittedModel = modRemNo, n =250)
hist(resRemNo)
plot(resRemNo)

Anova(modRemNo)
summary(modRemNo)

emmeans(modRemNo, ~env_type, type = "response")

#Latency until Removal
modLat <- glmmTMB(data = db_data, latency2 ~ env_type + (1|block_id) + (1|burn_season), family = gaussian)

resLat <- simulateResiduals(fittedModel = modLat, n=250)
hist(resLat)
plot(resLat)

Anova(modLat)
summary(modLat)

emmeans(modLat, ~env_type, type = "response")

## --------------- GRAPH MEANS -------------------------------------------------

#Removal event

emRem <- emmeans(modRem1, ~env_type, type="response") %>% as.data.frame()

ggplot(emRem, aes(x = env_type, y = prob)) +
  geom_point(aes(color=env_type), size=3) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_errorbar(aes(x=env_type, y=prob, ymin=(prob-SE), ymax=(prob+SE), color=env_type), width=.2, lwd=1.25, position=position_dodge(width=0.5)) +
  labs(x = "Environment type", y = "Probability of removal")

#Removal amount

emAmt <- emmeans(modRemNo, ~env_type, type="response") %>% as.data.frame()

ggplot(emAmt, aes(x = env_type, y = response)) +
  geom_point(aes(color=env_type), size=3) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_errorbar(aes(x=env_type, y=response, ymin=(response-SE), ymax=(response+SE), color=env_type), width=.2, lwd=1.25, position=position_dodge(width=0.5)) +
  labs(x = "Environment type", y = "Amount of dung pellets removed")

#Latency

emLat <- emmeans(modLat, ~env_type, type="response") %>% as.data.frame()

ggplot(emLat, aes(x = env_type, y = emmean)) +
  geom_point(aes(color=env_type), size=3) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_errorbar(aes(x=env_type, y=emmean, ymin=(emmean-SE), ymax=(emmean+SE), color=env_type), width=.2, lwd=1.25, position=position_dodge(width=0.5)) +
  labs(x = "Environment type", y = "Latency (hours) until removal event")
