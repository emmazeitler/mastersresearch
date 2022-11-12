library(tidyverse)
library(dplyr)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

read_csv("02_Clean_data/Dungbeetle_Env_clean.csv")

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

### GLMs ####

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
