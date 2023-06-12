library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

## Removal Event ##

# modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type + burn_season + (1|block_id), family=binomial)

modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type * burn_season + (1|block_id), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)

emmeans(modRem1, ~env_type, type = "response")

emmeans(modRem1, ~burn_season, type = "response") %>% as.data.frame()

remProb <- emmeans(modRem1, pairwise~env_type | burn_season , type = "response") %>% as.data.frame()

emmeans(modRem1, ~env_type | burn_season , type = "response")
## Amount of Dung Removed ##

 modRemNo <- glmmTMB(data = db_data, rem_no ~ env_type * burn_season + (1|block_id), family= nbinom2)

resRemNo <- simulateResiduals(fittedModel = modRemNo, n =250)
hist(resRemNo)
plot(resRemNo)

Anova(modRemNo)
summary(modRemNo)

emmeans(modRemNo, pairwise~env_type | burn_season , type = "response")

test <- emmeans(modRemNo, pairwise~env_type | burn_season , type = "response") %>% as.data.frame()

## Latency ##

modLat <- glmmTMB(data = db_data, latency2 ~ env_type * burn_season + (1|block_id), family = gaussian)

resLat <- simulateResiduals(fittedModel = modLat, n=250)
hist(resLat)
plot(resLat)

Anova(modLat)
summary(modLat)

emmeans(modLat, ~env_type, type = "response")

emmeans(modLat, pairwise~env_type | burn_season, type = "response")

