library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

str(db_data)
str(db_data_2)

db_data <- db_data %>% 
  filter(!burn_season == "Spring")

db_data_2 <- db_data %>% 
  filter(norempair == 0)

class(db_data_2$rem_no)
  
## Removal Event ##

modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type * burn_season + (1|block_id) + (1|pair_id), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)

emmeans(modRem1, ~env_type, type = "response")

emmeans(modRem1, ~burn_season, type = "response") %>% as.data.frame()

remProb <- emmeans(modRem1, pairwise~env_type | burn_season , type = "response")
remOddsRatio <- remProb$contrasts %>% as.data.frame()

emmeans(modRem1, pairwise~ burn_season | env_type, type = "response")
emmeans(modRem1, pairwise~ burn_season, type = "response")


remOddsRatio <- remOddsRatio %>% 
  mutate(lcl = odds.ratio - SE*qnorm(0.975),
         ucl = odds.ratio + SE*qnorm(0.975))

# write_csv(remOddsRatio, "02_Clean_data/propRem_or.csv")

## Amount of Dung Removed ##

modRemNo1 <- glmmTMB(data = db_data_2, rem_no ~ env_type * burn_season + (1|block_id) +(1|pair_id), family= nbinom2)

resRemNo <- simulateResiduals(fittedModel = modRemNo1, n =250)
hist(resRemNo)
plot(resRemNo)

testCategorical(resRemNo, db_data_2$env_type)

Anova(modRemNo1)
summary(modRemNo1)

emmeans(modRemNo1, pairwise~env_type | burn_season , type = "response")

test <- emmeans(modRemNo1, pairwise~env_type | burn_season , type = "response") %>% as.data.frame()

emmeans(modRemNo1, pairwise~ burn_season | env_type, type = "response")
emmeans(modRemNo1, pairwise~ burn_season, type = "response")


## Latency ##

modLat <- glmmTMB(data = db_data_2, latency ~ env_type * burn_season + (1|block_id), family = gaussian)

resLat <- simulateResiduals(fittedModel = modLat, n=250)
hist(resLat)
plot(resLat)

testCategorical(resLat, db_data_2$burn_season)
VarCorr(modLat)

total.SD = sqrt(0.89107^2)

Anova(modLat)
summary(modLat)

emmeans(modLat, ~env_type, type = "response")

emmeans(modLat, pairwise~env_type | burn_season, type = "response", bias.adjust = TRUE, sigma = total.SD)

emmeans(modLat, pairwise~burn_season, type = "response")

emmeans(modLat, pairwise~ burn_season | env_type, type = "response")
emmeans(modLat, pairwise~ burn_season, type = "response")


