library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

## Removal Event ##

modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type + burn_season + (1|block_id), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)

emmeans(modRem1, ~env_type, type = "response")
em_remevent <- emmeans(modRem1, ~burn_season + env_type, type = "response") %>% as.data.frame()
emmeans(modRem1, pairwise~burn_season + env_type, type = "response")

ggplot(em_remevent) +
  geom_point(aes(x=env_type, y=prob, color=env_type, size = 2))+
  facet_wrap(~burn_season)+
  theme_bw()

ggplot(em_remevent)+
  geom_point(aes(x=burn_season, y=prob, color=burn_season, size=2))+
  facet_wrap(~env_type)+
  ylab("Probability of Removal")+
  theme_bw()

## Amount pf Dung Removed ##
modRemNo <- glmmTMB(data = db_data, rem_no ~ env_type + burn_season + (1|block_id), family= nbinom2)

resRemNo <- simulateResiduals(fittedModel = modRemNo, n =250)
hist(resRemNo)
plot(resRemNo)

Anova(modRemNo)
summary(modRemNo)

emmeans(modRemNo, ~env_type, type = "response")
em_remno <- emmeans(modRemNo, ~burn_season + env_type, type = "response") %>% as.data.frame()
emmeans(modRem1, pairwise~burn_season + env_type, type = "response")

ggplot(em_remno) +
  geom_point(aes(x=env_type, y=response, color=env_type, size = 2))+
  facet_wrap(~burn_season)+
  theme_bw()

ggplot(em_remno)+
  geom_point(aes(x=burn_season, y=response, color=burn_season, size=2))+
  facet_wrap(~env_type)+
  ylab("Amount of Dung Removed")+
  theme_bw()

## Latency ##

modLat <- glmmTMB(data = db_data, latency2 ~ env_type + burn_season + (1|block_id), family = gaussian)

resLat <- simulateResiduals(fittedModel = modLat, n=250)
hist(resLat)
plot(resLat)

Anova(modLat)
summary(modLat)

emmeans(modLat, ~env_type, type = "response")
em_lat <- emmeans(modLat, ~burn_season + env_type, type = "response") %>% as.data.frame()
emmeans(modRem1, pairwise~burn_season + env_type, type = "response")

ggplot(em_lat) +
  geom_point(aes(x=env_type, y=emmean, color=env_type, size = 2))+
  facet_wrap(~burn_season)+
  theme_bw()

ggplot(em_lat)+
  geom_point(aes(x=burn_season, y=emmean, color=burn_season, size=2))+
  facet_wrap(~env_type)+
  ylab("Hours Until First Removal")+
  theme_bw()
