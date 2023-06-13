library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

#### RemNo ####
remno_lr <- read_csv("02_Clean_data/remNo_lr_raw.csv")

mod.remno.lr <- glmmTMB(data = remno_lr, ln.ratio ~ burn.season + (1|id), family=gaussian)

res.remno.lr <- simulateResiduals(fittedModel = mod.remno.lr, n = 250)
hist(res.remno.lr)
plot(res.remno.lr)

Anova(mod.remno.lr)
summary(mod.remno.lr)

remNo_lrmod <- emmeans(mod.remno.lr, ~burn.season) %>% as.data.frame()

write_csv(remNo_lrmod, "02_Clean_data/remNo_lrmod.csv")

#### Probability of Removal ####


#### Latency ####

remlat_lr <- read_csv("02_Clean_data/remLat_lr_raw.csv")

mod.remlat.lr <- glmmTMB(data = remlat_lr, ln.ratio ~ burn.season + (1|id), family=gaussian)

res.remlat.lr <- simulateResiduals(fittedModel = mod.remlat.lr, n = 250)
hist(res.remlat.lr)
plot(res.remlat.lr)

Anova(mod.remlat.lr)
summary(mod.remlat.lr)

remlat_lrmod <- emmeans(mod.remlat.lr, ~burn.season) %>% as.data.frame()
# write_csv(remlat_lrmod, "02_Clean_data/remlat_lrmod.csv")