#### RemNo ####

mod.remno.lr <- glmmTMB(data = remno_lr, ln.ratio ~ burn.season + (1|id), family=gaussian)

res.remno.lr <- simulateResiduals(fittedModel = mod.remno.lr, n = 250)
hist(res.remno.lr)
plot(res.remno.lr)

Anova(mod.remno.lr)
summary(mod.remno.lr)

emmeans(mod.remno.lr, ~burn.season)

emmeans(mod.remno.lr, pairwise~burn.season, adjust = "none")

## Summer - Winter 0.04 

em.remno.lr <- emmeans(mod.remno.lr, ~burn.season) %>% as.data.frame()

#### Probability of Removal ####


#### Latency ####

hist(remlat_lr$ln.ratio)

mod.remlat.lr <- glmmTMB(data = remlat_lr, ln.ratio ~ burn.season + (1|id), family=gaussian)

res.remlat.lr <- simulateResiduals(fittedModel = mod.remlat.lr, n = 250)
hist(res.remlat.lr)
plot(res.remlat.lr)

Anova(mod.remlat.lr)
summary(mod.remlat.lr)

emmeans(mod.remlat.lr, ~burn.season)

emmeans(mod.remlat.lr, pairwise~burn.season, adjust = "none")