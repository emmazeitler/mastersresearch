library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

#### Removal Number ####

# Separate the treatments

remno_b <- db_data %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, rem_no) %>% 
  select(-env_type)

colnames(remno_b)[2] <- "burn"
remno_b$id <- seq.int(nrow(remno_b))

remno_s <- db_data %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, rem_no) %>% 
  select(-env_type)

colnames(remno_s)[2] <- "scrub"
remno_s$id <- seq.int(nrow(remno_s))

remno <- remno_b %>% 
  merge(remno_s, by="id") %>%
  select(-burn_season.y) 

#Log ratio effect size

remno_lr <- remno %>% 
  mutate(burn = burn+1,
         scrub = scrub+1,
         diff = burn-scrub,
         ln.ratio = log(burn/scrub))

colnames(remno_lr)[2] <- "burn.season"

hist(remno_lr$ln.ratio)

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

## Figure ##

# Light = #FDA766
# Medium Light = #FFA80F
# Medium Dark = #FE8116
# Dark = #FE5A1D

firecolors1 <- c("#FE8116", "#FDA766","#FFA80F", "#FE5A1D")

p.remno <- ggplot() +
  geom_col(data=em.remno.lr,
           aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), y=emmean, fill=burn.season))+
  geom_errorbar(data=em.remno.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=(emmean-SE),
                    ymax=(emmean+SE),
                    color=burn.season),
                width=.09,
                lwd=1,
                position=position_dodge(width=0.5))+
  scale_fill_manual(values=firecolors1)+
  # scale_color_manual(values=firecolors1)+
  # geom_point(data=remno_lr, aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), y=ln.ratio), color="black")+
  xlab("Season of burn")+
  ylab("Effect size")+
  ggtitle("Amount of dung removed")+
  theme_bw()

p.remno

  
#### Probability of Removal ####
rm(list= ls()[!(ls() %in% c('db_data','p.remno', 'remno_lr', 'em.remno.lr', 'mod.remno.lr'))])

remev_b <- db_data %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, rem_event) %>% 
  select(-env_type)

colnames(remev_b)[2] <- "burn"
remev_b$id <- seq.int(nrow(remev_b))

remev_s <- db_data %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, rem_event) %>% 
  select(-env_type)

colnames(remev_s)[2] <- "scrub"
remev_s$id <- seq.int(nrow(remev_s))

remev <- remev_b %>% 
  merge(remev_s, by="id") %>%
  select(-burn_season.y)

#### Latency ####

remlat_b <- db_data %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, latency2) %>% 
  select(-env_type)

colnames(remlat_b)[2] <- "burn"
remlat_b$id <- seq.int(nrow(remlat_b))

remlat_s <- db_data %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, latency2) %>% 
  select(-env_type)

colnames(remlat_s)[2] <- "scrub"
remlat_s$id <- seq.int(nrow(remlat_s))

remlat <- remlat_b %>% 
  merge(remlat_s, by="id") %>%
  select(-burn_season.y)

## Log ratio effect size ##
remlat_lr <- remlat %>% 
  mutate(diff = burn-scrub,
         ln.ratio = log(burn/scrub))

colnames(remlat_lr)[2] <- "burn.season"

hist(remlat_lr$ln.ratio)

mod.remlat.lr <- glmmTMB(data = remlat_lr, ln.ratio ~ burn.season + (1|id), family=gaussian)

res.remlat.lr <- simulateResiduals(fittedModel = mod.remlat.lr, n = 250)
hist(res.remlat.lr)
plot(res.remlat.lr)

Anova(mod.remlat.lr)
summary(mod.remlat.lr)

emmeans(mod.remlat.lr, ~burn.season)

emmeans(mod.remlat.lr, pairwise~burn.season, adjust = "none")

## Fall - Summer 0.06 

em.remlat.lr <- emmeans(mod.remlat.lr, ~burn.season) %>% as.data.frame()

p.lat <- ggplot() +
  geom_col(data=em.remlat.lr,
           aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), y=emmean, fill=burn.season))+
  geom_errorbar(data=em.remlat.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=(emmean-SE),
                    ymax=(emmean+SE),
                    color=burn.season),
                width=.09,
                lwd=1,
                position=position_dodge(width=0.5))+
  scale_fill_manual(values=firecolors1)+
  # scale_color_manual(values=firecolors1)+
  # geom_point(data=remno_lr, aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), y=ln.ratio), color="black")+
  xlab("Season of burn")+
  ylab("Effect size")+
  ggtitle("Latency until removal")+
  theme_bw()

p.lat
