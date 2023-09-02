library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)
library(effsize)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

db_data2 <- db_data %>% 
  filter(norempair == 0) %>% 
  select(pair_id, block_id, burn_season, env_type, rem_no, rem_event)

burn <- db_data2 %>% 
  filter(env_type == "BURN") %>% 
  rename(burn_env = env_type,
         rem_no_b = rem_no,
         rem_event_b = rem_event)

burn$id <- seq.int(nrow(burn))
  
scrub <- db_data2 %>% 
  filter(env_type == "SCRUB") %>% 
  rename(scrub_env = env_type,
         rem_no_s = rem_no,
         rem_event_s = rem_event) %>% 
  select(pair_id, scrub_env, rem_no_s, rem_event_s)

scrub$id <- seq.int(nrow(scrub))

db_data2 <- merge(burn, scrub, by = "pair_id")

#### Log Ratio ####

#### Removal Number ####

# Separate the treatments

remno_b <- db_data2 %>% 
  filter(burn_env == "BURN") %>% 
  select(block_id, pair_id, burn_season, burn_env, rem_no_b) %>% 
  select(-burn_env)

colnames(remno_b)[4] <- "burn"
remno_b$id <- seq.int(nrow(remno_b))

remno_s <- db_data2 %>% 
  filter(scrub_env == "SCRUB") %>% 
  select(pair_id, burn_season, scrub_env, rem_no_s) %>% 
  select(-scrub_env)

colnames(remno_s)[3] <- "scrub"
remno_s$id <- seq.int(nrow(remno_s))

names(remno)

remno <- remno_b %>% 
  merge(remno_s, by="pair_id") %>%
  select(-burn_season.y, -id.x, -id.y) 

#Log ratio effect size

remno_lr <- remno %>% 
  mutate(burn = burn+1,
         scrub = scrub+1,
         diff = burn-scrub,
         ln.ratio = log(burn/scrub)) %>% 
  drop_na()

colnames(remno_lr)[3] <- "burn.season"
table(remno$burn_season)

write_csv(remno_lr, "02_Clean_data/remNo_lr_raw.csv")

hist(remno_lr$ln.ratio)

#### Latency ####

rm(list=setdiff(ls(), "db_data"))

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

db_data2 <- db_data %>% 
  select(pair_id, block_id, burn_season, env_type, latency, rem_event)

remlat_b <- db_data2 %>% 
  filter(env_type == "BURN") %>% 
  select(block_id, pair_id, burn_season, env_type, latency) %>% 
  select(-env_type)

colnames(remlat_b)[4] <- "burn"
remlat_b$id <- seq.int(nrow(remlat_b))

remlat_s <- db_data2 %>% 
  filter(env_type == "SCRUB") %>% 
  select(pair_id, burn_season, env_type, latency) %>% 
  select(-env_type)

colnames(remlat_s)[3] <- "scrub"
remlat_s$id <- seq.int(nrow(remlat_s))

names(remlat)

remlat <- remlat_b %>% 
  merge(remlat_s, by="pair_id") %>%
  select(-burn_season.y, -id.x, -id.y)

## Log ratio effect size ##

remlat_lr <- remlat %>% 
  mutate(diff = burn-scrub,
         ln.ratio = log(burn/scrub)) %>% 
  drop_na()

colnames(remlat_lr)[3] <- "burn.season"

write_csv(remlat_lr, "02_Clean_data/remLat_lr_raw.csv")

#### Cohen's d ####

rm(list=setdiff(ls(), "db_data"))

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

## Spring

sp.burn <- remno_b %>% 
  filter(burn_season =="Spring")
sp.scrub <- remno_s%>% 
  filter(burn_season == "Spring")
spring <- cohen.d(sp.burn$burn, sp.scrub$scrub)
sp.remno <- data_frame("burn_season" = "Spring",
                         "Estimate" = spring$estimate,
                         "lcl" = spring$conf.int[1],
                         "ucl" = spring$conf.int[2])

## Summer

su.burn <- remno_b %>% 
  filter(burn_season =="Summer") %>% 
  drop_na()
su.scrub <- remno_s%>% 
  filter(burn_season == "Summer") %>% 
  drop_na()
summer <- cohen.d(su.burn$burn, su.scrub$scrub)
su.remno <- data_frame("burn_season" = "Summer",
                       "Estimate" = summer$estimate,
                       "lcl" = summer$conf.int[1],
                       "ucl" = summer$conf.int[2])

## Fall

fa.burn <- remno_b %>% 
  filter(burn_season =="Fall") %>% 
  drop_na()
fa.scrub <- remno_s%>% 
  filter(burn_season == "Fall") %>% 
  drop_na()
fall <- cohen.d(fa.burn$burn, fa.scrub$scrub)
fa.remno <- data_frame("burn_season" = "Fall",
                       "Estimate" = fall$estimate,
                       "lcl" = fall$conf.int[1],
                       "ucl" = fall$conf.int[2])

## Winter 

wi.burn <- remno_b %>% 
  filter(burn_season =="Winter") %>% 
  drop_na()
wi.scrub <- remno_s%>% 
  filter(burn_season == "Winter") %>% 
  drop_na()
winter <- cohen.d(wi.burn$burn, wi.scrub$scrub)
wi.remno <- data_frame("burn_season" = "Winter",
                       "Estimate" = winter$estimate,
                       "lcl" = winter$conf.int[1],
                       "ucl" = winter$conf.int[2])

remno_cd <- rbind(sp.remno, su.remno)
remno_cd <- rbind(remno_cd, fa.remno)
remno_cd <- rbind(remno_cd, wi.remno)

# write_csv(remno_cd, "02_Clean_data/remno_cd.csv")

#### Latency ####

rm(list=setdiff(ls(), "db_data"))

db_data3 <- db_data %>% 
  filter(norempair == 0) %>% 
  select(pair_id, block_id, burn_season, env_type, latency, rem_event)

#### Removal Number ####

# Separate the treatments

lat_b <- db_data3 %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, latency) %>% 
  select(-env_type) %>% 
  drop_na()

colnames(lat_b)[2] <- "burn"
lat_b$id <- seq.int(nrow(lat_b))

lat_s <- db_data3 %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, latency) %>% 
  select(-env_type) %>% 
  drop_na()

colnames(lat_s)[2] <- "scrub"
lat_s$id <- seq.int(nrow(lat_s))

## Spring
# Can't do it - not enough data
sp.burn <- lat_b %>% 
  filter(burn_season =="Spring")
sp.scrub <- lat_s%>% 
  filter(burn_season == "Spring")
spring <- cohen.d(sp.burn$burn, sp.scrub$scrub)
sp.lat <- data_frame("burn_season" = "Spring",
                       "Estimate" = spring$estimate,
                       "lcl" = spring$conf.int[1],
                       "ucl" = spring$conf.int[2])

## Summer 
su.burn <- lat_b %>% 
  filter(burn_season =="Summer") %>% 
  drop_na()
su.scrub <- lat_s%>% 
  filter(burn_season == "Summer") %>% 
  drop_na()
summer <- cohen.d(su.burn$burn, su.scrub$scrub)
su.lat <- data_frame("burn_season" = "Summer",
                       "Estimate" = summer$estimate,
                       "lcl" = summer$conf.int[1],
                       "ucl" = summer$conf.int[2])

## Fall
fa.burn <- lat_b %>% 
  filter(burn_season =="Fall") %>% 
  drop_na()
fa.scrub <- lat_s%>% 
  filter(burn_season == "Fall") %>% 
  drop_na()
fall <- cohen.d(fa.burn$burn, fa.scrub$scrub)
fa.lat <- data_frame("burn_season" = "Fall",
                       "Estimate" = fall$estimate,
                       "lcl" = fall$conf.int[1],
                       "ucl" = fall$conf.int[2])

## Winter
wi.burn <- lat_b %>% 
  filter(burn_season =="Winter") %>% 
  drop_na()
wi.scrub <- lat_s%>% 
  filter(burn_season == "Winter") %>% 
  drop_na()
winter <- cohen.d(wi.burn$burn, wi.scrub$scrub)
wi.lat <- data_frame("burn_season" = "Winter",
                       "Estimate" = winter$estimate,
                       "lcl" = winter$conf.int[1],
                       "ucl" = winter$conf.int[2])

lat_cd <- rbind(su.lat, fa.lat)
lat_cd <- rbind(lat_cd, wi.lat)

# write_csv(lat_cd, "02_Clean_data/lat_cd.csv")

#### Cohen's h ####

#### Probability of Removal ####
library(pwr)

rm(list=setdiff(ls(), "db_data"))

# Spring

sp <- db_data %>% 
  filter(burn_season == "Spring") %>% 
  group_by(burn_season, env_type) %>%
  summarize(sum = sum(rem_event, na.rm=TRUE),
            total = n()) %>% 
  mutate(prop = sum/total)

sp_prop_b <- sp %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(sp_prop_b)[2] <- "burn"

sp_prop_S <- sp %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(sp_prop_S)[2] <- "scrub"

sp.prop <- merge(sp_prop_b, sp_prop_S, by="burn_season")

sp.prop.es <- sp.prop %>% 
  mutate(cohen.h = ES.h(sp.prop$burn, sp.prop$scrub))

# Summer

su <- db_data %>% 
  filter(burn_season == "Summer") %>% 
  group_by(burn_season, env_type) %>%
  summarize(sum = sum(rem_event, na.rm=TRUE),
            total = n()) %>% 
  mutate(prop = sum/total)

su_prop_b <- su %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(su_prop_b)[2] <- "burn"

su_prop_S <- su %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(su_prop_S)[2] <- "scrub"

su.prop <- merge(su_prop_b, su_prop_S, by="burn_season")

su.prop.es <- su.prop %>% 
  mutate(cohen.h = ES.h(su.prop$burn, su.prop$scrub))

# Fall

fa <- db_data %>% 
  filter(burn_season == "Fall") %>% 
  group_by(burn_season, env_type) %>%
  summarize(sum = sum(rem_event, na.rm=TRUE),
            total = n()) %>% 
  mutate(prop = sum/total)

fa_prop_b <- fa %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(fa_prop_b)[2] <- "burn"

fa_prop_S <- fa %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(fa_prop_S)[2] <- "scrub"

fa.prop <- merge(fa_prop_b, fa_prop_S, by="burn_season")

fa.prop.es <- fa.prop %>% 
  mutate(cohen.h = ES.h(fa.prop$burn, fa.prop$scrub))

# Winter

wi <- db_data %>% 
  filter(burn_season == "Winter") %>% 
  group_by(burn_season, env_type) %>%
  summarize(sum = sum(rem_event, na.rm=TRUE),
            total = n()) %>% 
  mutate(prop = sum/total)

wi_prop_b <- wi %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(wi_prop_b)[2] <- "burn"

wi_prop_S <- wi %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, prop) %>% 
  select(-env_type)

colnames(wi_prop_S)[2] <- "scrub"

wi.prop <- merge(wi_prop_b, wi_prop_S, by="burn_season")

wi.prop.es <- wi.prop %>% 
  mutate(cohen.h = ES.h(wi.prop$burn, wi.prop$scrub))

propRem_ch <- rbind(sp.prop.es, su.prop.es)
propRem_ch <- rbind(propRem_ch, fa.prop.es)
propRem_ch <- rbind(propRem_ch, wi.prop.es)

propRem_ch <- propRem_ch %>% 
  pivot_longer(cols = 2:3, names_to = "env_type", values_to = "remProb")

# write_csv(propRem_ch, "02_Clean_data/propRem_ch.csv")
