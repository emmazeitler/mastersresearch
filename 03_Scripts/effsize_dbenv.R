library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)
library(effsize)

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
         ln.ratio = log(burn/scrub)) %>% 
  drop_na()

colnames(remno_lr)[2] <- "burn.season"
table(remno$burn_season.x)

# write_csv(remno_lr, "02_Clean_data/lr_remNo.csv")

hist(remno_lr$ln.ratio)

#### Probability of Removal ####

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
         ln.ratio = log(burn/scrub)) %>% 
  drop_na()

colnames(remlat_lr)[2] <- "burn.season"

# write_csv(remno_lr, "02_Clean_data/lr_remNo.csv")

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

#### Removal Number ####

# Separate the treatments

lat_b <- db_data %>% 
  filter(env_type == "BURN") %>% 
  select(burn_season, env_type, latency2) %>% 
  select(-env_type) %>% 
  drop_na()

colnames(lat_b)[2] <- "burn"
lat_b$id <- seq.int(nrow(lat_b))

lat_s <- db_data %>% 
  filter(env_type == "SCRUB") %>% 
  select(burn_season, env_type, latency2) %>% 
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

write_csv(lat_cd, "02_Clean_data/lat_cd.csv")

#### Cohen's h ####

#### Probability of Removal ####
library(pwr)

rm(list=setdiff(ls(), "db_data"))

spring_avgs <- db_data %>% 
  filter(burn_season == "Spring") %>% 
  group_by(burn_season, env_type) %>% 
  summarize(rem_no = mean(rem_no, na.rm=TRUE),
            rem_event = mean(rem_event, na.rm=TRUE),
            latency = mean(latency2, na.rm=TRUE))

sp <- db_data %>% 
  filter(burn_season == "Spring") %>% 
  group_by(burn_season, env_type) %>%
  summarize(sum = sum(rem_event, na.rm=TRUE),
            total = n()) %>% 
  mutate(prop = sum/total)

test <- sp %>% 
  pivot_wider(names_from = env_type, values_from = prop)

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
  mutate(lo.ratio = burn/(1 - burn)/(scrub / (1 - scrub)))


