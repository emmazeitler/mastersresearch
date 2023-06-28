library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)
library(effsize)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

#### Log Ratio ####

remlat_b <- db_data %>% 
  filter(env_type == "BURN") %>% 
  select(block_id, pair_id, burn_season, env_type, latency) %>% 
  select(-env_type)

colnames(remlat_b)[4] <- "burn"
remlat_b$id <- seq.int(nrow(remlat_b))

remlat_s <- db_data %>% 
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

write_csv(remlat_lr, "02_Clean_data/NEW_remLat_lr_raw.csv")

#### LR GLM ####
remlat_lr_n <- read_csv("02_Clean_data/NEW_remLat_lr_raw.csv")
remlat_lr_n <- remlat_lr %>% 
  filter(!burn.season == "Spring")

mod.remlat.lr.n <- glmmTMB(data = remlat_lr_n, ln.ratio ~ burn.season + (1|block_id), family=gaussian)

diagnose(mod.remlat.lr.n)

res.remlat.lr.n <- simulateResiduals(fittedModel = mod.remlat.lr.n, n = 250)
hist(res.remlat.lr.n)
plot(res.remlat.lr.n)

Anova(mod.remlat.lr.n)
summary(mod.remlat.lr.n)

remlat_lrmod.n <- emmeans(mod.remlat.lr.n, ~burn.season) %>% as.data.frame()

#### Figure ####
firecolors1 <- c("#FE8116", "#FDA766","#FFA80F", "#FE5A1D")

ggplot() +
  geom_col(data=remlat_lrmod.n,
           aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), y=emmean, fill=burn.season))+
  geom_errorbar(data=remlat_lrmod.n, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL,
                    color=burn.season),
                width=.09,
                lwd=1,
                position=position_dodge(width=0.5))+
  scale_fill_manual(values=firecolors1)+
  xlab("Season of burn")+
  ylab("Effect size")+
  ggtitle("Latency until removal")+
  theme_bw()+
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(color = "black",
                                   size = 18),
        axis.text.x = element_text(color = "black",
                                   size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5,
                                          colour = "gray"),
        panel.grid.minor.y = element_line(linetype = 2,
                                          color = "lightgray"),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, size=1.3),
        legend.text = element_text(size = 18),
        legend.position="none",
        legend.title = element_text(size = 19))
