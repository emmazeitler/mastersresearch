#### SET UP WORKSPACE ####
library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)
library(cowplot)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

db_data <- db_data %>% 
  filter(!burn_season == "Spring") %>% 
  mutate(env_type = str_replace(env_type, "BURN", "Burned"),
         env_type = str_replace(env_type, "SCRUB", "Unburned"))


db_data_2 <- db_data %>% 
  filter(norempair == 0)

#### Averages - Probability of Removal ####
modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type * burn_season + (1|block_id) + (1|pair_id), family=binomial)

remProb <- emmeans(modRem1, ~env_type | burn_season , type = "response") %>% as.data.frame()

names(remProb)

prob1 <- ggplot(data=remProb)+
  geom_point(aes(x = env_type, 
                 y = prob,
                 color = env_type),
             size = 4)+
  geom_errorbar(aes(x = env_type, 
                    y = prob,
                    ymin = asymp.LCL,
                    ymax = asymp.UCL,
                    color = env_type),
                width=0.1,
                lwd=1.5)+
  scale_color_manual(values=c("#d2601a", "#1d3c45"))+
  labs(x="Environment type",
       y="Probability of removal")+
  facet_wrap(~factor(burn_season,
                     levels=c("Summer",
                              "Fall",
                              "Winter")))+
  theme_bw()+
  theme(strip.text = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        axis.title = element_text(size = 20),
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
                                    fill=NA, size=1),
        legend.text = element_text(size = 18),
        legend.position="none",
        legend.title = element_text(size = 19))

prob1

####EffSize - Probability of Removal ####

remProb.or <- read_csv("02_Clean_data/propRem_or.csv")

prob2 <- ggplot(data=remProb.or)+
  geom_point(aes(x=factor(burn_season, 
                          level=c("Summer", "Fall", "Winter")), 
                 y=odds.ratio, color=burn_season),
             size = 5)+
  geom_errorbar(aes(x=factor(burn_season, 
                             level=c("Summer", "Fall", "Winter")), 
                    y=odds.ratio, 
                    ymin=lcl,
                    ymax=ucl,
                    color=burn_season),
                width=0.2,
                lwd=1.5) +
  scale_color_manual(values=c("#404040","#6b6b6b","black"))+
  scale_y_continuous(limits=c(-1, 6))+
  geom_hline(yintercept = 1)+
  labs(y="Odds ratio",
       x="Season of fire")+
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

prob2

##### cowplot ####

g1 <- plot_grid(prob1, 
                prob2,
                nrow=1, 
                align="v",
                labels = c('a)','b)'))
g1

ggsave("05_Figures/probrem_cowplot.jpg", height = 9, width = 15)

#### Averages - RemAmt ####

modRemNo <- glmmTMB(data = db_data_2, rem_no ~ env_type * burn_season + (1|block_id) + (1|pair_id), family= gaussian)

remNo <- emmeans(modRemNo, ~env_type | burn_season , type = "response") %>% as.data.frame()

names(remNo)

no1 <- ggplot(data = remNo)+
  geom_point(aes(x = env_type, 
                 y = emmean,
                 color = env_type),
             size=4)+
  geom_errorbar(aes(x = env_type, 
                    y = emmean,
                    ymin = lower.CL,
                    ymax = upper.CL,
                    color = env_type),
                width=0.2,
                lwd=1.5)+
  scale_color_manual(values=c("#d2601a", "#1d3c45"))+
  labs(x="Environment Type",
       y="Amount of dung removed")+
  facet_wrap(~factor(burn_season,
                     levels=c("Spring",
                              "Summer",
                              "Fall",
                              "Winter")))+
  theme_bw()+
  theme(strip.text = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        axis.title = element_text(size = 20),
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
                                    fill=NA, size=1),
        legend.text = element_text(size = 18),
        legend.position="none",
        legend.title = element_text(size = 19))



no1

#### EffSize - RemAmt ####

remno.lr <- read_csv("02_Clean_data/remNo_lrmod.csv")

remno_lr <- read_csv("02_Clean_data/remNo_lr_raw.csv")
remno_lr <- remno_lr %>% 
  filter(!burn.season == "Spring")

no2 <- ggplot() +
  geom_col(data=remno.lr,
             aes(x=factor(burn.season, level=c("Summer", "Fall", "Winter")), 
                 y=emmean, 
                 fill=burn.season))+
  geom_hline(yintercept = 0)+
  geom_jitter(data = remno_lr,
             aes(x = factor(burn.season, level=c("Summer", "Fall", "Winter")),
                 y = ln.ratio),
             alpha=0.5,
             width=0.05,
             size=2,
             color="blue")+
  scale_fill_manual(values=c("#404040","#6b6b6b","black"))+
  xlab("Season of burn")+
  ylab("Effect size (lrr)")+
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

no2

##### cowplot ####

g2 <- plot_grid(no1, 
                no2,
                nrow=1, 
                align="v",
                labels = c('a)','b)'))
g2

ggsave("05_Figures/norem_cowplot.jpg", height = 9, width = 15)

#### Averages - Latency ####
modLat <- glmmTMB(data = db_data_2, latency ~ env_type * burn_season + (1|block_id), family = gaussian)

Lat <- emmeans(modLat, ~env_type | burn_season, type = "response") %>% 
  as.data.frame()

lat1 <- ggplot(data = Lat)+
  geom_point(aes(x = env_type, 
                 y = emmean,
                 color = env_type),
             size=4)+
  geom_errorbar(aes(x = env_type, 
                    y = emmean,
                    ymin = lower.CL,
                    ymax = upper.CL,
                    color = env_type),
                width=0.2,
                lwd=1.5)+
  scale_color_manual(values=c("#d2601a", "#1d3c45"))+
  labs(x="Environment Type",
       y="Time until removal")+
  facet_wrap(~factor(burn_season,
                     levels=c("Spring",
                              "Summer",
                              "Fall",
                              "Winter")))+
  theme_bw()+
  theme(strip.text = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        axis.title = element_text(size = 20),
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
                                    fill=NA, size=1),
        legend.text = element_text(size = 18),
        legend.position="none",
        legend.title = element_text(size = 19))

lat1

#### Effsize - Latency ####
remlat_lr <- read_csv("02_Clean_data/remLat_lr_raw.csv")
remlat_lr <- remlat_lr %>% 
  filter(!burn.season == "Spring")

remlat.lr <- read_csv("02_Clean_data/remlat_lrmod.csv")

lat2 <- ggplot() +
  geom_col(data=remlat.lr,
             aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), 
                 y=emmean, 
                 fill=burn.season))+
  geom_hline(yintercept = 0)+
  geom_segment(aes(x = 1.7, y = 0, xend = 2.3, yend = 0),colour = "#404040", size=2)+
  geom_jitter(data = remlat_lr,
              aes(x = factor(burn.season, level=c("Summer", "Fall", "Winter")),
                  y = ln.ratio),
              alpha=0.5,
              width=0.05,
              size=2,
              color="blue")+
  scale_fill_manual(values=c("#404040","#6b6b6b","black"))+
  xlab("Season of burn")+
  ylab("Effect size (lrr)")+
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

lat2

#### Cowplot ####
g3 <- plot_grid(lat1, 
                lat2,
                nrow=1, 
                align="v",
                labels = c('a)','b)'))
g3

ggsave("05_Figures/lat_cowplot.jpg", height = 9, width = 15)
