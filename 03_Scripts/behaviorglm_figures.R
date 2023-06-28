library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")

db_data <- db_data %>% 
  filter(!burn_season == "Spring")

db_data_2 <- db_data %>% 
  filter(norempair == 0)

#### Probability of removal ####

modRem1 <- glmmTMB(data = db_data, rem_event ~ env_type * burn_season + (1|block_id) + (1|pair_id), family=binomial)

remProb <- emmeans(modRem1, ~env_type | burn_season , type = "response") %>% as.data.frame()

names(remProb)

ggplot(data=remProb)+
  geom_point(aes(x = env_type, 
                 y = prob,
                 color = env_type),
             size = 4)+
  geom_errorbar(aes(x = env_type, 
                y = prob,
                ymin = asymp.LCL,
                ymax = asymp.UCL,
                color = env_type),
                width=0.2)+
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

ggsave("05_Figures/remProb.jpg", height = 10, width = 10)

#### Amount of dung removed ####

modRemNo <- glmmTMB(data = db_data_2, rem_no ~ env_type * burn_season + (1|block_id) + (1|pair_id), family= gaussian)

remNo <- emmeans(modRemNo, ~env_type | burn_season , type = "response") %>% as.data.frame()

names(remNo)

ggplot(data = remNo)+
  geom_point(aes(x = env_type, 
                 y = emmean,
                 color = env_type),
             size=4)+
  geom_errorbar(aes(x = env_type, 
                     y = emmean,
                     ymin = lower.CL,
                     ymax = upper.CL,
                     color = env_type),
                 width=0.2)+
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

ggsave("05_Figures/remNo.jpg", height=10, width=10)

#### Latency ####

modLat <- glmmTMB(data = db_data_2, latency ~ env_type * burn_season + (1|block_id), family = gaussian)

Lat <- emmeans(modLat, ~env_type | burn_season, type = "response") %>% 
  as.data.frame()

emmeans(modLat, pairwise~env_type | burn_season, type = "response")

ggplot(data = Lat)+
  geom_point(aes(x = env_type, 
                 y = emmean,
                 color = env_type),
             size=4)+
  geom_errorbar(aes(x = env_type, 
                    y = emmean,
                    ymin = lower.CL,
                    ymax = upper.CL,
                    color = env_type),
                width=0.2)+
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
ggsave("05_Figures/remLat.jpg", height=10, width=10)
             