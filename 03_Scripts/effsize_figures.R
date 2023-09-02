#### SET UP WORKSPACE ####

library(tidyverse)
library(ggplot2)

firecolors1 <- c("#FE8116", "#FDA766","#FFA80F", "#FE5A1D")


#### Probability of removal - Cohen's h ####

probRem_es <- read_csv("02_Clean_data/propRem_ch.csv")

names(probRem_es)

ggplot(data = probRem_es) +
  geom_point(aes(x=factor(burn_season, 
                          level=c("Spring", "Summer", "Fall", "Winter")),
                 y=cohen.h, color=burn_season),
             size=5)+
  geom_hline(yintercept = 0,
             linetype="dashed")+
  scale_color_manual(values=firecolors1)+
  labs(x="Season of fire",
       y="Effect size (cohens h)",
       title = "Probability of removal")+
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

# ggsave("05_Figures/effsize_remProb_ch.jpg", height=10, width= 10)

#### Probability of Removal - Odds Ratio ####

seasoncolors1 <- c('#40394A','#845EC2','#242424')

remProb.or <- read_csv("02_Clean_data/propRem_or.csv")

ggplot(data=remProb.or)+
  geom_col(aes(x=factor(burn_season, 
                          level=c("Summer", "Fall", "Winter")), 
                 y=odds.ratio, fill=burn_season),
             size = 5)+
  geom_errorbar(aes(x=factor(burn_season, 
                             level=c("Summer", "Fall", "Winter")), 
                    y=odds.ratio, 
                    ymin=lcl,
                    ymax=ucl,
                    fill=burn_season),
                width=.02) +
  scale_fill_manual(values=seasoncolors1)+
  scale_y_continuous(limits=c(-1, 6))+
  geom_hline(yintercept = 0)+
  labs(y="Odds ratio",
       x="Season of fire",
       title="Probability of removal")+
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
  
# ggsave("05_Figures/effsize_remProb_lo.jpg", height=10, width= 10)

#### Amount of dung removed - Log Ratio ####
remno.lr <- read_csv("02_Clean_data/remNo_lrmod.csv")

ggplot() +
  geom_col(data=remno.lr,
           aes(x=factor(burn.season, level=c("Summer", "Fall", "Winter")), 
               y=emmean, 
               fill=burn.season))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  geom_errorbar(data=remno.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL),
                width=.02,
                color="black")+
  scale_fill_manual(values=seasoncolors1)+
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

ggsave("05_Figures/effsize_remNo_lr.jpg", height=10, width= 10)

#### Amount of dung removed - Cohen's d ####
noRem_cd <- read_csv("02_Clean_data/remno_cd.csv")

ggplot(data=noRem_cd)+
  geom_col(aes(x=factor(burn_season, 
                          level=c("Spring", "Summer", "Fall", "Winter")),
                 y=Estimate,
                 fill=burn_season))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  geom_errorbar(data=noRem_cd, 
                aes(x=burn_season, 
                    y=Estimate, 
                    ymin=lcl,
                    ymax=ucl),
                color="black",
                width=0.2)+
  scale_fill_manual(values=firecolors1)+
  labs(x="Season of fire",
       y="Effect size (Cohen's d)",
       title = "Amount of dung removed")+
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

ggsave("05_Figures/effsize_remNo_cd.jpg", height=10, width= 10)

#### Latency - Log ratio ####
seasoncolors1 <- c('#40394A','#845EC2','#242424')
remlat.lr <- read_csv("02_Clean_data/remlat_lrmod.csv")

ggplot() +
  geom_col(data=remlat.lr,
           aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), y=emmean, fill=burn.season))+
  geom_errorbar(data=remlat.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL,
                    color=burn.season),
                width=.09,
                lwd=1,
                position=position_dodge(width=0.5))+
  scale_fill_manual(values=seasoncolors1)+
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

ggsave("05_Figures/effsize_remLat_lr.jpg", height=10, width= 10)

#### Latency - Cohen's d ####

lat_cd <- read_csv("02_Clean_data/lat_cd.csv")

ggplot(data=lat_cd)+
  geom_col(aes(x=factor(burn_season,
                           levels=c("Summer", "Fall", "Winter")),
                 y=Estimate,
                 fill=burn_season))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  geom_errorbar(aes(x=burn_season, 
                    y=Estimate, 
                    ymin=lcl,
                    ymax=ucl),
                color="black",
                width=0.2)+
  scale_fill_manual(values=firecolors1)+
  labs(x="Season of fire",
       y="Effect size (Cohen's d)",
       title = "Time until dung removal")+
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

# ggsave("05_Figures/effsize_remLat_cd.jpg", height=10, width= 10)

#### Removal Odds Ratio + Removal Amount Effect Size ####

## Odds Ratio

seasoncolors1 <- c('#845EC2','#40394A','#242424')

remProb.or <- read_csv("02_Clean_data/propRem_or.csv")

p1 <- ggplot(data=remProb.or)+
  geom_col(aes(x=factor(burn_season, 
                        levels=c("Summer", "Fall", "Winter")), 
               y=odds.ratio, fill=burn_season),
           size = 5)+
  geom_errorbar(aes(x=factor(burn_season, 
                             levels=c("Summer", "Fall", "Winter")), 
                    y=odds.ratio, 
                    ymin=lcl,
                    ymax=ucl),
                color="black",
                width=.02) +
  scale_fill_manual(values=c('#40394A', '#845EC2', '#242424'))+
  scale_y_continuous(limits=c(-1, 6))+
  geom_hline(yintercept = 0,
             linetype="dashed")+
  labs(y="Odds ratio of removal probability",
       x="Season of fire")+
  theme_bw()+
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(color = "black",
                                   size = 18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
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
p1

## Removal amount

remno.lr <- read_csv("02_Clean_data/remNo_lrmod.csv")

p2 <- ggplot() +
  geom_col(data=remno.lr,
           aes(x=factor(burn.season, 
                        levels=c("Summer", "Fall", "Winter")), 
               y=emmean, 
               fill=burn.season))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  geom_errorbar(data=remno.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL),
                width=.02,
                color="black")+
  scale_fill_manual(values=c('#845EC2','#40394A', '#242424'),
                    breaks=c('Summer', 'Fall', 'Winter'))+
  xlab("Season of fire")+
  ylab("Effect size (lrr) of removal amount")+
  theme_bw()+
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(color = "black",
                                   size = 18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5,
                                          colour = "gray"),
        panel.grid.minor.y = element_line(linetype = 2,
                                          color = "lightgray"),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, size=1.3),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

p2 


## Together

g1 <- plot_grid(p1, 
                p2 + theme(legend.position="none"), 
                nrow=1, 
                align="vh",
                hjust = -1)
g1

# extract legend 

legend <- get_legend(
  p2 + theme(legend.box.margin = margin(0, 0, 0, 12))
)

# cowplot with legend 

g2 <-plot_grid(g1, legend, rel_widths = c(3, .4))

g2

ggsave("05_Figures/effsize_cowplot.jpg", height = 7, width = 15)
