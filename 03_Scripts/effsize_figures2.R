#### SET UP WORKSPACE ####

library(tidyverse)
library(cowplot)

#### Probability of Removal ####

seasoncolors1 <- c('#40394A','#845EC2','#242424')

remProb.or <- read_csv("02_Clean_data/propRem_or.csv")

ggplot(data=remProb.or)+
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
                width=.07,
                size=1) +
  scale_color_manual(values=seasoncolors1)+
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

# ggsave("05_Figures/effsize_remProb_lo.jpg", height=10, width= 10)

#### Removal Number ####

remno.lr <- read_csv("02_Clean_data/remNo_lrmod.csv")

ggplot() +
  geom_point(data=remno.lr,
           aes(x=factor(burn.season, level=c("Summer", "Fall", "Winter")), 
               y=emmean, 
               color=burn.season),
           size=5)+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  geom_errorbar(data=remno.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL,
                    color=burn.season),
                width=.07,
                size = 1)+
  scale_color_manual(values=seasoncolors1)+
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

#### Latency ####

remlat.lr <- read_csv("02_Clean_data/remlat_lrmod.csv")

ggplot() +
  geom_point(data=remlat.lr,
           aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), 
               y=emmean, 
               color=burn.season),
           size=5)+
  geom_errorbar(data=remlat.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL,
                    color=burn.season),
                width=.09,
                lwd=1,
                position=position_dodge(width=0.5))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  scale_color_manual(values=seasoncolors1)+
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

ggsave("05_Figures/effsize_remLat_lr.jpg", height=10, width= 10)

#### Cowplot ####

remProb.or <- read_csv("02_Clean_data/propRem_or.csv")

p1 <- ggplot(data=remProb.or)+
  geom_hline(yintercept = 1)+
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
                width=0.5,
                lwd=1.5) +
  scale_color_manual(values=seasoncolors1,
                     breaks = c('Summer', 'Fall', 'Winter'))+
  scale_y_continuous(limits=c(-2, 6))+
  labs(y="Odds ratio")+
  theme_bw()+
  theme(axis.title = element_text(size = 26,face = "bold"),
        axis.text = element_text(color = "black",
                                   size = 22),
        axis.title.x = element_blank(),
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

##
seasoncolors1 <- c('#a32cc4','#67032f','#311432')
remno.lr <- read_csv("02_Clean_data/remNo_lrmod.csv")

p2 <- ggplot() +
  geom_hline(yintercept = 0,
             linetype = "solid")+
  geom_point(data=remno.lr,
             aes(x=factor(burn.season, level=c("Summer", "Fall", "Winter")), 
                 y=emmean, 
                 color=burn.season),
             size=5)+
  geom_errorbar(data=remno.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL,
                    color=burn.season),
                width=0.5,
                lwd = 1.5)+
  scale_color_manual(values=seasoncolors1,
                     breaks = c('Summer', 'Fall', 'Winter'))+
  ylab("Effect size (lrr)")+
  labs(color="Season of fire")+
  theme_bw()+
  theme(axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(color = "black",
                                   size = 22),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5,
                                          colour = "gray"),
        panel.grid.minor.y = element_line(linetype = 2,
                                          color = "lightgray"),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, size=1.3),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 19))
p2

##
remlat.lr <- read_csv("02_Clean_data/remlat_lrmod.csv")

p3 <- ggplot() +
  geom_hline(yintercept = 0,
             linetype = "solid")+
  geom_point(data=remlat.lr,
             aes(x=factor(burn.season, level=c("Spring", "Summer", "Fall", "Winter")), 
                 y=emmean, 
                 color=burn.season),
             size=7)+
  geom_errorbar(data=remlat.lr, 
                aes(x=burn.season, 
                    y=emmean, 
                    ymin=lower.CL,
                    ymax=upper.CL,
                    color=burn.season),
                width=0.5,
                lwd=1.5,
                position=position_dodge(width=0.5))+
  scale_color_manual(values=seasoncolors1,
                     breaks = c('Summer', 'Fall', 'Winter'))+
  xlab("Season of burn")+
  ylab("Effect size (lrr)")+
  theme_bw()+
  theme(axis.title = element_text(size = 26, face = "bold"),
        axis.text.y = element_text(color = "black",
                                   size = 22),
        axis.text.x = element_text(color = "black",
                                   size = 22),
        axis.title.x = element_blank(),
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

p3

##
g1 <- plot_grid(p1, 
                p2+ theme(legend.position="none"),
                p3,
                nrow=1, 
                align="vh",
                hjust = -1)
g1

##

legend <- get_legend(
  p2 + theme(legend.box.margin = margin(0, 0, 0, 12))
)

##

g2 <-plot_grid(g1, legend, rel_widths = c(3, .4))

g2

ggsave("05_Figures/effsize_cowplot.jpg", height = 9, width = 15)
