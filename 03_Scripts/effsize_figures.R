## RemNo ##

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

#### Latency ####

firecolors1 <- c("#FE8116", "#FDA766","#FFA80F", "#FE5A1D")

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
