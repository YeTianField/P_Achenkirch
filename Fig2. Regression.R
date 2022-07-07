#####load packages
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(ggpubr)

#####import data
file <- file.choose()
achenkirch <- read.csv(file,header=T,dec=",",sep=";") ### for German system. English is dec="." and sep=","
achenkirch[achenkirch==""] <- NA
str(achenkirch) 

Dit_fe_ex <- expression(Dithionite~extracted~Fe~(mg~Fe~g~d.s.^-1))
cryst_fe_ex <- expression(Crystalline~Fe~(mg~Fe~g~d.s.^-1))
log_OlsenP_ex <- expression(log-transferred~Olsen~P~(µg~P~g~d.s.^-1))
TP_ex <- expression(TP~(g~P~kg~d.s.^-1))
sqrt_MBP_ex <- expression(sqrt-transferred~MBP~(µg~P~g~d.s.^-1))
TP_seasonal_ex <- expression(TP~(g~P~kg~d.s.^-1))
log_MBP_seasonal_ex <- expression(log-transferred~MBP~(µg~P~g~d.s.^-1))

achenkirch$Treatment <- c(rep(c("Warmed", "Control"), each = 12, times = 1))

#####figure making
p1 <- total_Fe_abio <- ggscatter(achenkirch, x="Fe_dithionite", y="abio_immobi",
                                 add = "reg.line", conf.int = TRUE,
                                 shape = "Treatment",
                                 cor.method = "pearson", cor.coef = TRUE,
                                 cor.coeff.args = list(label.x = 2.1, label.y = 80),
                                 add.params = list(fill="lightgrey"),
                                 ggtheme = theme_bw())+
  labs(x = Dit_fe_ex, 
       y = "Net abiotic immobilization (%)")


p1 <- p1 + theme(axis.title.x = element_text(size = 8))
p1 <- p1 + theme(axis.title.y = element_text(size = 8))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))                 
p1                

p2 <- total_Fe_OlsenP <- ggscatter(achenkirch, x="Fe_dithionite", y="OlsenP",
                                add = "reg.line", conf.int = TRUE,
                                add.params = list(fill="lightgrey"),
                                shape = "Treatment",
                                cor.method = "pearson", cor.coef = TRUE,
                                cor.coeff.args = list(label.x = 2,label.y = 0.71),
                                ggtheme = theme_bw())+
  labs(x = Dit_fe_ex, 
       y = log_OlsenP_ex)+
  scale_y_continuous(breaks = c(0.3,0.4,0.5,0.6,0.7))

p2 <- p2 + theme(axis.title.x = element_text(size = 8))
p2 <- p2 + theme(axis.title.y = element_text(size = 8))

p2 <- p2 + theme(
  legend.position = c(.97, .97),
  legend.justification = c("right", "top"),
  #legend.box.just = "right",
  legend.margin = margin(0.1, 6, 6, 0.1),
  legend.box.background = element_rect(color="black", size=0.5),
  #legend.box.margin = margin(2, 2, 2, 2),
  legend.title = element_blank())
p2 <- p2 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))   
p2 

p3 <- crys_Fe_abio <- ggscatter(achenkirch, x="Fe_crystalline", y="abio_immobi",
                                add = "reg.line", conf.int = TRUE,
                                add.params = list(fill="lightgrey"),
                                shape = "Treatment",
                                cor.method = "pearson", cor.coef = TRUE,
                                cor.coeff.args = list(label.x = 1.2,label.y = 80),
                                ggtheme = theme_bw())+
  labs(x = cryst_fe_ex, 
       y = "Net abiotic immobilization (%)")

p3 <- p3 + theme(axis.title.x = element_text(size = 8))
p3 <- p3 + theme(axis.title.y = element_text(size = 8))
p3 <- p3 + theme(legend.position = "none")
p3 <- p3 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black")) 
p3  


p4 <- crys_Fe_OlsenP <- ggscatter(achenkirch, x="Fe_crystalline", y="OlsenP",
                               add = "reg.line", conf.int = TRUE,
                               add.params = list(fill="lightgrey"),
                               shape = "Treatment",
                               cor.method = "pearson", cor.coef = TRUE,
                               cor.coeff.args = list(label.x = 1.2, label.y = 0.71),
                               ggtheme = theme_bw())+
  labs(x = cryst_fe_ex, 
       y = log_OlsenP_ex)+
  scale_y_continuous(breaks = c(0.3,0.4,0.5,0.6,0.7))

p4 <- p4 + theme(axis.title.x = element_text(size = 8))
p4 <- p4 + theme(axis.title.y = element_text(size = 8))
p4 <- p4 + theme(legend.position = "none")
p4 <- p4 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black")) 
p4 



p5 <- Dit_fe_MBP <- ggscatter(achenkirch, x="Fe_dithionite", y="MBP",
                              add = "reg.line", conf.int = TRUE,
                              add.params = list(fill="lightgrey"),
                              shape = "Treatment",
                              cor.method = "pearson", cor.coef = TRUE,
                              cor.coeff.args = list(label.x = 2.1, label.y = 8.55),
                              ggtheme = theme_bw())+
  labs(x = Dit_fe_ex, 
       y = sqrt_MBP_ex)

p5 <- p5 + theme(axis.title.x = element_text(size = 8))
p5 <- p5 + theme(axis.title.y = element_text(size = 8))
p5 <- p5 + theme(legend.position = "none")
p5 <- p5 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black")) 
p5 


p6 <- TP_MBP <- ggscatter(achenkirch, x="TP_seasonal", y="MBP_seasonal",
                          add = "reg.line", conf.int = TRUE,
                          add.params = list(fill="lightgrey"),
                          shape = "Treatment",
                          cor.method = "pearson", cor.coef = TRUE,
                          cor.coeff.args = list(label.x = 0.24, label.y = 1.95),
                          ggtheme = theme_bw())+
  labs(x = TP_seasonal_ex, 
       y = log_MBP_seasonal_ex)

p6 <- p6 + theme(axis.title.x = element_text(size = 8))
p6 <- p6 + theme(axis.title.y = element_text(size = 8))
p6 <- p6 + theme(legend.position = "none")
p6 <- p6 + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black")) 
p6 



figure <- ggarrange(p1, p2, p3, p4, p5, p6,
                    labels = c("a  ", "b  ", "c  ", "d  ", "e  ", "f  "),
                    font.label = list(size = 12),
                    ncol = 2, nrow = 3)


figure
#ggexport(figure, filename = "correlation.pdf")
ggsave("correlation.pdf", width = 18, height = 27, units = "cm")