##### load packages
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(ggpubr)

##### import data
file <- file.choose()
achenkirch <- read.csv(file,header=T,dec=",",sep=";") ### for German system. English is dec="." and sep=","
achenkirch[achenkirch==""] <- NA
str(achenkirch) 

##### control axis title&legend
Dit_fe_ex <- expression(Dithionite~extracted~Fe~(mg~Fe~g~d.s.^-1))
log_TDP_ex <- expression(log-transformed~Olsen~total~P~(g~P~g~d.s.^-1))
TP_ex <- expression(TP~(g~P~kg~d.s.^-1))
sqrt_MBP_ex <- expression(sqrt-transformed~MBP~(µg~P~g~d.s.^-1))
TP_seasonal_ex <- expression(TP~(g~P~kg~d.s.^-1))
log_MBP_seasonal_ex <- expression(log-transformed~MBP~(µg~P~g~d.s.^-1))

achenkirch$Treatment <- c(rep(c("Warmed", "Control"), each = 12, times = 1))

##### sub-fig 1
p1 <- total_Fe_abio <- ggscatter(achenkirch, x="Fe_dithionite", y="abio_immobi",
                           add = "reg.line", conf.int = TRUE,
                           shape = "Treatment", size = 1.5,
                           cor.method = "pearson", cor.coef = F,
                           #cor.coeff.args = list(label.x = 2.1, label.y = 80, size = 7),
                           add.params = list(fill="lightgrey"),
                           ggtheme = theme_bw())+
  labs(x = Dit_fe_ex, 
       y = "Net abiotic immobilization (%)")+
  annotate(geom= "text", 
           x = 2.5, y = 80, 
           label = c(as.expression(bquote(italic(R^2) ~ "= 0.61 , " ~ italic(p) ~ "= 0.0016"))), 
           color = "black", size = 2)

p1 <- p1 + theme(axis.title.x = element_text(size = 7))
p1 <- p1 + theme(axis.title.y = element_text(size = 7))
p1 <- p1 + theme(axis.text.x = element_text(size = 7))
p1 <- p1 + theme(axis.text.y = element_text(size = 7))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 +theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"))                 
p1

##### sub-fig 2           
p2 <- total_Fe_TDP <- ggscatter(achenkirch, x="Fe_dithionite", y="TDP",
                          add = "reg.line", conf.int = TRUE,
                          add.params = list(fill="lightgrey"),
                          shape = "Treatment", size = 1.5,
                          cor.method = "pearson", cor.coef = F,
                          #cor.coeff.args = list(label.x = 2,label.y = 0.71),
                          ggtheme = theme_bw())+
  labs(x = Dit_fe_ex, 
       y = log_TDP_ex)+
  annotate(geom= "text", 
           x = 2.55, y = 0.71, 
           label = c(as.expression(bquote(italic(R^2) ~ "= -0.46 , "~ italic(p) ~ "= 0.024"))), 
           color = "black", size = 2)+
  scale_y_continuous(breaks = c(0.3,0.4,0.5,0.6,0.7))

p2 <- p2 + theme(axis.title.x = element_text(size = 7))
p2 <- p2 + theme(axis.title.y = element_text(size = 7))
p2 <- p2 + theme(axis.text.x = element_text(size = 7))
p2 <- p2 + theme(axis.text.y = element_text(size = 7))

p2 <- p2 + theme(
  legend.position = c(.97, .97),
  legend.justification = c("right", "top"),
  legend.margin = margin(0.1, 6, 6, 0.1),
  legend.box.background = element_rect(color="black", size=0.5),
  legend.text = element_text(size = 7),
  legend.title = element_blank())
p2 <- p2 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))   
p2 


##### sub-fig 3
p3 <- Dit_fe_MBP <- ggscatter(achenkirch, x="Fe_dithionite", y="MBP",
                               add = "reg.line", conf.int = TRUE,
                               add.params = list(fill="lightgrey"),
                               shape = "Treatment", size = 1.5,
                               cor.method = "pearson", cor.coef = F,
                               #cor.coeff.args = list(label.x = 2.1, label.y = 8.55),
                               ggtheme = theme_bw())+
  labs(x = Dit_fe_ex, 
       y = sqrt_MBP_ex)+
  annotate(geom= "text", 
           x = 2.5, y = 8.55, 
           label = c(as.expression(bquote(italic(R^2) ~ "= -0.44 , "~ italic(p) ~ " = 0.03"))), 
           color = "black", size = 2)

p3 <- p3 + theme(axis.title.x = element_text(size = 7))
p3 <- p3 + theme(axis.title.y = element_text(size = 7))
p3 <- p3 + theme(axis.text.x = element_text(size = 7))
p3 <- p3 + theme(axis.text.y = element_text(size = 7))
p3 <- p3 + theme(legend.position = "none")
p3 <- p3 +theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black")) 
p3 

##### sub-fig 4
p4 <- TP_MBP <- ggscatter(achenkirch, x="TP_seasonal", y="MBP_seasonal",
                          add = "reg.line", conf.int = TRUE,
                          add.params = list(fill="lightgrey"),
                          shape = "Treatment", size = 1.5,
                          cor.method = "pearson", cor.coef = F,
                          #cor.coeff.args = list(label.x = 0.24, label.y = 1.95),
                          ggtheme = theme_bw())+
  labs(x = TP_seasonal_ex, 
       y = log_MBP_seasonal_ex)+
  annotate(geom= "text", 
           x = 0.4, y = 1.95, 
           label = expression(italic(R^2) == "-0.75" ~ ", " ~ italic(p) == 3.6 * e^-14), 
           color = "black", size = 2, parse = TRUE)

p4 <- p4 + theme(axis.title.x = element_text(size = 7))
p4 <- p4 + theme(axis.title.y = element_text(size = 7))
p4 <- p4 + theme(axis.text.x = element_text(size = 7))
p4 <- p4 + theme(axis.text.y = element_text(size = 7))
p4 <- p4 + theme(legend.position = "none")
p4 <- p4 + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black")) 
p4 

figure <- ggarrange(p1, p2, p3, p4,
                    labels = c("a  ", "b  ", "c  ", "d  "),
                    font.label = list(size = 7),
                    ncol = 2, nrow = 2)


figure

##### export figure
ggsave("Figure 3_Correlations.pdf", width = 18, height = 18, units = "cm")
