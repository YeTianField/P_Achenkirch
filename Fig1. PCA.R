#####load packages
library(FactoMineR)
library(factoextra)
library(ade4)
library(ExPosition)
library(corrplot)
library(dplyr)
library(ggpubr)

#####import data
data_pca=read.csv(file.choose(),dec=",",sep=";",row.names=1)
property=read.csv(file.choose(),dec=",",sep=";",row.names=1)
group_pca=read.csv(file.choose(),dec=",",sep=";", row.names=1)

#####check data
str(data_pca)
str(property)
str(group_pca)

property <- property %>%
  mutate(Treatments = case_when(Warming == "warmed" ~ 1,
                                Warming == "control" ~ 2))

property$Warming <- factor(property$Warming, levels = c("warmed", "control"))

names(data_pca)[1:9] <- c("total soil P", "microbial biomass P", "net abiotic immobilization",
                          "net biotic immobilization", "gross Pi mobilization", "total Fe oxide",
                          "sand content (%)", "clay content (%)", "exchangeable Ca++")

#####run PCA and check
pca <- PCA(data_pca, scale.unit = TRUE, ncp = 5, graph = TRUE) 

#####PCA biplox

p1 <- fviz_pca_biplot(pca,
                      # Fill individuals by groups
                      addEllipses = T,
                      geom.ind = "point",
                      pointshape = 21,
                      pointsize = 2.5,
                      fill.ind = property$Warming,
                      show.legend = F,
                      col.ind = "black",
                      # Color variable by groups
                      col.var = factor(c("P pools", "P pools", "P processes", 
                                         "P processes","P processes","Metal oxides",
                                         "Soil texture","Soil texture","Metal oxides")),
                      
                      legend.title = list(fill = "Treatments", color = "Groups"),
                      palette = c("#000000", "#D55E00", "#009E73","#CC79A7"),
                      title = NULL,
                      repel = TRUE)+
  ggpubr::fill_palette(c("#F0E442","#56B4E9"))


p1$labels[[2]] <- "PC1 (51.22 %)"
p1$labels[[3]] <- "PC2 (28.14 %)"
p1$layers[[6]]$aes_params$size <- 3.8
p1$layers[[6]]$aes_params$fontface <- "italic"
p1$layers[[6]]$aes_params$alpha <- 1
p1$layers[[7]]$aes_params$linetype <- "solid"
p1$layers[[7]]$aes_params$alpha <- 0.5
p1$layers[[7]]$aes_params$size <- 1
p1$layers[[2]]$aes_params$alpha <- 0.13


p1 <- p1 + theme_bw() + theme(axis.line = element_line(color='black'),
                              plot.background = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.grid.major = element_blank())

p1

ggsave("PCA_plot.pdf", width = 18, height = 15, units = "cm")