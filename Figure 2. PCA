#####load package
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
pca <- PCA(data_pca, scale.unit = TRUE, ncp = 5, graph = TRUE) ##### remember to change scale.unit =TRUE, if the data hasn't been standardized

#####PCA biplox
p1 <- fviz_pca_biplot(pca,
                      # Fill individuals by groups
                      addEllipses = T,
                      geom.ind = "point",
                      pointshape = 21,
                      pointsize = 0.4,
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

#control x and y axis labelling
p1$labels[[2]] <- "PC1 (51.22 %)"
p1$labels[[3]] <- "PC2 (28.14 %)"

#control labels
p1$layers[[6]]$aes_params$size <- 1.9
p1$layers[[6]]$aes_params$alpha <- 1

#control treatments
p1$layers[[3]]$aes_params$size <- 1.5

#control arrows
p1$layers[[7]]$aes_params$linetype <- "solid"
p1$layers[[7]]$aes_params$alpha <- 0.5
p1$layers[[7]]$aes_params$size <- 0.2

#control dotted lines
p1$layers[[4]]$aes_params$size <- 0.1
p1$layers[[5]]$aes_params$size <- 0.1
p1$layers[[8]]$aes_params$size <- 0.1
p1$layers[[9]]$aes_params$size <- 0.1

#control ellipses
p1$layers[[2]]$aes_params$alpha <- 0.23

#change font size and remove background colors and grid
p1 <- p1 + theme_bw() + 
  theme(axis.line = element_line(color='black', size = 0.1),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 5),
        legend.text = element_text(size = 5),
        title = element_text(size = 5))

p1

ggsave("Figure 2_PCA.pdf", width = 8.8, height = 6.5, units = "cm")

