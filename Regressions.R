##### loading packages
library("PerformanceAnalytics")
library(ggplot2)
library(ggpubr)

################## regression for seasonal data from 2019 ####################
### importing data
file_2019 <- file.choose()
achenkirch_2019 <- read.csv(file_2019,header=T,dec=",",sep=";") ### for German system。 German is dec="." and sep=","
achenkirch_2019[achenkirch_2019==""] <- NA
str(achenkirch_2019) 

### checking assumptions
bartlett.test(root_total_P,achenkirch_2019)  ### original
bartlett.test(sqrt(achenkirch_2019$root_total_P)~achenkirch_2019)    ### sqrt
bartlett.test(log(achenkirch_2019$root_total_P)~achenkirch_2019)    ### log

par(mfrow = c(3,1))
par("mar")
hist(achenkirch_2019$root_total_P,breaks = 10,xlab="root_total_P", ylab="number")
boxplot(achenkirch_2019$root_total_P, horizontal = TRUE)
qqnorm(achenkirch_2019$root_total_P)
qqline(achenkirch_2019$root_total_P) ### original

par(mfrow = c(3,1))
par("mar")
hist(sqrt(achenkirch_2019$root_total_P),breaks = 10,xlab="root_total_P", ylab="number")
boxplot(sqrt(achenkirch_2019$root_total_P), horizontal = TRUE)
qqnorm(sqrt(achenkirch_2019$root_total_P))
qqline(sqrt(achenkirch_2019$root_total_P)) ### sqrt

par(mfrow = c(3,1))
par("mar")
hist(log(achenkirch_2019$root_total_P),breaks = 10,xlab="root_total_P", ylab="number")
boxplot(log(achenkirch_2019$root_total_P), horizontal = TRUE)
qqnorm(log(achenkirch_2019$root_total_P))
qqline(log(achenkirch_2019$root_total_P)) ### log

### importing normalized data
file_2019_normalized <- file.choose()
achenkirch_2019_normalized <- read.csv(file_2019_normalized,header=T,dec=",",sep=";") 
achenkirch_2019_normalized[achenkirch_2019_normalized==""] <- NA
str(achenkirch_2019_normalized) 

### runing regression
figure_2019 <- chart.Correlation(achenkirch_2019_normalized, histogram=TRUE, pch=19)
figure_2019


############### regression betweeen averaged seasonal data from 2019 and data from 2020 ####################
### import data
file_2020 <- file.choose()
achenkirch_2020 <- read.csv(file_2020,header=T,dec=",",sep=";") 
achenkirch_2020[achenkirch_2020==""] <- NA
str(achenkirch_2020)  

### check assumptions for averaged seasonal data

bartlett.test(root_total_P,achenkirch_2020)  ### original
bartlett.test(sqrt(achenkirch_2020$root_total_P)~achenkirch_2020)    ### sqrt
bartlett.test(log(achenkirch_2020$root_total_P)~achenkirch_2020)    ### log

par(mfrow = c(3,1))
par("mar")
hist(achenkirch_2020$root_total_P,breaks = 10,xlab="root_total_P", ylab="number")
boxplot(achenkirch_2020$root_total_P, horizontal = TRUE)
qqnorm(achenkirch_2020$root_total_P)
qqline(achenkirch_2020$root_total_P) ### original

par(mfrow = c(3,1))
par("mar")
hist(sqrt(achenkirch_2020$root_total_P),breaks = 10,xlab="root_total_P", ylab="number")
boxplot(sqrt(achenkirch_2020$root_total_P), horizontal = TRUE)
qqnorm(sqrt(achenkirch_2020$root_total_P))
qqline(sqrt(achenkirch_2020$root_total_P)) ### sqrt

par(mfrow = c(3,1))
par("mar")
hist(log(achenkirch_2020$root_total_P),breaks = 10,xlab="root_total_P", ylab="number")
boxplot(log(achenkirch_2020$root_total_P), horizontal = TRUE)
qqnorm(log(achenkirch_2020$root_total_P))
qqline(log(achenkirch_2020$root_total_P)) ### log

### importing normalized data
file_2020_normalized <- file.choose()
achenkirch_2020_normalized <- read.csv(file_2020_normalized,header=T,dec=",",sep=";") 
achenkirch_2020_normalized[achenkirch_2020_normalized==""] <- NA
str(achenkirch_2020_normalized) 

### run regression
figure_2020 <- chart.Correlation(achenkirch_2020_normalized, histogram=TRUE, pch=19)
figure_2020