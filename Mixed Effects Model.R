##### loading packages
library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(car)
library(carData)
library(histogram)
library(ggplot2)
library(multcomp) # for Tukey test

#################### mixed effects model for seasonal data from 2019 #################
##### importing data
mastersheet <- file.choose()
Achenkirch <- read.csv(mastersheet,header=T,dec=",",sep=";") ### This is for German system. English system is dec="." and sep=",".
Achenkirch[Achenkirch==""] <- NA
str(Achenkirch)
head(Achenkirch)# looking at the data 

##### setting variables as factors 
Achenkirch$Depth <- as.factor(Achenkirch$Depth)
Achenkirch[,"Block"] <- as.factor(Achenkirch[,"Block"])
Achenkirch$Start <- as.factor(Achenkirch$Start)
Achenkirch$Duration <- as.numeric(Achenkirch$Duration)

##### adding a column for date (numerical season) and for warming time (numerical time since warming started)
Achenkirch <- Achenkirch %>%
  mutate(Date = case_when(Season == "May" ~ 1,
                          Season == "Aug" ~ 2,
                          Season == "Oct" ~ 3))

##### checking homeogenity
bartlett.test(pH~Warming,Achenkirch)
bartlett.test(pH~Depth,Achenkirch)
bartlett.test(pH~Season,Achenkirch)   ### original

bartlett.test(sqrt(Achenkirch$pH)~Achenkirch$Warming) 
bartlett.test(sqrt(Achenkirch$pH)~Achenkirch$Depth) 
bartlett.test(sqrt(Achenkirch$pH)~Achenkirch$Season)   ### sqrt

bartlett.test(log(Achenkirch$pH)~Achenkirch$Warming)
bartlett.test(log(Achenkirch$pH)~Achenkirch$Depth)
bartlett.test(log(Achenkirch$pH)~Achenkirch$Season)  ### log

##### creating a window for figures, which check normal distribution
par(mfrow = c(3,7))
par("mar")
par(mar=c(1,1,1,1))
hist(Achenkirch$pH[Achenkirch$Warming=="warmed"],breaks = 10,xlab="pH", ylab="number", main = "warmed")
hist(Achenkirch$pH[Achenkirch$Warming=="control"],breaks = 10,xlab="pH", ylab="number", main = "control")
hist(Achenkirch$pH[Achenkirch$Depth=="10"],breaks = 10,xlab="pH", ylab="number", main = "10")
hist(Achenkirch$pH[Achenkirch$Depth=="20"],breaks = 10,xlab="pH", ylab="number", main = "20")
hist(Achenkirch$pH[Achenkirch$Season=="May"],breaks = 10,xlab="pH", ylab="number", main = "May")
hist(Achenkirch$pH[Achenkirch$Season=="Aug"],breaks = 10,xlab="pH", ylab="number", main = "Aug")
hist(Achenkirch$pH[Achenkirch$Season=="Oct"],breaks = 10,xlab="pH", ylab="number", main = "Oct")
boxplot(Achenkirch$pH[Achenkirch$Warming=="warmed"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Warming=="control"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Depth=="10"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Depth=="20"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Season=="May"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Season=="Aug"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Season=="Oct"], horizontal = TRUE)
qqnorm(Achenkirch$pH[Achenkirch$Warming=="warmed"])
qqline(Achenkirch$pH[Achenkirch$Warming=="warmed"])
qqnorm(Achenkirch$pH[Achenkirch$Warming=="control"])
qqline(Achenkirch$pH[Achenkirch$Warming=="control"])
qqnorm(Achenkirch$pH[Achenkirch$Depth=="10"])
qqline(Achenkirch$pH[Achenkirch$Depth=="10"])
qqnorm(Achenkirch$pH[Achenkirch$Depth=="20"])
qqline(Achenkirch$pH[Achenkirch$Depth=="20"])
qqnorm(Achenkirch$pH[Achenkirch$Season=="May"])
qqline(Achenkirch$pH[Achenkirch$Season=="May"])
qqnorm(Achenkirch$pH[Achenkirch$Season=="Aug"])
qqline(Achenkirch$pH[Achenkirch$Season=="Aug"])
qqnorm(Achenkirch$pH[Achenkirch$Season=="Oct"])
qqline(Achenkirch$pH[Achenkirch$Season=="Oct"])   ### original

par(mfrow = c(3,7))
par("mar")
par(mar=c(1,1,1,1))
hist(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="pH", ylab="number", main = "warmed")
hist(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]),breaks = 10,xlab="pH", ylab="number", main = "control")
hist(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]),breaks = 10,xlab="pH", ylab="number", main = "10")
hist(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]),breaks = 10,xlab="pH", ylab="number", main = "20")
hist(sqrt(Achenkirch$pH[Achenkirch$Season=="May"]),breaks = 10,xlab="pH", ylab="number", main = "May")
hist(sqrt(Achenkirch$pH[Achenkirch$Season=="Aug"]),breaks = 10,xlab="pH", ylab="number", main = "Aug")
hist(sqrt(Achenkirch$pH[Achenkirch$Season=="Oct"]),breaks = 10,xlab="pH", ylab="number", main = "Oct")
boxplot(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Season=="May"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Season=="Aug"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Season=="Oct"]), horizontal = TRUE)
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Season=="May"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Season=="May"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Season=="Aug"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Season=="Aug"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Season=="Oct"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Season=="Oct"]))   ### sqrt

par(mfrow = c(3,7))
par("mar")
par(mar=c(1,1,1,1))
hist(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="pH", ylab="number", main = "warmed")
hist(log(Achenkirch$pH[Achenkirch$Warming=="control"]),breaks = 10,xlab="pH", ylab="number", main = "control")
hist(log(Achenkirch$pH[Achenkirch$Depth=="10"]),breaks = 10,xlab="pH", ylab="number", main = "10")
hist(log(Achenkirch$pH[Achenkirch$Depth=="20"]),breaks = 10,xlab="pH", ylab="number", main = "20")
hist(log(Achenkirch$pH[Achenkirch$Season=="May"]),breaks = 10,xlab="pH", ylab="number", main = "May")
hist(log(Achenkirch$pH[Achenkirch$Season=="Aug"]),breaks = 10,xlab="pH", ylab="number", main = "Aug")
hist(log(Achenkirch$pH[Achenkirch$Season=="Oct"]),breaks = 10,xlab="pH", ylab="number", main = "Oct")
boxplot(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Depth=="20"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Season=="May"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Season=="Aug"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Season=="Oct"]), horizontal = TRUE)
qqnorm(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqline(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqline(log(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqline(log(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Depth=="20"]))
qqline(log(Achenkirch$pH[Achenkirch$Depth=="20"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Season=="May"]))
qqline(log(Achenkirch$pH[Achenkirch$Season=="May"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Season=="Aug"]))
qqline(log(Achenkirch$pH[Achenkirch$Season=="Aug"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Season=="Oct"]))
qqline(log(Achenkirch$pH[Achenkirch$Season=="Oct"]))   ### log

# lmer model with warming time also as a random effect

lmer_pH <-  lmer(pH ~ Warming*Depth*Season + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_pH)
anova(lmer_pH)
tukey_lmer_pH <- glht(lmer_pH, linfct = mcp(Season = "Tukey"))
summary(tukey_lmer_pH)
cld(tukey_lmer_pH)   ### original

lmer_pH_sqrt <-  lmer(sqrt(pH) ~ Warming*Depth*Season + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_pH_sqrt)
anova(lmer_pH_sqrt)
tukey_lmer_pH_sqrt <- glht(lmer_pH_sqrt, linfct = mcp(Season = "Tukey"))
summary(tukey_lmer_pH_sqrt)
cld(tukey_lmer_pH_sqrt)   ### sqrt

lmer_pH_log <-  lmer(log(pH) ~ Warming*Depth*Season + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_pH_log)
anova(lmer_pH_log)
tukey_lmer_pH_log <- glht(lmer_pH_log, linfct = mcp(Season = "Tukey"))
summary(tukey_lmer_pH_log)
cld(tukey_lmer_pH_log)   ### log


#################### mixed effects model for data from 2020 #########################
##### checking homeogenity
bartlett.test(pH~Warming,Achenkirch)
bartlett.test(pH~Depth,Achenkirch)  ### original

bartlett.test(sqrt(Achenkirch$pH)~Achenkirch$Warming) 
bartlett.test(sqrt(Achenkirch$pH)~Achenkirch$Depth)    ### sqrt

bartlett.test(log(Achenkirch$pH)~Achenkirch$Warming)
bartlett.test(log(Achenkirch$pH)~Achenkirch$Depth)    ### log

##### creating a window for figures, which check normal distribution
par(mfrow = c(3,4))
par("mar")
par(mar=c(1,1,1,1))
hist(Achenkirch$pH[Achenkirch$Warming=="warmed"],breaks = 10,xlab="pH", ylab="number", main = "warmed")
hist(Achenkirch$pH[Achenkirch$Warming=="control"],breaks = 10,xlab="pH", ylab="number", main = "control")
hist(Achenkirch$pH[Achenkirch$Depth=="10"],breaks = 10,xlab="pH", ylab="number", main = "10")
hist(Achenkirch$pH[Achenkirch$Depth=="20"],breaks = 10,xlab="pH", ylab="number", main = "20")
boxplot(Achenkirch$pH[Achenkirch$Warming=="warmed"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Warming=="control"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Depth=="10"], horizontal = TRUE)
boxplot(Achenkirch$pH[Achenkirch$Depth=="20"], horizontal = TRUE)
qqnorm(Achenkirch$pH[Achenkirch$Warming=="warmed"])
qqline(Achenkirch$pH[Achenkirch$Warming=="warmed"])
qqnorm(Achenkirch$pH[Achenkirch$Warming=="control"])
qqline(Achenkirch$pH[Achenkirch$Warming=="control"])
qqnorm(Achenkirch$pH[Achenkirch$Depth=="10"])
qqline(Achenkirch$pH[Achenkirch$Depth=="10"])
qqnorm(Achenkirch$pH[Achenkirch$Depth=="20"])
qqline(Achenkirch$pH[Achenkirch$Depth=="20"])  ### original

par(mfrow = c(3,4))
par("mar")
par(mar=c(1,1,1,1))
hist(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="pH", ylab="number", main = "warmed")
hist(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]),breaks = 10,xlab="pH", ylab="number", main = "control")
hist(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]),breaks = 10,xlab="pH", ylab="number", main = "10")
hist(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]),breaks = 10,xlab="pH", ylab="number", main = "20")
boxplot(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]), horizontal = TRUE)
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqnorm(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"]))
qqline(sqrt(Achenkirch$pH[Achenkirch$Depth=="20"])) ### sqrt

par(mfrow = c(3,4))
par("mar")
par(mar=c(1,1,1,1))
hist(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="pH", ylab="number", main = "warmed")
hist(log(Achenkirch$pH[Achenkirch$Warming=="control"]),breaks = 10,xlab="pH", ylab="number", main = "control")
hist(log(Achenkirch$pH[Achenkirch$Depth=="10"]),breaks = 10,xlab="pH", ylab="number", main = "10")
hist(log(Achenkirch$pH[Achenkirch$Depth=="20"]),breaks = 10,xlab="pH", ylab="number", main = "20")
boxplot(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(log(Achenkirch$pH[Achenkirch$Depth=="20"]), horizontal = TRUE)
qqnorm(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqline(log(Achenkirch$pH[Achenkirch$Warming=="warmed"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqline(log(Achenkirch$pH[Achenkirch$Warming=="control"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqline(log(Achenkirch$pH[Achenkirch$Depth=="10"]))
qqnorm(log(Achenkirch$pH[Achenkirch$Depth=="20"]))
qqline(log(Achenkirch$pH[Achenkirch$Depth=="20"]))  ### log

# lmer model with warming time also as a random effect

lmer_pH <-  lmer(pH ~ Warming*Depth + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_pH)
anova(lmer_pH)  ### original

lmer_pH_sqrt <-  lmer(sqrt(pH) ~ Warming*Depth + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_pH_sqrt)
anova(lmer_pH_sqrt)  ### sqrt

lmer_pH_log <-  lmer(log(pH) ~ Warming*Depth + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_pH_log)
anova(lmer_pH_log)   ### log