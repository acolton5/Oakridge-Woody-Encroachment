# Oakridge woody encroachment project
# Analysis of height by diversity trt and herbivore trt ##

#### General notes about next steps ####
#in munging script, add NA's for all blank cells (read.csv(file, na.strings=c("", "NA", "na")))
#make decision about LD/edge, LD/HD, 

#in raw data
#figure out what white mulberry is. 
#figure out seedling 155 (if no data, then put NA's all across)
#distinguish between couldn't identify, and NA (meaning not a seedling to be included in study) in genus and species

#how much area is in LD vs HD? (2x as much!)

#########################
#load libraries
library(lsmeans)
library(lme4)
library(ggplot2)
library(car)

#load data
setwd("Data/Tidy")
Seedling <- read.csv("SeedlingData_Tidy.csv", na.strings=c("", "NA", "na"))

#explore data
str(Seedling)
head(Seedling)
summary(Seedling)
Seedling$Height<-as.numeric(Seedling$Height)

###### Data Exploration ############

##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers

hist(Seedling$Height, ylab = "Heights")
#above result in boxplot of heights, no outliers, skewed left(?), most heights short? 

#separating by block, there are outliers in block 1 and 7 
boxplot(Seedling$Height~Seedling$Block, ylab="Height (m)", xlab= "Block", main="Heights of Woody Stems")

ggplot(Seedling, aes(Treatment, Height, color=Herbivory))+
  geom_boxplot() 

with(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD",], table(Treatment, Height))
  
#collinearity x, samples uneven - can't use anova, unequal sample sizes? 507 vs 152
with(Seedling, table(Herbivory, Treatment))

# y vs x
ggplot(Seedling, aes(Treatment, Height..m., color=Herbivory, ylab="Heights (m)", main="Heights by Diversity and Herbivory Treatments")+
  geom_boxplot()

#are y's independent
#checks for any variance patterns over date - i don't think i see any? 
ggplot(Seedling, aes(Treatment, Height..m., color=Herbivory))+
  geom_boxplot()+
  facet_grid(.~Date)
with(Seedling, ftable(Herbivory, Treatment, Date))


#*****************************ANALYSIS**************************

#ANOVAS I don't think I can use - this is a linear model ie normal dis. which i don't have? 
webmod1<-lm(Height~Herbivory*Treatment, data=Seedling[Seedling$Treatment=="LD" | Seedling$Treatment =="HD",])
summary(webmod1)
anova(webmod1)

webmod2<-lm(Height~Herbivory+Treatment, data=Seedling[Seedling$Treatment=="LD" | Seedling$Treatment =="HD",])
summary(webmod2)
anova(webmod2)
confint(webmod2)

head(model.matrix(webmod1))


#linear mixed model
HeightMod <- lmer(Height..m. ~ Treatment * Herbivory + (1|Block), data=Seedling)
#do i need to run this again for date
summary(HeightMod)
coef(summary(HeightMod))
confint(HeightMod)


#some extra munge (move to munging code, not analysis)
SeedlingData_Tidy$Treatment[SeedlingData_Tidy$ID == "80"]<- "LD"
SeedlingData_Tidy$Date[SeedlingData_Tidy$ID == "673"] <- "10/26/2016"
  #chaning info for 673 but I'm not actually sure if it's right - have to check GPS

