# Oakridge woody encroachment project
# Analysis of height by diversity trt and herbivore trt ##

#### General notes about next steps ####
#make decision about LD/edge, LD/HD

#in raw data

#how much area is in LD vs HD? (2x as much!)

#########################
#load libraries
library(lsmeans)
library(lme4)
library(ggplot2)
library(car)

#load data
Seedling <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.strings=c("", "NA", "na"))

#explore data
str(Seedling)
head(Seedling)
summary(Seedling$height)

###### Data Exploration ############

##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
dotchart(Seedling$height, xlab="Height (cm)", main = "Seedling Heights Again")
hist(Seedling$height, ylab = "Number of Individuals", xlab= "Height (cm)", main = "Seedling Heights")
#above result in boxplot of heights, no outliers, skewed left(?), most heights short? 

#separating by block - definite pattern of tallest individuals being in blocks 5-8. Lot more variance in exclosure than open. 
boxplot(Seedling$height~Seedling$Block, ylab="height (cm)", xlab= "Block", main="heights of Woody Stems")

ggplot(Seedling, aes(Herbivory, height, color=Treatment))+
  geom_boxplot() 
  
#Have both diversity trts represented for both herbivory categories
with(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD",], table(Treatment, Date))
with(Seedling, table(Herbivory, Treatment))

# y vs x - same as before
ggplot(Seedling, aes(Treatment, height, color=Herbivory, ylab="heights (m)", main="heights by Diversity and Herbivory Treatments"))+
  geom_boxplot()

#are y's independent
#checks for any variance patterns over date - no patterns, or really any reason to believe there would be
ggplot(Seedling, aes(Treatment, height, color=Herbivory))+
  geom_boxplot()+
  facet_grid(.~Date)
with(Seedling, ftable(Herbivory, Treatment, Date))


#*****************************ANALYSIS**************************

#ANOVAS I don't think I can use - this is a linear model ie normal dis. which i don't have? 
webmod1<-lm(height~Herbivory*Treatment, data=Seedling[Seedling$Treatment=="LD" | Seedling$Treatment =="HD",])
summary(webmod1)
anova(webmod1)

webmod2<-lm(height~Herbivory+Treatment, data=Seedling[Seedling$Treatment=="LD" | Seedling$Treatment =="HD",])
summary(webmod2)
anova(webmod2)
confint(webmod2)


#linear mixed model
heightMod <- lmer(height ~ Treatment * Herbivory + (1|Block), data=Seedling)
summary(heightMod)
coef(summary(heightMod))
confint(heightMod)


#some extra munge (move to munging code, not analysis)
SeedlingData_Tidy$Treatment[SeedlingData_Tidy$ID == "80"]<- "LD"
SeedlingData_Tidy$Date[SeedlingData_Tidy$ID == "673"] <- "10/26/2016"
  #changing info for 673 but I'm not actually sure if it's right - have to check GPS

