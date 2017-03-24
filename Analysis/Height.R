# Oakridge woody encroachment project
# Analysis of height of woody encroachment by diversity trt and herbivore trt ##

#plan: 
#1) explore data, determine error structure, outliers, any missing trt combinations? 
#2) Run a model - response is height. could try logging it to reduce influence of the tail. predictors are diversity, herbivory and interaction. will need block as a random effect to account for any local areas that have really high growth for reasons not associated with diversity or herbivory. 
#3) check residuals to assess model fit. 
#4) graph model predictions. 

#### General next steps/questions ####
#make decision about LD/edge, LD/HD
#how much area is in LD vs HD? (2x as much!) Need to account for this. 

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

### fix up dataset ###
#remove those

###### Data Exploration ############

##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use dotplot or boxplot, identify outliers
dotchart(Seedling$height, xlab="Height (cm)", main = "Seedling Heights Again")
hist(Seedling$height, ylab = "Number of Individuals", xlab= "Height (cm)", main = "Seedling Heights")
#above result in boxplot of heights, no outliers, skewed left(?), most heights short? 

#Explore variance - are variances equal?
#separating by block - definite pattern of tallest individuals being in blocks 5-8. Lot more variance in exclosure than open. 
boxplot(Seedling$height~Seedling$Block, ylab="height (cm)", xlab= "Block", main="heights of Woody Stems")

ggplot(Seedling, aes(Herbivory, height, color=Treatment))+
  geom_boxplot() 
  
#.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
nrow(Seedling) #663 rows, three parameters, so we're good. 

#Check to make sure both diversity trts represented for both herbivory categories
with(Seedling, table(Herbivory, Treatment))

#check to see if a lot of NA's in height
length(Seedling[is.na(Seedling$height),]) # 11 rows where height is NA

# y vs x - same as before
ggplot(Seedling, aes(Treatment, height, color=Herbivory, ylab="heights (m)", main="heights by Diversity and Herbivory Treatments"))+
  geom_boxplot()

#are y's independent?
#checks for any variance patterns over date - no patterns, or really any reason to believe there would be
ggplot(Seedling, aes(Treatment, height, color=Herbivory))+
  geom_boxplot()+
  facet_grid(.~Date)

#Check to see what the pattern is by date in terms of when collected data
with(Seedling, ftable(Herbivory, Date)) #did most of hte open's first, then exclosures. But I don't think date should be important for woody stems. 

with(Seedling, ftable(Herbivory, Treatment, Date))


#*****************************ANALYSIS**************************

#linear model - height appears normally distributed
heightMod <- lmer(height ~ Treatment * Herbivory + (1|Block), data=Seedling)
summary(heightMod)
confint(heightMod)

#the interaction is not significant. Try without the interaction

heightMod2 <- lmer(log(height) ~ Treatment + Herbivory + (1|Block), data=Seedling)
summary(heightMod2)
confint(heightMod2) #shows that seedlings in herbivory open are shorter than those in exclosure

#Model validation
#A. Look at homogeneity: plot fitted values vs residuals
#method 1: plot(heightMod)

#method 2: 
#extract residuals
E1 <- resid(heightMod2, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(heightMod2, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

#B. Look at influential values: Cook

#C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense 
plot(x=Seedling$Herbivory, y=F1) #heterogeneity in residuals bt Herbivory trts
plot(x=Seedling$Diversity, y=F1) #heterogeneity in residuals wrt Diversity trts
plot(x=Seedling$Block, y=F1) #residual variance in random effects

#D. Look at normality of residuals: histogram
hist(E1) #look more normal when height is logged than on original scale

#graph it!



#some extra munge (move to munging code, not analysis)
SeedlingData_Tidy$Treatment[SeedlingData_Tidy$ID == "80"]<- "LD"
SeedlingData_Tidy$Date[SeedlingData_Tidy$ID == "673"] <- "10/26/2016"
  #changing info for 673 but I'm not actually sure if it's right - have to check GPS

