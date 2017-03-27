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
library(lme4)
library(ggplot2)

#load data
Seedling <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.strings=c("", "NA", ""))

#explore data
str(Seedling)
head(Seedling)

### fix up dataset ###
#remove the grapevines and the other few that are missing a height
Seedling_ht<-Seedling[!is.na(Seedling$height),] #removes 101 observations

###### Data Exploration ############
 
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use dotplot or boxplot, identify outliers
dotchart(Seedling_ht$height, xlab="Height (cm)", main = "Seedling Heights Again") #no obvious outliers
 hist(Seedling_ht$height, ylab = "Number of Individuals", xlab= "Height (cm)", main = "Seedling Heights")
 #above result in boxplot of heights, no outliers, skewed left(?), most heights short? 
 
#Explore variance - are variances equal?
 #separating by block - definite pattern of tallest individuals being in blocks 5-8. Lot more variance in exclosure than open. 
boxplot(Seedling_ht$height~Seedling_ht$Block, ylab="height (cm)", xlab= "Block", main="heights of Woody Stems")
 
 ggplot(Seedling_ht, aes(Herbivory, height, color=Treatment))+
   geom_boxplot() 
   
#.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
nrow(Seedling_ht) #556 rows, three parameters, so we're good. 

#Check to make sure both diversity trts represented for both herbivory categories
with(Seedling_ht, table(Herbivory, Treatment))
 
#check to see if a lot of NA's in height
nrow(Seedling_ht[is.na(Seedling_ht$height),]) # 0 rows where height is NA

 # y vs x - same as before
 ggplot(Seedling_ht, aes(Treatment, height, color=Herbivory, ylab="heights (m)", main="heights by Diversity and Herbivory Treatments"))+
   geom_boxplot()
 
#are y's independent?
#checks for any variance patterns over date - no patterns, or really any reason to believe there would be
 ggplot(Seedling_ht, aes(Treatment, height, color=Herbivory))+
   geom_boxplot()+
   facet_grid(.~Date)

#Check to see what the pattern is by date in terms of when collected data
with(Seedling_ht, ftable(Herbivory, Date)) #did most of hte open's first, then exclosures. But I don't think date should be important for woody stems. 

 with(Seedling_ht, ftable(Herbivory, Treatment, Date))
 
 
#*****************************ANALYSIS*************************
 
#linear model - height appears normally distributed, but residuals look better when height is logged
heightMod <- lmer(log(height) ~ Treatment * Herbivory + (1|Block), data=Seedling)
summary(heightMod)
confint(heightMod)

#the interaction is not significant. Try without the interaction
heightMod2 <- lmer(log(height) ~ Treatment + Herbivory + (1|Block), data=Seedling_ht)
summary(heightMod2)
confint(heightMod2) #shows that seedlings in herbivory open are shorter than those in exclosure

#Model validation
#A. Look at homogeneity: plot fitted values vs residuals
#method 1: 
plot(heightMod2)

#method 2: 
#extract residuals
E1 <- resid(heightMod2, type = "pearson")

#extract fitted values
F1 <- fitted(heightMod2, type = "response")

#plot fitted vs residuals
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
plot(x=Seedling_ht$Herbivory, y=E1) #heterogeneity in residuals bt Herbivory trts 
plot(x=Seedling_ht$Treatment, y=E1) #heterogeneity in residuals wrt Diversity trts
plot(x=Seedling_ht$Block, y=E1) #residual variance in random effects

#D. Look at normality of residuals: histogram
hist(E1) #look more normal when height is logged than on original scale

#graph it!

sumseedling <- ddply(Seedling_ht, c("Herbivory", "Treatment"), summarise,
               N    = length(height),
               mean = mean(height),
               sd   = sd(height),
               se   = sd / sqrt(N))

group.colors <- c("HD"="#E69F00", "LD"="#D55E00FF", "LD/HD"="#F0E442") #need to change these

ggplot(sumseedling, aes(Herbivory, mean, color=Treatment))+
  geom_point(stat="identity", position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(width=0.4))+
  scale_color_manual(name="Diversity Treatment", labels=c("High Diversity", "Low Diversity", "High/Low"), values=group.colors)+
  ylab("Height (m)")+
  theme_classic()

ggsave("Graphics/height_figure.png")
