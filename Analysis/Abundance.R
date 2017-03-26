#*****************ANALYSIS FOR ABUNDANCE********************
#load libraries
library(lme4)
library(ggplot2)
library(lsmeans)
library(car)
library(plyr)

#load data
Seedling <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.strings=c("", "NA", ""))

with(Seedling, table(Block))
barplot(table(Seedling$Block))

sumseedling<-ddply(Seedling, .(Block, Treatment, Herbivory), summarize, total=length(Herbivory))

ggplot(sumseedling, aes(Treatment, total, color=Herbivory))+
  geom_boxplot()

sumseedling_gen<-ddply(Seedling, .(Block, Treatment, Herbivory, Genus), summarize, total=length(Herbivory))

ggplot(sumseedling_gen, aes(Treatment, total, color=Herbivory))+
  geom_boxplot()+
  facet_grid(.~Genus)

###### Analysis ##########
#using mixed linear model
#need to adjust for abundance of low vs high diversity area still. 
AbunMod <- glm(total ~ Treatment * Herbivory, family=poisson, data=sumseedling)
summary(AbunMod)
confint (AbunMod)

#Model validation
#A. Look at homogeneity: plot fitted values vs residuals
#method 1: 
plot(AbunMod)

#method 2: 
#extract residuals
E1 <- resid(AbunMod, type = "pearson")

#extract fitted values
F1 <- fitted(AbunMod, type = "response")

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
plot(x=sumseedling$Herbivory, y=E1) #heterogeneity in residuals bt Herbivory trts 
plot(x=sumseedling$Treatment, y=E1) #heterogeneity in residuals wrt Diversity trts
plot(x=sumseedling$Block, y=E1) #residual variance in random effects

#D. Look at normality of residuals: histogram
hist(E1) #look more normal when height is logged than on original scale

##########################
#######graph it! ##############
sumseedling <- ddply(sumseedling, c("Herbivory", "Treatment"), summarise,
               N    = length(total),
               mean = mean(total),
               sd   = sd(total),
               se   = sd / sqrt(N))
 
group.colors <- c("HD"="#E69F00", "LD"="#D55E00FF", "LD/HD"="#F0E442") #need to change these
 
ggplot(sumseedling, aes(Herbivory, mean, color=Treatment))+
  geom_point(stat="identity", position=position_dodge(width=0.4))+  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(width=0.4))+
 scale_color_manual(name="Diversity Treatment", labels=c("High Diversity", "Low Diversity", "High/Low"), values=group.colors)+
  ylab("Number of Seedlings")+
 theme_classic()

 
ggsave("Graphics/NumberOfSeedlings_figure.png")