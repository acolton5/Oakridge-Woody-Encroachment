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

sumseedling$trtarea<-"NA"
sumseedling$trtarea[sumseedling$Treatment=="HD"] <- 289.53
sumseedling$trtarea[sumseedling$Treatment=="LD"] <- 743.47
sumseedling$trtarea<-as.numeric(sumseedling$trtarea)
sumseedling$seedperm<-sumseedling$total/sumseedling$trtarea*10

ggplot(sumseedling, aes(Herbivory, seedperm, color=Treatment))+
  geom_boxplot()

sumseedling_gen<-ddply(Seedling, .(Block, Treatment, Herbivory, Genus), summarize, total=length(Herbivory))

ggplot(sumseedling_gen, aes(Treatment, total, color=Herbivory))+
  geom_boxplot()+
  facet_grid(.~Genus)

#figure out area of each treatment
#HD treatment is 289.53m2, 
# LD is 662.47m2
# the strips are 72m2 
#sum: HD = 289.53, LD= 743.47

hist(sumseedling$seedperm)
dotchart(sumseedling$seedperm)

###### Analysis ##########
#using mixed linear model
#need to adjust for abundance of low vs high diversity area still. 
AbunMod <- lm(seedperm ~ Treatment * Herbivory,  data=sumseedling)
summary(AbunMod)
confint (AbunMod) #interaction is not significant

AbunMod2 <- lm(seedperm ~ Treatment + Herbivory,  data=sumseedling)
summary(AbunMod2)
confint (AbunMod2) #only sig diff is in herbivory, not diversity. 

#Model validation
#A. Look at homogeneity: plot fitted values vs residuals
#method 1: 
plot(AbunMod2)

#method 2: 
#extract residuals
E1 <- resid(AbunMod2, type = "pearson")

#extract fitted values
F1 <- fitted(AbunMod2, type = "response")

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
hist(E1) #

#use lsmeans to test which combinations are significant. 

##########################
#######graph it! ##############
sumseedling <- ddply(sumseedling, c("Herbivory", "Treatment"), summarise,
               N    = length(seedperm),
               mean = mean(seedperm),
               sd   = sd(seedperm),
               se   = sd / sqrt(N))
 
ISUcolors <- c("#de2d26", "#feb24c")
 
ggplot(sumseedling, aes(Herbivory, mean, color=Treatment))+
  geom_point(stat="identity", position=position_dodge(width=0.4), size=3)+  
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.2, position=position_dodge(width=0.4), lwd=1)+
 scale_color_manual(name="Prairie\nDiversity\nTreatment", labels=c("High Diversity", "Low Diversity"), values=ISUcolors)+
  ylab("Number of Seedlings per 10m^2")+
  annotate("text", x=1.5, y=2, label="***", size=10)+
 theme_classic()+
  theme(axis.text = element_text(size=12, face="bold"), 
    line=element_line(size=3), 
    axis.title=element_text(size=14, face="bold"), 
    legend.title = element_text(size=14, face="bold"), 
    legend.text = element_text(size=12, face="bold"))

 
ggsave("Graphics/NumberOfSeedlings_figure.png", width=6, height=5, units="in")
