#*****************ANALYSIS FOR ABUNDANCE********************

boxplot(Seedling$ID, ylab = "Heights")
Seedling$ID <- as.integer(Seedling$ID)
boxplot(Seedling$ID~Seedling$Block)

with(Seedling, table(Block))
barplot(table(Seedling$Block))

library(lsmeans)
library(lme4)
library(ggplot2)
library(car)
AbunMod <- lmer(Block ~ Treatment * Herbivory + (1|Date), data=Seedling)
summary(AbunMod)

#Using simple anova from bats
t.test(Individuals~Herbivory, data=Abundance)
aov(Individuals~Herbivory, data=Abundance)

#Splitting Abundance by Herbivory into LD/HD
with(Seedling, table(Block, Treatment))

#think i did abundance per herb treatment correctly
#not sure how to find t test for differences in low and high diversity between 
#herb exclosure and open but think i need 4 t.test/anovas? 

#Herbivory vs LD use TreatmentLD
t.test(Individuals~Herbivory, data=Treatment2)
aov(Individuals~Herbivory, data=TreatmentLD)


#Herbivory vs use TreatmentHD
t.test(Individuals~Herbivory, data=Treatment2)
aov(Individuals~Herbivory, data=TreatmentHD)

#LD vs HD use Treatment
t.test(Individuals~Herbivory, data=Treatment2)
aov(Individuals~Treatment, data=Treatment)
 

#Trying it all with a linear mixed model
AbundModLMM <- lmer(Individuals ~ Treatment * Herbivory + (1|Block), data=Treatment2)
summary(AbundModLMM)
confint(AbundModLMM)



levels(Seedling$Genus)
