#*******************DENSITY**********************

with(Seedling, table(Block, Treatment))

#using density?
Density <- lmer(Density~Treatment*Herbivory + (1|Block), data=Density)
summary(Density)
coef(summary(Density))
confint(Density)

t.test(Density~Treatment, data=Density)
aov(Density~Treatment, data=Density)

t.test(Density~Herbivory, data=Density)
aov(Density~Herbivory, data=Density)

#*****************DIVERSITY***************************

#Low Diversity
block1 <-c(10,0,0,0,0,0,0,0,0,2)
block2 <-c(12,0,0,0,0,0,7,0,0,3)
block3 <-c(3,0,4,0,0,3,0,3,0,2)
block4 <-c(18,1,4,0,1,0,8,0,0,2)
block5 <-c(71,0,19,2,8,6,3,8,6,5)
block6 <-c(66,1,23,0,0,2,0,15,1,1)
block7 <-c(7,0,1,0,4,0,0,6,17,30)
block8 <-c(19,0,1,0,10,3,0,7,8,23)

library(vegan)
diversity(SeedlingData_Tidy, index = "shannon", MARGIN = 1, base = exp(1))











