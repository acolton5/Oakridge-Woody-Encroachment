#Woody Encroachment Analysis - Andrea 
#started on Jan. 19 2017 

library(plyr)
library(ggplot2)

#read in data
seedling <- read.csv("Data/tidy/SeedlingData_tidy.csv", na.string=c("", "na", "NA"))
str (seedling)

#explore data 
with(seedling[seedling$Treatment=="LD"|seedling$Treatment=="HD",], table(Treatment, Genus))

with(seedling[seedling$Treatment=="LD"|seedling$Treatment=="HD",], ftable(Herbivory, Treatment, Genus))

sumseedling<-ddply(seedling[seedling$Treatment=="LD"|seedling$Treatment=="HD"& !is.na(seedling$Herbivory),], .(Block, Treatment, Herbivory, Genus), summarize, total=length(Genus))

ggplot(sumseedling[!is.na(sumseedling$Treatment) & !is.na(sumseedling$Herbivory),], aes(Treatment, total, color=Herbivory))+
  geom_boxplot()

ggplot(sumseedling[!is.na(sumseedling$Treatment) & !is.na(sumseedling$Herbivory),], aes(Treatment, total))+
  geom_boxplot()+
  facet_grid(Herbivory~.)+
  theme_bw()

ggplot(sumseedling[!is.na(sumseedling$Treatment) & !is.na(sumseedling$Herbivory),], aes(Genus, total, color=Herbivory))+
  geom_boxplot()
  facet_grid(Treatment~Genus)+
  theme_bw()


#my go

Seedling <- read.csv("SeedlingData_Tidy.csv", na.string=c("", "na", "NA"))
str (seedling)

with(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD",], table(Treatment, Genus))

with(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD",], ftable(Herbivory, Treatment, Genus))

sumSeedling<-ddply(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD"& !is.na(Seedling$Herbivory),], .(Block, Treatment, Herbivory, Genus), summarize, total=length(Genus))

ggplot(sumSeedling[!is.na(sumSeedling$Treatment) & !is.na(sumSeedling$Herbivory),], aes(Treatment, total))+
  geom_boxplot()+
  facet_grid(Herbivory~Genus)+
  theme_bw()

ggplot(sumSeedling[!is.na(sumSeedling$Treatment) & !is.na(sumSeedling$Herbivory),], aes(Herbivory, total))+
  geom_boxplot()+
  facet_grid(Treatment~Genus)+
  theme_bw()



#barplots (using updated )
head(Seedling)
table(Seedling$Genus)
barplot(table(Seedling$Genus)) #basic barplot
barplot(table(Seedling$Genus), ylab= 'Counts', main='Number of Each Species') 
#grouped bar chart
Table1 <- table(Seedling$Block, Seedling$Genus)
barplot(Table1, beside = T, legend.text = T)
barplot(Table1, beside = T, main = "Number of Species per Block", xlab= "Species", ylab= "Count", 
        col=c(1,2,3,4,5,6,7,8))

