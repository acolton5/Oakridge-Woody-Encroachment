#Woody Encroachment Analysis - Andrea 
#started on Jan. 19 2017 

library(plyr)
library(ggplot2)

#read in data
seedling <- read.csv("SeedlingData_tidy.csv", na.string=c("", "na", "NA"))
str (seedling)

#explore data 
with(seedling[seedling$Treatment=="LD"|seedling$Treatment=="HD",], table(Treatment, Genus))

seedling$herbexcl<-"NA"
seedling$herbexcl[seedling$Block==5|seedling$Block==6|seedling$Block==7|seedling$Block==8]<-"excl"
seedling$herbexcl[seedling$Block==1|seedling$Block==2|seedling$Block==3|seedling$Block==4]<-"open"

with(seedling[seedling$Treatment=="LD"|seedling$Treatment=="HD",], ftable(herbexcl, Treatment, Genus))

sumseedling<-ddply(seedling[seedling$Treatment=="LD"|seedling$Treatment=="HD"& !is.na(seedling$herbexcl),], .(Block, Treatment, herbexcl, Genus), summarize, total=length(Genus))

ggplot(sumseedling[!is.na(sumseedling$Treatment) & !is.na(sumseedling$herbexcl),], aes(Treatment, total))+
  geom_boxplot()+
  facet_grid(herbexcl~Genus)+
  theme_bw()

ggplot(sumseedling[!is.na(sumseedling$Treatment) & !is.na(sumseedling$herbexcl),], aes(herbexcl, total))+
  geom_boxplot()+
  facet_grid(Treatment~Genus)+
  theme_bw()


#my go

Seedling <- read.csv("SeedlingData_Tidy.csv", na.string=c("", "na", "NA"))
str (seedling)

with(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD",], table(Treatment, Genus))

Seedling$herbexcl<-"NA"
Seedling$herbexcl[Seedling$Block==5|Seedling$Block==6|Seedling$Block==7|Seedling$Block==8]<-"excl"
Seedling$herbexcl[Seedling$Block==1|Seedling$Block==2|Seedling$Block==3|Seedling$Block==4]<-"open"

with(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD",], ftable(herbexcl, Treatment, Genus))

sumSeedling<-ddply(Seedling[Seedling$Treatment=="LD"|Seedling$Treatment=="HD"& !is.na(Seedling$herbexcl),], .(Block, Treatment, herbexcl, Genus), summarize, total=length(Genus))

ggplot(sumSeedling[!is.na(sumSeedling$Treatment) & !is.na(sumSeedling$herbexcl),], aes(Treatment, total))+
  geom_boxplot()+
  facet_grid(herbexcl~Genus)+
  theme_bw()

ggplot(sumSeedling[!is.na(sumSeedling$Treatment) & !is.na(sumSeedling$herbexcl),], aes(herbexcl, total))+
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

#Modeling
library(lme4)
library(ggplot2)
install.packages("car")
library(car)
install.packages("lsmeans")
library(lsmeans)

