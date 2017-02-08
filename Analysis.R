#Woody Encroachment Analysis - Andrea 
#started on Jan. 19 2017 
#
library(plyr)
library(ggplot2)

seedling <- read.csv("SeedlingData_tidy.csv", na.string=c("", "na", "NA"))
str (seedling)

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

# Simple Bar Plot 
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears")
counts <- table($Treatment)
barplot(counts, main= "Distribution")

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(SeedlingData_Raw_Compiled$Species)
barplot(counts, main= "Species by Block", xlab = "Block", col=
          c("aquamarine","red","azure4","black","blue3","blueviolet","brown", 
           "burlywood1","cyan", "darkcyan", "darkgrey", "cornflowerblue"), 
        legend = Species(counts), beside=TRUE)
        