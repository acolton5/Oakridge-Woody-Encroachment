###### Analysis of diversity by herbivory and diversity trt #######

#########################
#load libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(plyr)

#load data
Seedling <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.strings=c("", "NA", ""))
spprich<-read.csv("Data/Tidy/spprich.csv")
sppcomp<-read.csv("Data/Tidy/sppcomposition.csv")

#need to get a community matrix for each plot (spp in col, block in rows)