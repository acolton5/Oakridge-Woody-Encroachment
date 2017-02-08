#Woody Encroachment Analysis - Andrea 
#started on Jan. 19 2017 
Andrea_test <- read.csv("~/Haldre/Woody Encroachment/Oakridge-Woody-Encroachment/Andrea_test.csv")
str (Andrea_test)
str(SeedlingData_Raw_Compiled)


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
        