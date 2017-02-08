#Munging Script for SeedlingData_Raw_Compiled 

#for replacing words
#data$column <- gsub("thing to be replaced", "what you want to replace it with", data$column)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s.maple", "silver maple", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s. maple", "silver maple", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("S.maple", "silver maple", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("S. maple", "silver maple", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("dogwood", "roughleaf dogwood", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("Dogwood", "roughleaf dogwood", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("cornus", "roughleaf dogwood", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("Cornus", "roughleaf dogwood", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("b.cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("b. cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("B. cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("B.cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("vitus", "grape vine", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("Vitus", "grape vine", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("g.ash", "green ash", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("g. ash", "green ash", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s.elm", "siberian elm", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s. elm", "siberian elm", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
       #pay attention to "black cherry family" change for unknown cherries
SeedlingData_Raw_Compiled$Common.Name <- gsub("box elder", "boxelder", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("dog", "roughleaf dogwood", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("malus", "callery pear", SeedlingData_Raw_Compiled$Common.Name)
       # have to individually fix "maple" in common name
SeedlingData_Raw_Compiled$Common.Name <- gsub("mulberry?", "mulberry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("mulberry ", "mulberry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("mul", "mulberry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("mul.", "mulberry", SeedlingData_Raw_Compiled$Common.Name)
       # leaving what i designated white/red - can change and add species name
SeedlingData_Raw_Compiled$Genus <- gsub("cornus", "Cornus", SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Genus <- gsub("vitus", "Vitus", SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Genus <- gsub("Vitus? ", "Vitus", SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Common.Name <- gsub("siberian elm?", "siberian elm", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("mulberryerryberry?", "mulberry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Genus <- gsub("Vitus?", "Vitus", SeedlingData_Raw_Compiled$Genus)


#for filling in columns 
#must be as a character 
SeedlingData_Raw_Compiled$Genus<-as.character(SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Species<-as.character(SeedlingData_Raw_Compiled$Species)
SeedlingData_Raw_Compiled$Date<-as.character(SeedlingData_Raw_Compiled$Date)


SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name=="silver maple"]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "silver maple"]<- "saccharinum"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "roughleaf dogwood"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "roughleaf dogwood"]<- "drummondii"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "black cherry"]<- "Prunus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "black cherry"]<- "serotina"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "grape vine"]<- "Vitus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "grape vine"]<- "riparia"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "green ash"]<- "Fraxinus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "green ash"]<- "pennsylvanica"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "boxelder "]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "boxelder "]<- "negundo"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "red mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "white mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "m/p"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "mal/pr"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "mal/pru"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "malus/prunus"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Cornus"]<- "roughleaf dogwood"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "49"]<- "red osier dogwood"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "670"]<- "red osier dogwood"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "red osier dogwood"]<- "stolonifera"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "red osier dogwood"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "sycamore"]<- "occidentalis"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "sycamore"]<- "Platanus"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "104"]<- "boxelder"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "callery pear"]<- "Pyrus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "callery pear"]<- "calleryana"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Genus== "Vitus"]<- "riparia"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Vitus"]<- "grape vine"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name=="siberian elm"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name=="siberian elm"]<- "pumila"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "redbud"]<- "Cercis"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "redbud"]<- "canadensis"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Species== "s. maple"]<- "silver maple"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Prunus"]<- "black cherry"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name=="american elm"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "american elm"]<- "americana"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "red mulberry"]<- "Morus"


#fixing individual problems
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "493"]<- "callery pear"



#to view levels - have to switch back to factor 
SeedlingData_Raw_Compiled$Common.Name <- as.factor(SeedlingData_Raw_Compiled$Common.Name)
levels(SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Genus <- as.factor(SeedlingData_Raw_Compiled$Genus)
levels(SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Species <- as.factor(SeedlingData_Raw_Compiled$Species)
levels(SeedlingData_Raw_Compiled$Species)

SeedlingData_Raw_Compiled$Treatment <- as.factor(SeedlingData_Raw_Compiled$Treatment)
levels(SeedlingData_Raw_Compiled$Treatment)
hist(SeedlingData_Raw_Compiled$Treatment)

SeedlingData_Raw_Compiled$Treatment <- as.numeric(SeedlingData_Raw_Compiled$Treatment)
#hist 




