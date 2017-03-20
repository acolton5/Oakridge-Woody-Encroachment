#Munging Script for SeedlingData_Raw_Compiled 

SeedlingData_Raw_Compiled <- read.csv("Data/Raw/seedling_survey/SeedlingData_Raw_Compiled.csv", na.string=c("", "na", "NA"))

SeedlingData_Raw_Compiled$Common.Name<-trimws(SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name<-tolower(SeedlingData_Raw_Compiled$Common.Name)
levels(factor(SeedlingData_Raw_Compiled$Common.Name))

#clean up common names
#data$column <- gsub("thing to be replaced", "what you want to replace it with", data$column)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s.maple", "silver maple", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s. maple", "silver maple", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("cornus", "roughleaf dogwood", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("vitus", "grape vine", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("g.ash", "green ash", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("g. ash", "green ash", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("s.elm", "siberian elm", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("s. elm", "siberian elm", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("box elder", "boxelder", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("malus", "callery pear", SeedlingData_Raw_Compiled$Common.Name)
# have to individually fix "maple" in common name

SeedlingData_Raw_Compiled$Common.Name <- gsub("siberian elm?", "siberian elm", SeedlingData_Raw_Compiled$Common.Name)

# Mulberry
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "mulberry?"]<- "white mulberry"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "mul."]<- "white mulberry"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "mul?"]<- "white mulberry"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "red mulberry"]<- "white mulberry"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "red  mulberry"]<- "white mulberry"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "mulberry"]<- "white mulberry"

#pay attention to "black cherry family" change for unknown cherries
SeedlingData_Raw_Compiled$Common.Name <- gsub("b.cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("b. cherry", "black cherry", SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name <- gsub("black cherry family", "cherry family", SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Common.Name <- gsub("callery pear/pyrus", "callery pear", SeedlingData_Raw_Compiled$Common.Name)


#clean up genus and species names

#for filling in columns 
#must be as a character 
SeedlingData_Raw_Compiled$Genus<-as.character(SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Species<-as.character(SeedlingData_Raw_Compiled$Species)
SeedlingData_Raw_Compiled$Date<-as.character(SeedlingData_Raw_Compiled$Date)
SeedlingData_Raw_Compiled$Common.Name<-as.character(SeedlingData_Raw_Compiled$Common.Name)

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name=="silver maple"]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "silver maple"]<- "saccharinum"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Species== "s. maple"]<- "saccharinum"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Species== "s. maple"]<- "silver maple"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Genus== "s. maple"]<- "Acer"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "black cherry"]<- "Prunus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "black cherry"]<- "serotina"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Genus== "cherry?"]<- "Prunus"

SeedlingData_Raw_Compiled$Genus <- gsub("vitus", "Vitus", SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Genus <- gsub("Vitus? ", "Vitus", SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "grape vine"]<- "Vitus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "grape vine"]<- "riparia"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Genus== "Vitus?"]<- "Vitus"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "green ash"]<- "Fraxinus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "green ash"]<- "pennsylvanica"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "boxelder"]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "boxelder"]<- "negundo"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "red mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "white mulberry"]<- "Morus"

SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "m/p"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "mal/pr"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "mal/pru"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "mal/pru??"]<- "callery pear"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "malus/prunus"]<- "callery pear"

SeedlingData_Raw_Compiled$Genus <- gsub("cornus", "Cornus", SeedlingData_Raw_Compiled$Genus)
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "roughleaf dogwood"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "roughleaf dogwood"]<- "drummondii"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Cornus"]<- "roughleaf dogwood"

SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "49"]<- "red osier dogwood"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "670"]<- "red osier dogwood"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "red osier dogwood"]<- "stolonifera"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "red osier dogwood"]<- "Cornus"

SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "sycamore"]<- "occidentalis"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "sycamore"]<- "Platanus"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "callery pear"]<- "Pyrus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "callery pear"]<- "calleryana"

SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Genus== "Vitus"]<- "riparia"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Vitus"]<- "grape vine"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name=="siberian elm"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name=="siberian elm"]<- "pumila"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "redbud"]<- "Cercis"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "redbud"]<- "canadensis"

SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Prunus"]<- "black cherry"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Species== "b. cherry"]<- "black cherry"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Species== "b. cherry"]<- "serotina"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name=="american elm"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "american elm"]<- "americana"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Species== "s. elm"]<- "pumila"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "red mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "white mulberry"]<- "Morus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Genus== "Morus"]<- "alba"

SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "Crataegus spp.1 " | SeedlingData_Raw_Compiled$Common.Name=="crataegus sp. 1 "]<- "Crataegus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "Crataegus spp.1 "| SeedlingData_Raw_Compiled$Common.Name=="crataegus sp. 1 "]<- "unknown"


#######clean up treatment names#####
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="HD/LD"]<- "LD/HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="LD/H"]<- "LD/HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="HD?"]<- "HD" #for now, assume all HD? are in HD
SeedlingData_Raw_Compiled$Treatment[is.na(SeedlingData_Raw_Compiled$Treatment)]<- "LD/HD"
levels(factor(SeedlingData_Raw_Compiled$Treatment))
#still need to sort out where LD/HD and HD? plants actually are (HD or LD)

#####Make a herbivory category##########
SeedlingData_Raw_Compiled$Herbivory<-"NA"
SeedlingData_Raw_Compiled$Herbivory[SeedlingData_Raw_Compiled$Block==5|SeedlingData_Raw_Compiled$Block==6|SeedlingData_Raw_Compiled$Block==7|SeedlingData_Raw_Compiled$Block==8]<-"Exclosure"
SeedlingData_Raw_Compiled$Herbivory[SeedlingData_Raw_Compiled$Block==1|SeedlingData_Raw_Compiled$Block==2|SeedlingData_Raw_Compiled$Block==3|SeedlingData_Raw_Compiled$Block==4]<-"Open"
SeedlingData_Raw_Compiled$Herbivory<-as.factor(SeedlingData_Raw_Compiled$Herbivory)
levels(SeedlingData_Raw_Compiled$Herbivory)

####### fixing individual problems ######
#add date to the block 8 survey data
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "8"]<- "10/26/2016"
#change date for ID 99
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "99"]<- "10/13/2016"

#change common name for ID104 to boxelder
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "104"]<- "boxelder"

#Check all levels to make sure data wrangling worked
levels(factor(SeedlingData_Raw_Compiled$Common.Name))
levels(factor(SeedlingData_Raw_Compiled$Genus)) #need to determine cherry?, 
#Vitus? are 3 plants that are dead or "pretty dead
levels(factor(SeedlingData_Raw_Compiled$Species))
levels(factor(SeedlingData_Raw_Compiled$Treatment))
levels(factor(SeedlingData_Raw_Compiled$Herbivory))

missingdata<-SeedlingData_Raw_Compiled[is.na(SeedlingData_Raw_Compiled$Genus),]

#Create tidy csv

write.csv(SeedlingData_Raw_Compiled, "Data/Tidy/SeedlingData_Tidy.csv")


