#Munging Script for SeedlingData_Raw_Compiled 
library(dplyr)
library(lubridate)

SeedlingData_Raw_Compiled <- read.csv("Data/Raw/seedling_survey/SeedlingData_Raw_Compiled.csv", na.string=c("", "na", "NA"))

SeedlingData_Raw_Compiled$Common.Name<-trimws(SeedlingData_Raw_Compiled$Common.Name)
SeedlingData_Raw_Compiled$Common.Name<-tolower(SeedlingData_Raw_Compiled$Common.Name)
levels(factor(SeedlingData_Raw_Compiled$Common.Name))

names(SeedlingData_Raw_Compiled)[names(SeedlingData_Raw_Compiled)=="Height..m."] <- "height"
SeedlingData_Raw_Compiled$height<-as.numeric(as.character(SeedlingData_Raw_Compiled$height))

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


SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "cherry"]<- "black cherry"
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
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Genus== "Cornus"]<- "drummondii"


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


#######clean up treatment names#####
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="HD/LD"]<- "LD/HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="LD/H"]<- "LD/HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="HD?"]<- "HD" #for now, assume all HD? are in HD
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment=="LD/edg"]<- "LD" #for now, assume all LD/edg are LD
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

#change common name for ID104 to boxelder
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "104"]<- "boxelder"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "boxelder"]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "boxelder"]<- "negundo"

#fill in data for unkown crateagus 
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "9"]<- "hawthorn"
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "31"]<- "hawthorn"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "hawthorn"]<- "Crataegus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "hawthorn"]<- "unknown"

#fill in ID42 as honeysuckle genus
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "42"]<- "honeysuckle"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "42"]<- "Lonicera"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "42"]<- "unknown"

#fill in ID44 for unknown rubus
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "44"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "44"]<- "Rubus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "44"]<- "unknown"

#fill in unkowns for ID50
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "57"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "57"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "57"]<- "unknown"

#fill in genus and species for ID57 boxelder
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "50"]<- "boxelder"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "50"]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "50"]<- "negundo"

#filling in unknowns for ID 71 and 73
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "71"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "71"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "71"]<- "unknown"

SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "73"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "73"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "73"]<- "unknown"

#filling in unknowns for ID106
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "106"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "106"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "106"]<- "unknown"

#fill in genus/species for ID110
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "110"]<- "siberian elm"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "110"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "110"]<- "pumila"

#fill in genus/species for ID 125
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "125"]<- "raspberry"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "125"]<- "Rubus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "125"]<- "unknown"

#ID 127 as Pyrus calleryana
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "127"]<- "callery pear"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "127"]<- "Pyrus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "127"]<- "calleryana"

#fill in genus/species for ID132
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "132"]<- "roughleaf dogwood"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "132"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "132"]<- "drummondii"

#fill in info for ID 135
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "135"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "135"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "135"]<- "unknown"

#fixing common name dogwood
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "dogwood"]<- "roughleaf dogwood"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "roughlea f dogwood"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "roughleaf dogwood"]<- "drummondii"

#fill in genus/species for ID145
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "145"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "145"]<- "unknown"

#155 is not high enough 
SeedlingData_Raw_Compiled$Notes[SeedlingData_Raw_Compiled$ID== "155"]<- "not high enough"

#fill in genus/common name for species saccharinum 
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Species== "saccharinum"]<- "silver maple"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Species== "saccharinum"]<- "Acer"

#fillin unknowns for ID158
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "158"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "158"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "158"]<- "unknown"

#fill in genus/species for ID171
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "171"]<- "silver maple"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "171"]<- "Platanus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "171"]<- "occidentalis"

#fill in genus/species for ID175
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "175"]<- "siberian elm"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "175"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "175"]<- "pumila"

#fill in unknowns for ID178
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "178"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "178"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "178"]<- "unknown"

#designate ID179 as black cherry
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "179"]<- "black cherry"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "179"]<- "Prunus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "179"]<- "serotina"

#fill in species/common name for ID183
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Genus== "Acer"]<- "silver maple"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Genus== "Acer"]<- "saccharinum"

#fill in unknowns for ID235
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "235"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "235"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "235"]<- "unknown"

#fill in genus/species for ID267
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "267"]<- "roughleaf dogwood"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "267"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "267"]<- "drummondii"

#fill in unknowns for ID269
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "269"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "269"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "269"]<- "unknown"

#fill in genus/species for ID273
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$Common.Name== "maple"]<- "silver maple"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Common.Name== "silver maple"]<- "Acer"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$Common.Name== "silver maple"]<- "saccharinum"

#fill in genus/species for ID297
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "297"]<- "siberian elm"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "297"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "297"]<- "pumila"

#fill in genus/species for ID333
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "333"]<- "siberian elm"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "333"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "333"]<- "pumila"

#fill in unknowns for ID346
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "346"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "346"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "346"]<- "unknown"

#fill in unknowns for ID402
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "402"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "402"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "402"]<- "unknown"

#fill in unknowns for ID435
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "435"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "435"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "435"]<- "unknown"

#fill in unknowns for ID441
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "441"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "441"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "441"]<- "unknown"

#fill in unknowns for ID443
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "443"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "443"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "443"]<- "unknown"

#fill in genus/species for ID453b
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "453b"]<- "roughleaf dogwood"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "453b"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "453b"]<- "drummondii"

#fill in genus/species for ID456
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "456"]<- "siberian elm"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "456"]<- "Ulmus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "456"]<- "pumila"

#fill in genus/species for ID503
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "503"]<- "raspberry"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "503"]<- "Rubus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "503"]<- "unknown"

#fill in ID520 buckthorn
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "520"]<- "buckthorn"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "520"]<- "Rhamnus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "520"]<- "unknown"

#fill in genus/species for ID 522
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "522"]<- "roughleaf dogwood"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "522"]<- "Cornus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "522"]<- "drummondii"

#fill in unknowns for ID597
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "597"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "597"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "597"]<- "unknown"

#fill in unknowns for ID606
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "606"]<- "unknown"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "606"]<- "unknown"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "606"]<- "unknown"

#fill in genus/species for ID607
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "607"]<- "raspberry"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "607"]<- "Rubus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "607"]<- "unknown"

#fill in genus/species for ID651
SeedlingData_Raw_Compiled$Common.Name[SeedlingData_Raw_Compiled$ID== "651"]<- "raspberry"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$ID== "651"]<- "Rubus"
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID== "651"]<- "unknown"

#fill in date and block for ID673
SeedlingData_Raw_Compiled$Block[SeedlingData_Raw_Compiled$ID== "673"]<- "8"

#add species to 351
SeedlingData_Raw_Compiled$Species[SeedlingData_Raw_Compiled$ID=="351"] <- "serotina"

#filling in missing dates
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "2"]<- "10/12/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "93"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "94"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "95"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "96"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "97"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "98"]<- "10/13/2016"

SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "1"]<- "10/16/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "99"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "100"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "101"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "102"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "103"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "104"]<- "10/13/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$ID== "105"]<- "10/13/2016"

SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "3"]<- "10/23/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "5"]<- "10/16/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "6"]<- "10/19/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "7"]<- "10/23/2016"
SeedlingData_Raw_Compiled$Date[SeedlingData_Raw_Compiled$Block== "8"]<- "10/26/2016"
SeedlingData_Raw_Compiled$Date<-mdy(SeedlingData_Raw_Compiled$Date)

#fixing heights
SeedlingData_Raw_Compiled$height[SeedlingData_Raw_Compiled$ID== "158"]<- 0.69
SeedlingData_Raw_Compiled$height[SeedlingData_Raw_Compiled$ID== "222"]<- 1.81
SeedlingData_Raw_Compiled$height[SeedlingData_Raw_Compiled$ID== "51"]<- ""

#filling in correct diversity treatments for LD/HD
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "111"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "64"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "80"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "559"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "34"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "42"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "153"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "154"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "157"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "158"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "476"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "477"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "483"]<- "LD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "610"]<- "LD"

SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$Treatment== "LD/HD"]<- "HD"

SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "110"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "115"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "116"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "81"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "82"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "83"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "88"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "89"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "23"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "161"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "162"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "184"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "185"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "190"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "191"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "192"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "194"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "200"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "206"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "207"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "208"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "209"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "210"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "211"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "212"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "214"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "215"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "217"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "229"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "230"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "231"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "232"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "233"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "234"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "235"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "236"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "237"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "238"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "239"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "240"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "241"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "275"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "276"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "278"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "311"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "313"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "323"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "333"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "363"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "365"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "373"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "374"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "375"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "379"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "380"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "383"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "388"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "389"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "390"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "393"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "394"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "395"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "396"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "397"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "399"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "432"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "433"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "442"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "493"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "494"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "495"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "496"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "497"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "508"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "525"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "600"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "601"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "602"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "605"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "607"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "608"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "618"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "641"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "642"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "643"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "644"]<- "HD"
SeedlingData_Raw_Compiled$Treatment[SeedlingData_Raw_Compiled$ID== "646"]<- "HD"

#these two edits won't work unless they come at the end of everything else 
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Species== "drummondii"]<- "Cornus"
SeedlingData_Raw_Compiled$Genus[SeedlingData_Raw_Compiled$Species =="serotina"] <- "Prunus"

#Check all levels to make sure data wrangling worked
levels(factor(SeedlingData_Raw_Compiled$Common.Name))
levels(factor(SeedlingData_Raw_Compiled$Genus)) 
levels(factor(SeedlingData_Raw_Compiled$Species))
levels(factor(SeedlingData_Raw_Compiled$Treatment))
levels(factor(SeedlingData_Raw_Compiled$Herbivory))

missingdata<-SeedlingData_Raw_Compiled[is.na(SeedlingData_Raw_Compiled$Genus),]
missingdata<-SeedlingData_Raw_Compiled[is.na(SeedlingData_Raw_Compiled$height),]
missingdata<-SeedlingData_Raw_Compiled[is.na(SeedlingData_Raw_Compiled$Species),]
missingdata<-SeedlingData_Raw_Compiled[is.na(SeedlingData_Raw_Compiled$Date),]
missingdata<-SeedlingData_Raw_Compiled[is.na(SeedlingData_Raw_Compiled$Treatment),]


#Create tidy csv
write.csv(SeedlingData_Raw_Compiled, "Data/Tidy/SeedlingData_Tidy.csv")


