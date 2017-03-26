#munge gps dataframes #####

#read in GPS point csv's
adultgps<-read.csv("Data/Tidy/AdultsGPS.csv")

#deal with common names. Need this to match the seedling dataframe eventually. 
#first, make a new column for common name. 
adultgps$common.name<-adultgps$Comment
#then, get rid of numbers and trailing whitespace. 
adultgps$common.name<- gsub("\\d", "", adultgps$common.name)
adultgps$common.name<-trimws(adultgps$common.name)

levels(as.factor(adultgps$common.name))
#then, make a column for genus, and fill in. 
adultgps$Genus<-"NA"

adultgps$Genus[adultgps$Common.Name== "black cherry"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"
adultgps$Genus[adultgps$Common.Name== "black walnut"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"
adultgps$Genus[adultgps$Common.Name== "boxelder"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"
adultgps$Genus[adultgps$Common.Name== "buckthorn"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"
adultgps$Genus[adultgps$Common.Name== "buckthorn"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"
adultgps$Genus[adultgps$Common.Name== "buckthorn"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"
adultgps$Genus[adultgps$Common.Name== "buckthorn"]<- "Prunus"
adultgps$Species[adultgps$Common.Name== "black cherry"]<- "serotina"

#then, make a column for species, and fill in. 
#then, make a column for genus + species.