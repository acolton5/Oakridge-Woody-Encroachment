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
#then, make a column for genus and one for spp, and fill in. 
adultgps$Genus<-"NA"
adultgps$Species<-"NA"

adultgps$Genus[adultgps$common.name== "black cherry"]<- "Prunus"
adultgps$Species[adultgps$common.name== "black cherry"]<- "serotina"

adultgps$Genus[adultgps$common.name== "black walnut"]<- "Juglans"
adultgps$Species[adultgps$common.name== "black walnut"]<- "nigra"

adultgps$Genus[adultgps$common.name== "boxelder"]<- "Acer"
adultgps$Species[adultgps$common.name== "boxelder"]<- "negundo"

adultgps$Genus[adultgps$common.name== "buckthorn"]<- "Rhamnus"
adultgps$Species[adultgps$common.name== "buckthorn"]<- "unknown"

adultgps$Genus[adultgps$common.name== "grape"]<- "Vitus"
adultgps$Species[adultgps$common.name== "grape"]<- "riparia"

adultgps$Genus[adultgps$common.name== "green ash"]<- "Fraxinus"
adultgps$Species[adultgps$common.name== "green ash"]<- "pennsylvanica"

adultgps$Genus[adultgps$common.name== "hackberry"]<- "Celtis"
adultgps$Species[adultgps$common.name== "hackberry"]<- "occidentalis"

adultgps$Genus[adultgps$common.name== "honeysuckle"]<- "Lonicera"
adultgps$Species[adultgps$common.name== "honeysuckle"]<- "unknown"

adultgps$Genus[adultgps$common.name== "mulberry"]<- "Morus"
adultgps$Species[adultgps$common.name== "mulberry"]<- "alba"

adultgps$Genus[adultgps$common.name== "oak"]<- "Quercus"
adultgps$Species[adultgps$common.name== "oak"]<- "unknown"

adultgps$Genus[adultgps$common.name== "other vine species"]<- "unknown"
adultgps$Species[adultgps$common.name== "other vine species"]<- "unknown"

adultgps$Genus[adultgps$common.name== "pear"]<- "Pyrus"
adultgps$Species[adultgps$common.name== "pear"]<- "calleryana"

adultgps$Genus[adultgps$common.name== "plum"]<- "Prunus"
adultgps$Species[adultgps$common.name== "plum"]<- "unknown"

adultgps$Genus[adultgps$common.name== "redbud"]<- "Cercis"
adultgps$Species[adultgps$common.name== "redbud"]<- "canadensis"

adultgps$Genus[adultgps$common.name== "serviceberry"]<- "Amelanchier"
adultgps$Species[adultgps$common.name== "serviceberry"]<- "unknown"

adultgps$Genus[adultgps$common.name== "si.elm"]<- "Ulmus"
adultgps$Species[adultgps$common.name== "si.elm"]<- "pumila"

adultgps$Genus[adultgps$common.name== "sil. maple"]<- "Acer"
adultgps$Species[adultgps$common.name== "sil. maple"]<- "saccharinum"

adultgps$Genus[adultgps$common.name== "sl.elm"]<- "Ulmus"
adultgps$Species[adultgps$common.name== "sl.elm"]<- "unknown"

adultgps$Genus[adultgps$common.name== "sw.oak"]<- "Quercus"
adultgps$Species[adultgps$common.name== "sw.oak"]<- "unknown"

adultgps$Genus[adultgps$common.name== "sycamore"]<- "Platanus"
adultgps$Species[adultgps$common.name== "sycamore"]<- "occidentalis"

adultgps$Genus[adultgps$common.name== "tree with light bark and prombud"]<- "unknown"
adultgps$Species[adultgps$common.name== "tree with light bark and prombud"]<- "unknown"

#then, make a column for genus + species.
adultgps$GenSp<-paste(adultgps$Genus,adultgps$Species)

write.csv(adultgps, "Data/Tidy/adultgps_tidy.csv")
