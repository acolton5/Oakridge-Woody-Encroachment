### Merge Seedling data with Seedling GPS data ###
## Point ID is the column of interest to merge. 

SeedlingData_Tidy <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.string=c("", "na", "NA"))
SeedlingData_Tidy$ID<-as.numeric(SeedlingData_Tidy$ID)

seedgps<-read.csv("Data/Tidy/SeedlingGPS.csv")

seedling_gps <- left_join(SeedlingData_Tidy, seedgps, by=c("ID"= "Point_ID"))

write.csv(seedling_gps, "Data/Tidy/seedlinggps_tidy.csv")
