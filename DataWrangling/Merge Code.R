### Merge Seedling data with Seedling GPS data ###
## Point ID is the column of interest to merge. 

SeedlingData_Tidy <- read.csv("SeedlingData_Tidy.csv", na.string=c("", "na", "NA"))
seedgps<-read.csv("Data/Tidy/SeedlingGPS.csv")

merged.data <- merge(SeedlingData_Tidy, SeedlingGPS, by="ID")
#can't get it to recognize SeedlingGPS without importing to environment 

write.csv(merged.data, "seedlinggps_tidy.csv")
