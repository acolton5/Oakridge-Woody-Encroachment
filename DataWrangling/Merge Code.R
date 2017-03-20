#haven't done 

write.csv(SeedlingData_Tidy, "SeedlingData_Tidy.csv")
SeedlingData_Tidy <- read.csv("SeedlingData_Tidy.csv", na.string=c("", "na", "NA"))

merged.data <- merge(SeedlingData_Tidy, SeedlingGPS, by="ID")
#can't get it to recognize SeedlingGPS without importing to environment 
write.csv(merged.data, "Merge.csv")
