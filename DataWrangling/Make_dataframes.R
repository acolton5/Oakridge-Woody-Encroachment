### Make some new datasets #### 
library(plyr)

Seedling <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.strings=c("", "NA", ""))

#number of seedlings per spp per block
sppcomposition<-as.data.frame(with(Seedling, table(Block, GenSp)))
write.csv(sppcomposition, "Data/Tidy/sppcomposition.csv")

#need to figure out # of species in each block
spprich<-ddply(Seedling, .(Block), summarise, spprich=length(unique(GenSp)))
write.csv(spprich, "Data/Tidy/spprich.csv")

#guide to common name and scientific name
SppID_guide<-ddply(Seedling, .(Common.Name, GenSp), summarise, num=length(GenSp) )
