### Make some new datasets #### 

#number of seedlings per spp per block
sppcomposition<-as.data.frame(with(Seedling, table(Block, GenSp)))
write.csv(sppcomposition, "Data/Tidy/sppcomposition.csv")

#need to figure out # of species in each block
spprich<-ddply(Seedling, .(Block), summarise, spprich=length(unique(GenSp)))
write.csv(spprich, "Data/Tidy/spprich.csv")

#guide to common name and scientific name
Seedling<-read.csv("Data/Tidy/SeedlingData_Tidy.csv")
library(plyr)
SppID_guide<-ddply(Seedling, .(Common.Name, GenSp), summarise, num=length(GenSp) )
