#calculate mindist
#1) loop over each row of seedling_gps [i]
#2) for every row, calculate distance to each row/tree in adultgps where seedling_gps$GenSp == adultgps$GenSp, and print in a temporary vector. This will be the same length as the number of adult trees of that spp. 
#3) pull out the minimum distance of all distances to adults. Print in a single column in seedling_gps. 

seedling_gps<-read.csv("Data/Tidy/seedlinggps_tidy.csv")
adultgps<-read.csv("Data/Tidy/adultgps_tidy.csv")

#just pull out seedlings that have adults in adult gps file
adulttrees<-levels(adultgps$GenSp)
seedling_gps2<-seedling_gps[seedling_gps$GenSp %in% adulttrees & seedling_gps$Species!="unknown",]  #

#Make empty matrix and then calculate distance between each tree-seedling combo for species i
    distances.i<-matrix(NA,nrow(seedling_gps2),20) #make empty matrix to populate during for loop; use 20 columns, becase all spp have less than 20 adult trees. 

#rows are adult tree individuals of spp i, columns are seedlings
		for (j in 1:nrow(seedling_gps2)){
		      spptrees=subset(adultgps, as.character(adultgps$GenSp)==as.character(seedling_gps2$GenSp[j]))
		      for (k in 1:nrow(spptrees)){
        	distances.i[j,k]=sqrt((seedling_gps2$Easting[j]-spptrees$Easting[k])^2+(seedling_gps2$Northing[j]-spptrees$Northing[k])^2) 	#calculate tree-trap 
																#distances for each site
		}
		}
    
#now for each row of distances.i, pull out the minimum value in that row, and print it in seedling_gps in mindist column
    
    for (i in 1:nrow(seedling_gps2)){
      seedling_gps2$mindist[i]<-min(distances.i[i,], na.rm=T)
      }

### Analysis #######

mod1<-lmer(mindist~disperser+(1|Block), data=seedling_gps2[!is.na(seedling_gps2$mindist)&seedling_gps2$mindist<Inf & seedling_gps2$GenSp!="unknown unknown" & seedling_gps2$Species!="unknown",])
confint(mod1) #animal dispersed seeds go further than wind
    
    
## Graphing ########
ggplot(seedling_gps2[!is.na(seedling_gps2$mindist)&seedling_gps2$mindist<Inf & seedling_gps2$Species!="unknown",], aes(GenSp, mindist))+
  geom_boxplot()+
  ylab("Distance to nearest conspecific (m)")+
  scale_x_discrete(name = "Species")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Graphics/mindist_spp.png")

ggplot(seedling_gps2[!is.na(seedling_gps2$disperser) & seedling_gps2$mindist<Inf & seedling_gps2$Species!= "unknown",], aes(disperser, mindist))+
  geom_boxplot()+
  ylab("Distance to nearest conspecific (m)")+
  scale_x_discrete(name = "Dispersal mode")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Graphics/mindist_disperser.png")
