#calculate mindist
#1) loop over each row of seedling_gps [i]
#2) for every row, calculate distance to each row/tree in adultgps where seedling_gps$GenSp == adultgps$GenSp, and print in a temporary vector. This will be the same length as the number of adult trees of that spp. 
#3) pull out the minimum distance of all distances to adults. Print in a single column in seedling_gps. 
  
#Make empty matrix and then calculate distance between each tree-seedling combo for species i
    distances.i<-matrix(NA,nrow(seedling_gps),nrow(adultgps)) #make empty matrix to populate during for loop; 

#rows are adult tree individuals of spp i, columns are seedlings
		for (j in 1:nrow(seedling_gps)){
		      spptrees=subset(adultgps, adultgps$GenSp==seedling_gps$GenSp[j])
		      for (k in 1:nrow(spptrees)){
        	distances.i[j,k]=sqrt((seedling_gps$Easting[j]-spptrees$Easting[k])^2+(seedling_gps$Northing[j]-spptrees$Northing[k])^2) 	#calculate tree-trap 
																#distances for each site
		}
		}
    
#now for each row of distances.i, pull out the minimum value in that row, and print it in seedling_gps in mindist column
    
    for (i in 1:nrow(seedling_gps)){
      seedling_gps$mindist[i]<-min(distances.i[i,], na.rm=T)
      }

### Analysis #######

mod1<-lmer(mindist~disperser+(1|Block), data=seedling_gps[!is.na(seedling_gps$mindist)&seedling_gps$mindist<Inf & seedling_gps$GenSp!="unknown unknown",])
confint(mod1) #animal dispersed seeds go further than wind
    
    
## Graphing ########
ggplot(seedling_gps[!is.na(seedling_gps$mindist)&seedling_gps$mindist<Inf & seedling_gps$GenSp!="unknown unknown",], aes(GenSp, mindist))+
  geom_boxplot()+
  ylab("Distance to nearest conspecific (m)")+
  scale_x_discrete(name = "Species")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Graphics/mindist_spp.png")

ggplot(seedling_gps[!is.na(seedling_gps$mindist)&seedling_gps$mindist<Inf & seedling_gps$GenSp!="unknown unknown",], aes(disperser, mindist, color=Herbivory))+
  geom_boxplot()+
  ylab("Distance to nearest conspecific (m)")+
  scale_x_discrete(name = "Species")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Graphics/mindist_disperser.png")
