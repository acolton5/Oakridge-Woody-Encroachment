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
myColors <- c("#9ecae1", "#31a354") 
ISUcolors <- c("#de2d26", "#feb24c")

ggplot(seedling_gps2[!is.na(seedling_gps2$mindist)&seedling_gps2$mindist<Inf & seedling_gps2$Species!="unknown",], aes(reorder(GenSp, -mindist), mindist, color=disperser))+
  geom_boxplot()+
  ylab("Distance to nearest\nconspecific (m)")+
  scale_x_discrete(name = "")+
  scale_color_manual(name="Dispersal\nmode",
                         breaks=c("animal", "wind"),
                        labels=c("Animal", "Wind"), 
                        values=myColors)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text = element_text(size=12, face="bold"), 
    line=element_line(size=3), 
    axis.title=element_text(size=14, face="bold"), 
    legend.title = element_text(size=14, face="bold"), 
    legend.text = element_text(size=12, face="bold"))

ggsave("Graphics/mindist_spp.png", width=8, height=6, units="in")

ggplot(seedling_gps2[!is.na(seedling_gps2$disperser) & seedling_gps2$mindist<Inf & seedling_gps2$Species!= "unknown",], aes(disperser, mindist, fill=disperser))+
  geom_boxplot()+
  ylab("Distance to nearest\nconspecific (m)")+
  scale_x_discrete(name = "Dispersal mode", breaks=c("animal", "wind"), labels=c("Animal", "Wind"))+
  scale_fill_manual(values=myColors)+
  annotate("text", x=1.5, y=300, label="***", size=10)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size=12, face="bold"), 
    line=element_line(size=3), 
    axis.title=element_text(size=14, face="bold"),
    legend.position="none") 

ggsave("Graphics/mindist_disperser.png", width=4, height=4, units="in")
