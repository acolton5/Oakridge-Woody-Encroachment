## Make graph of # seedlings of each spp by disperser mode
#########################
#load libraries
library(lme4)
library(ggplot2)
library(plyr)

#load data
Seedling <- read.csv("Data/Tidy/SeedlingData_Tidy.csv", na.strings=c("", "NA", ""))

numseedspp<-ddply(Seedling[Seedling$Species!="unknown",], .(Block, GenSp, disperser), summarize, total=length(Block))

ggplot(numseedspp, aes(reorder(GenSp, -total), total, color=disperser))+
  geom_boxplot()+
  ylab("Number of seedlings per block")+
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

numseedspp<-ddply(Seedling[Seedling$Species!="unknown",], .(Block, GenSp, Herbivory, disperser), summarize, total=length(Block))
myColors <- c("#9ecae1", "#31a354") 
ggplot(numseedspp, aes(reorder(GenSp, -total), total, fill=disperser))+
  geom_boxplot()+
  ylab("Number of seedlings per block")+
  scale_x_discrete(name = "")+
  scale_fill_manual(values=myColors)+
  facet_grid(Herbivory~.)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text = element_text(size=12, face="bold"), 
    line=element_line(size=3), 
    axis.title=element_text(size=14, face="bold"), 
    legend.position="none", 
    panel.background = element_rect(fill = NA, color = "black"))

ggsave("Graphics/overallabund_figure.png", width=10, height=5, units="in")
