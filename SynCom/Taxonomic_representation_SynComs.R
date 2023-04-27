df<-data.frame(Datasets=c(rep("Soil isolates",5),rep("Isolates collection",5),rep("SPAF18",5)),
               Phylum=c("Proteobacteria","Actinobacteria","Bacteroidetes","Firmicutes","Others"),
               Number=c(968,868,195,377,145,
                 33,10,0,12,0,
                 8,5,0,5,0
                 ))
df
#glboal rhizosphere database
#Proteobacteria	1388
#Actinobacteria	338
#Firmicutes	168
#Bacteroidetes	109
#others	20
library(ggplot2)
level_order<-c("Soil isolates","Isolates collection","SPAF18")
stack<-ggplot(df,aes(x=factor(Datasets,levels = level_order),y=Number,fill=Phylum))+
  geom_bar(stat = "identity",position = "fill")+
  theme_classic()+
  xlab("")+
  ylab("Proportion")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

stack

ggsave(
  "Isolates_taxonomy.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()




