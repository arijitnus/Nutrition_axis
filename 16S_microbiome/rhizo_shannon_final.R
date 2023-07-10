#plotting alpha diversity of rhizosphere and bulk soil samples
#ASV tables is the rarefied and outlier samples removed
#plot the shannon diversity from the table
rhizo_shannon<-read_excel("/Users/arijitmukherjee/Downloads/rhizo_shannon.xlsx",sheet = "alpha_rhizo2",col_names = T,skip = 0)
rhizo_shannon<-as.data.frame(rhizo_shannon)
head(rhizo_shannon)
rhizo_shannon$Genotype<-as.factor(rhizo_shannon$Genotype)
rhizo_shannon$Experiment<-as.factor(rhizo_shannon$Experiment)
#Now plot the alpha diversity boxplot and also calculate the stats
cols=c("#8B4513",'#000000',"#008000","#DC143C","#000099","#808000")

rhizo_alpha<-ggplot(rhizo_shannon,aes(x=Genotype,y=shannon))+
  geom_boxplot(aes(col=Genotype),lwd=0.8)+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shannon diversity index"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-rhizo_alpha+scale_y_continuous(limits = c(4.75, 6), expand = expansion(mult = c(0, 0)))
q
ggsave(
  "shannon_rhizo_final.tiff",
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







shapiro.test(rhizo_shannon$shannon)#non-normal, so fit the KW test
library(lawstat)
levene.test(rhizo_shannon$shannon,rhizo_shannon$Genotype)#homogeneous
library(dunn.test)
kruskal.test(rhizo_shannon$shannon,rhizo_shannon$Genotype)
dunn.test(rhizo_shannon$shannon,rhizo_shannon$Genotype, wrap=TRUE, method="bh")
### compact letter display
rhizo_shannon$Genotype
levels(rhizo_shannon$Genotype)<-list(bulksoil="Bulk soil",Col="Col-0",msa="msa1-3",sdi="sdi2-1",
                                       sultr21="sultr2;1",sultr22="sultr2;2")
library(rcompanion)
library(multcomp)
dt <- dunn.test(rhizo_shannon$shannon,rhizo_shannon$Genotype, wrap=TRUE, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#Group Letter MonoLetter
#1 bulksoil      b          b
#2      Col      a         a 
#3      msa      a         a 
#4      sdi      a         a 
#  sultr21      a         a 
#6  sultr22      a         a  









