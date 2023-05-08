#Shannon diversity for root samples 
head(asv_tab_outrem)
library(vegan)
shannon_root<-diversity(t(asv_tab_outrem),MARGIN = 1)
class(shannon_root)
shannon_root
metadata
metadata$shannon<-shannon_root
head(metadata)
dim(metadata)
write.table(metadata,"shannon_root.tsv",sep = "\t")
#Now bring in the root and bulk soil samples together for plotting shannon index
library(readxl)
library(dplyr)
library(ggplot2)
root_shannon<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_root/ASV_table_master.xlsx",sheet = "alpha_root",
                         col_names = T,skip = 0)
root_shannon
root_shannon$Genotype<-as.factor(root_shannon$Genotype)
cols=c("#8B4513",'#000000',"#008000","#DC143C","#000099","#808000")
root_shannon$Experiment<-as.factor(root_shannon$Experiment)

root_alpha<-ggplot(root_shannon,aes(x=Genotype,y=shannon))+
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
q<-root_alpha+scale_y_continuous(limits = c(3, 6.5), expand = expansion(mult = c(0, 0)))
q

ggsave(
  "shannon_root_final.tiff",
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

shapiro.test(root_shannon$shannon)#not normal distribution
library(lawstat)
levene.test(root_shannon$shannon,root_shannon$Genotype)#homogeneous
library(dunn.test)
kruskal.test(root_shannon$shannon,root_shannon$Genotype)
dunn.test(root_shannon$shannon,root_shannon$Genotype, wrap=TRUE, method="bh")
### compact letter display
levels(root_shannon$Genotype)<-list(bulksoil="bulk soil",Col="Col-0",msa="msa1-3",sdi="sdi2-1",
                                       sultr21="sultr2;1",sultr22="sultr2;2")
library(rcompanion)
library(multcomp)
dt <- dunn.test(root_shannon$shannon,root_shannon$Genotype, wrap=TRUE, method="bh")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#1 bulksoil      b          b
#2      Col      a         a 
#3      msa      a         a 
#4      sdi      a         a 
#5  sultr21      a         a 
#6  sultr22      a         a 



