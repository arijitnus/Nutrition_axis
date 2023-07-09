#Date: 20.04.2023
#load packages
libs<-list("ggplot2","dplyr","readxl","vegan","phyloseq","GUniFrac")
lapply(libs, require,character.only=T)

#read in ASV tab, taxa tab and metadata
asv_tab<-read_excel("/Users/arijitmukherjee/Desktop/Leaf_repeat/Leaf_repeat_master_sheet.xlsx",
                   sheet = "asv_tab",col_names = T,skip = 0)
taxa_tab<-read_excel("/Users/arijitmukherjee/Desktop/Leaf_repeat/Leaf_repeat_master_sheet.xlsx",
                        sheet = "taxa_tab",col_names = T,skip = 0)
metadata<-read_excel("/Users/arijitmukherjee/Desktop/Leaf_repeat/Leaf_repeat_master_sheet.xlsx",
                     sheet = "metadata",col_names = T,skip = 0)
head(asv_tab)
head(taxa_tab)
head(metadata)
metadata$Niche<-as.factor(metadata$Niche)
metadata$Genotype<-as.factor(metadata$Genotype)
metadata$Plate<-as.factor(metadata$Plate)
metadata$Experiment<-as.factor(metadata$Experiment)

asv_tab<-as.data.frame(asv_tab)
taxa_tab<-as.data.frame(taxa_tab)
metadata<-as.data.frame(metadata)


###Remove the rows containing chloroplast/mitochondria/no kingdom assignments

##merge the asv_tab with taxa tab
asv_tax_tab<-merge(asv_tab,taxa_tab,by="ASVs")
head(asv_tax_tab)

##filter 
asv_tax_tab<-asv_tax_tab%>%filter(Order!="Chloroplast"&Family!="Mitochondria")
asv_tax_tab<-asv_tax_tab%>%filter(Kingdom=="Bacteria")
asv_tax_tab<-asv_tax_tab%>%filter(Phylum!="NA")
dim(asv_tax_tab)#5083 ASVs
names(asv_tax_tab)
#Plot the diversity of each of the samples

shannon<-diversity(t(asv_tax_tab[,2:73]),MARGIN = 1)
class(shannon)
metadata$shannon<-shannon
head(metadata)
dim(metadata)
#remove NC, syncom and ND samples
metadata<-metadata%>%filter(Genotype!="ND")
metadata<-metadata%>%filter(Niche!="Syncom")
metadata<-metadata%>%filter(Niche!="NC")

cols=c("#8B4513",'#000000',"#008000","#DC143C","#000099","#808000")

head(metadata)

#Now remove samples with less than 3 Shannon diversity

metadata_outrem<-metadata%>%filter(shannon>3)

leaf_alpha<-ggplot(metadata_outrem,aes(x=Genotype,y=shannon))+
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
q<-leaf_alpha+scale_y_continuous(limits = c(3, 6.5), expand = expansion(mult = c(0, 0)))
q

ggsave(
  "shannon_leaf_final.tiff",
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
metadata_outrem
shapiro.test(metadata_outrem$shannon)#not normal distribution
library(lawstat)
levene.test(metadata_outrem$shannon,metadata_outrem$Genotype)#homogeneous
library(dunn.test)
kruskal.test(metadata_outrem$shannon,metadata_outrem$Genotype)
dunn.test(metadata_outrem$shannon,metadata_outrem$Genotype, wrap=TRUE, method="bh")
### compact letter display
levels(metadata_outrem$Genotype)<-list(bulksoil="bulk soil",Col="Col-0",msa="msa1-3",sdi="sdi2-1",
                              sultr21="sultr2;1",sultr22="sultr2;2")
library(rcompanion)
library(multcomp)
dt <- dunn.test(metadata_outrem$shannon,metadata_outrem$Genotype, wrap=TRUE, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#Group Letter MonoLetter
#1 bulksoil      b          b
#2      Col      a         a 
#3      msa      a         a 
#4      sdi      a         a 
#  sultr21      a         a 
#6  sultr22      a         a  



































































































