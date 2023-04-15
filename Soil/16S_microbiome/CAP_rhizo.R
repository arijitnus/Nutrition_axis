#replotting the CAP plot based on new codes from leaf microbiome analyses
#ASV tables is the rarefied and outlier samples removed
library(readxl)
library(ggplot2)
library(dplyr)
rhizo_asv_tab<-read_excel("/Users/arijitmukherjee/Downloads/rhizo_outrem_rrf.xlsx",sheet = "ASV_tab",col_names = T,skip = 0)
rhizo_metadata<-read_excel("/Users/arijitmukherjee/Downloads/rhizo_outrem_rrf.xlsx",sheet = "Metadata",col_names = T,skip = 0)
rhizo_tax<-read_excel("/Users/arijitmukherjee/Downloads/rhizo_outrem_rrf.xlsx",sheet = "taxonomy",col_names = T,skip = 0)

#The dataframe is rarefied and needs to be used for downstream analyses
#rarefied to 17705 reads in each sample
#the dataframe may contain ASVs with zero reads across samples, remember to remove them
rhizo_asv_tab<-as.data.frame(rhizo_asv_tab)
rhizo_metadata<-as.data.frame(rhizo_metadata)
rhizo_tax<-as.data.frame(rhizo_tax)

#filter the ASV tab for zero read ASVs
rownames(rhizo_asv_tab)<-rhizo_asv_tab$ASVs
rhizo_asv_tab<-rhizo_asv_tab[,-1]
rhizo_asv_tab$sum<-rowSums(rhizo_asv_tab)
rhizo_asv_tab<-rhizo_asv_tab%>%filter(sum!=0)
dim(rhizo_asv_tab)#2255 ASVs and 60 samples here

#Now remove the sum column
rhizo_asv_tab<-rhizo_asv_tab[,-61]
names(rhizo_asv_tab)
names(rhizo_metadata)
#Filter the metadata and taxnomoy table based on ASV table
rhizo_metadata<-rhizo_metadata%>%filter(Samples%in%colnames(rhizo_asv_tab))
dim(rhizo_metadata)
rhizo_metadata$Genotype<-as.factor(rhizo_metadata$Genotype)
rhizo_metadata$Experiment<-as.factor(rhizo_metadata$Experiment)
rhizo_metadata$plate<-as.factor(rhizo_metadata$plate)

rhizo_tax<-rhizo_tax%>%filter(ASVs%in%rownames(rhizo_asv_tab))
dim(rhizo_tax)

####Now you have all these datasets ready for analyses
set.seed(1514)
head(rhizo_asv_tab)
rhizo_asv_tab<-t(rhizo_asv_tab)
rhizo_asv_tab_ra<-sweep(rhizo_asv_tab,1,rowSums(asv_tax_tab_outrem_rrf),"/")
#ignore the warning 
library(vegan)
cs_rhizo<-capscale(rhizo_asv_tab_ra~Genotype+Condition(Experiment)+Condition(plate),
                  data = rhizo_metadata,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_rhizo,permutations = 999)
term<-anova.cca(cs_rhizo)
term
p.val <- term[1, 4]
p.val#0.001

eig=cs_rhizo$CCA$eig
var1=format(100 * eig[1] / sum(eig))
var2=format(100 * eig[2] / sum(eig))
var3=format(100 * eig[3] / sum(eig))
var4=format(100 * eig[4] / sum(eig))
var5=format(100 * eig[5] / sum(eig))
vars<-c(var1,var2,var3,var4,var5)
vars#
#CAP1       CAP2       CAP3       CAP4       <NA> 
#"42.15138" "23.00514" "18.34896" "16.49452" 

cs_rhizo_df<-cs_rhizo$CCA$wa
class(cs_rhizo_df)
cs_rhizo_df<-as.data.frame(cs_rhizo_df)
cs_rhizo_df$Genotype<-rhizo_metadata$Genotype
cs_rhizo_df$Experiment<-rhizo_metadata$Experiment
head(cs_rhizo_df)









#Model: capscale(formula = rhizo_asv_tab_ra ~ Genotype + Condition(Experiment) + Condition(plate), data = rhizo_metadata, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)    
#Model     4   0.5717 1.3102  0.001 ***
#Residual 52   5.6727     
#plot CAP1 and CAP3 for the five genotypes

cols=c('#000000',"#008000","#DC143C","#FF1493","#808000")

library(ggplot2)
p<-ggplot(cs_rhizo_df,aes(x=CAP1,y=CAP2,color=Genotype))+
  geom_point(aes(shape=Experiment,size=3,alpha=0.7))+
  theme_classic()+
  xlab("CAP1 (42.15%)")+
  ylab("CAP2 (23%)")+
  scale_color_manual(values=cols)+
  stat_ellipse(level = 0.8,linetype=2)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))+
  theme(legend.position = "top",legend.text = element_text(size = 12))+
  theme(legend.title = element_blank())+
  theme(legend.key.height = unit(0.8,'cm'))+
  theme(legend.key.width = unit(0.8,'cm'))
p
ggsave(
  "CAP1_CAP2_leaf_bulksoil.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 7,
  height = 8,
  units = "in",
  dpi = 400,
)
dev.off()

rhizo_metadata$Samples
head(rhizo_metadata)
#calculate pairwise PERMANOVA
#msa
rhizo_metadata_msa<-rhizo_metadata%>%filter(Genotype%in%c("Col-0","msa"))
rhizo_asv_tab_ra_msa<-rhizo_asv_tab_ra[rhizo_metadata_msa$Samples,]

cs_rhizo_msa<-capscale(rhizo_asv_tab_ra_msa~Genotype+Condition(Experiment)+Condition(plate),
                      data = rhizo_metadata_msa,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_msa,nperm=999)#1.4851; P=0.003

#for sdi
rhizo_metadata_sdi<-rhizo_metadata%>%filter(Genotype%in%c("Col-0","sdi 2;1"))
rhizo_asv_tab_ra_sdi<-rhizo_asv_tab_ra[rhizo_metadata_sdi$Samples,]

cs_rhizo_sdi<-capscale(rhizo_asv_tab_ra_sdi~Genotype+Condition(Experiment)+Condition(plate),
                       data = rhizo_metadata_sdi,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_sdi,nperm=999)####F=1.86; P=0.001

#for sultr2;1
rhizo_metadata_sultr21<-rhizo_metadata%>%filter(Genotype%in%c("Col-0","sultr2;1"))
rhizo_asv_tab_ra_sultr21<-rhizo_asv_tab_ra[rhizo_metadata_sultr21$Samples,]

cs_rhizo_sultr21<-capscale(rhizo_asv_tab_ra_sultr21~Genotype+Condition(Experiment)+Condition(plate),
                       data = rhizo_metadata_sultr21,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_sultr21,nperm=999)#NS


#for sultr2;2
rhizo_metadata_sultr22<-rhizo_metadata%>%filter(Genotype%in%c("Col-0","sultr2;2"))
rhizo_asv_tab_ra_sultr22<-rhizo_asv_tab_ra[rhizo_metadata_sultr22$Samples,]

cs_rhizo_sultr22<-capscale(rhizo_asv_tab_ra_sultr22~Genotype+Condition(Experiment)+Condition(plate),
                           data = rhizo_metadata_sultr22,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_sultr22,nperm=999)#NS




















