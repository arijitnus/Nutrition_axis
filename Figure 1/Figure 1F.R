#RRoot PcoA plot
#Date: 03.05.2023
#DADA2 done
#load packages
libs<-list("ggplot2","dplyr","readxl","vegan","phyloseq","GUniFrac")
lapply(libs, require,character.only=T)

#read in ASV tab, taxa tab and metadata
asv_tab<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/ASV_table_master.xlsx",
                    sheet = "endosphere",col_names = T,skip = 0)
head(asv_tab)
names(asv_tab)
metadata<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/ASV_table_master.xlsx",
                     sheet = "master_metadata",col_names = T,skip = 0)
metadata<-metadata%>%filter(Niche=="root")
dim(metadata)
#remove outlier samples with less than 10000 total reads, also remove NC and ND samples
rem_samples<-c("AMREND5C","AMREND100Y","AMREND26A","AMREND33B","AMREND75Y",
               "AMREND68B","AMREND30","AMRENDNC2","AMREND9B","AMRENDNC1","AMRENDS")
asv_tab_outrem<-select(asv_tab,select = -rem_samples)

names(asv_tab_outrem)#49 samples pass QC

metadata<-metadata%>%filter(Samples%in%colnames(asv_tab_outrem[,2:50]))
dim(metadata)#Now the metdata has 49 samples same as the number of samples for asv table
##filter 
asv_tab_outrem<-asv_tab_outrem%>%filter(Order!="Chloroplast"&Family!="Mitochondria")
asv_tab_outrem<-asv_tab_outrem%>%filter(Kingdom=="Bacteria")
asv_tab_outrem<-asv_tab_outrem%>%filter(Phylum!="NA")
dim(asv_tab_outrem)#3977 ASVs
names(asv_tab_outrem)
asv_tab_outrem<-asv_tab_outrem[,1:50]
class(asv_tab_outrem)
asv_tab_outrem<-as.data.frame(asv_tab_outrem)
rownames(asv_tab_outrem)<-asv_tab_outrem$ASVs
asv_tab_outrem<-asv_tab_outrem[,-1]
#remove ASVs withut a minimum of 10 reads
asv_tab_outrem$sum<-rowSums(asv_tab_outrem)
asv_tab_outrem<-asv_tab_outrem%>%filter(sum>=10)
names(asv_tab_outrem)
asv_tab_outrem<-asv_tab_outrem[,-50]
dim(asv_tab_outrem)#1526 ASVs
#Now perform other operations
set.seed(1515)
names(asv_tab_outrem)
asv_tab_outrem_rrf<-Rarefy(t(asv_tab_outrem),
                               depth = min(rowSums(t(asv_tab_outrem))))$otu.tab.rff
rowSums(asv_tab_outrem_rrf)
rownames(asv_tab_outrem_rrf)#9773 reads per sample 

#convert this to rel abundance
rowSums(asv_tab_outrem_rrf)
class(asv_tab_outrem_rrf)
asv_tab_outrem_rrf<-as.data.frame(asv_tab_outrem_rrf)
asv_tab_outrem_rrf_ra<-sweep(asv_tab_outrem_rrf,1,rowSums(asv_tab_outrem_rrf),"/")
dim(asv_tab_outrem_rrf_ra)
metadata$Genotype<-as.factor(metadata$Genotype)
metadata$Experiment<-as.factor(metadata$Experiment)
metadata$plate<-as.factor(metadata$plate)
#Now perform CAP based on ANOVA
set.seed(1232)
cs_root<-capscale(asv_tab_outrem_rrf_ra~Genotype+Condition(Experiment)+Condition(plate),
                              data = metadata,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_root,permutations = 999)#NS

head(metadata)
rownames(asv_tab_outrem_rrf_ra)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab_outrem_rrf_ra ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata, distance = "bray", sqrt.dist = T, add = F)
##Df SumOfSqs      F Pr(>F)
#Model     4   1.0301 1.0997  0.224
#Residual 42   9.8362 

eig=cs_root$CCA$eig
var1=format(100 * eig[1] / sum(eig))
var2=format(100 * eig[2] / sum(eig))
var3=format(100 * eig[3] / sum(eig))
var4=format(100 * eig[4] / sum(eig))
var5=format(100 * eig[5] / sum(eig))
vars<-c(var1,var2,var3,var4,var5)
vars
#CAP1       CAP2       CAP3       CAP4       <NA> 
#"48.06919" "19.18454" "16.94301" "15.80326"
summary(cs_root)
#Partitioning of Bray distance:
#Inertia Proportion
#Total          12.207    1.00000
#Conditioned     1.341    0.10985
#Constrained     1.030    0.08439
#Unconstrained   9.836    0.80577
cols <- c('#000000',"#008000","#DC143C","#000099","#808000")
cs_root_df<-cs_root$CCA$wa
cs_root_df<-as.data.frame(cs_root_df)
cs_root_df$Genotype<-metadata$Genotype
cs_root_df$Experiment<-metadata$Experiment
cs_root_df
x<-cs_root_df%>%group_by(Genotype)%>%summarise(mean=mean(CAP1))
y<-cs_root_df%>%group_by(Genotype)%>%summarise(mean=mean(CAP2))
x
y
library(ggplot2)
p<-ggplot(cs_root_df,aes(x=CAP1,y=CAP2,color=Genotype))+
  geom_point(aes(shape=Experiment,size=0.4,alpha=0.6))+
  theme_classic()+
  xlab("CAP1 (48.06%)")+
  ylab("CAP2 (19.18%)")+
  scale_color_manual(values=cols)+
  stat_ellipse(level = 0.8,linetype=2)+
  theme(axis.text.x = element_text(size = 14,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-p+geom_segment(aes(x = -0.183, y = 0.112, xend = 0.193, yend = -0.101),
                  arrow = arrow(length = unit(0.5, "cm")),colour="black")
q
ggsave(
  "CAP_root_with_arrow.tiff",
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


#perform pairwise PERMANOVA
saveRDS(p,"CAP_root_plot.rds")
#msa
set.seed(12349)
metadata_msa<-metadata%>%filter(Genotype%in%c("Col-0","msa1-3"))
asv_tab_outrem_rrf_ra_msa<-asv_tab_outrem_rrf_ra[metadata_msa$Samples,]

cs_root_msa<-capscale(asv_tab_outrem_rrf_ra_msa~Genotype+Condition(Experiment)+Condition(plate),
                       data = metadata_msa,add=F,sqrt.dist=T,distance="bray")
anova(cs_root_msa,nperm=999)
#Model: capscale(formula = asv_tab_outrem_rrf_ra_msa ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_msa, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)
##Model     1   0.1877 0.8008  0.695
#Residual 18   4.2180   

#sdi
set.seed(12350)
metadata_sdi<-metadata%>%filter(Genotype%in%c("Col-0","sdi2-1"))
asv_tab_outrem_rrf_ra_sdi<-asv_tab_outrem_rrf_ra[metadata_sdi$Samples,]

cs_root_sdi<-capscale(asv_tab_outrem_rrf_ra_sdi~Genotype+Condition(Experiment)+Condition(plate),
                      data = metadata_sdi,add=F,sqrt.dist=T,distance="bray")
anova(cs_root_sdi,nperm=999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab_outrem_rrf_ra_sdi ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_sdi, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)  
#Model     1   0.3527 1.5281   0.06 .
#Residual 19   4.3849                
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#sultr21
set.seed(12350)
metadata_sultr21<-metadata%>%filter(Genotype%in%c("Col-0","sultr2;1"))
asv_tab_outrem_rrf_ra_sultr21<-asv_tab_outrem_rrf_ra[metadata_sultr21$Samples,]

cs_root_sultr21<-capscale(asv_tab_outrem_rrf_ra_sultr21~Genotype+Condition(Experiment)+Condition(plate),
                      data = metadata_sultr21,add=F,sqrt.dist=T,distance="bray")
anova(cs_root_sultr21,nperm=999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab_outrem_rrf_ra_sultr21 ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_sultr21, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)
#Model     1  0.16209 0.7482  0.829
#Residual 12  2.59974 

#sultr2;2
set.seed(12355)
metadata_sultr22<-metadata%>%filter(Genotype%in%c("Col-0","sultr2;2"))
asv_tab_outrem_rrf_ra_sultr22<-asv_tab_outrem_rrf_ra[metadata_sultr22$Samples,]

cs_root_sultr22<-capscale(asv_tab_outrem_rrf_ra_sultr22~Genotype+Condition(Experiment)+Condition(plate),
                          data = metadata_sultr22,add=F,sqrt.dist=T,distance="bray")
anova(cs_root_sultr22,nperm=999)
#Number of permutations: 999

#Model: capscale(formula = asv_tab_outrem_rrf_ra_sultr22 ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_sultr22, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)   
#Model     1   0.4585 2.1875  0.009 **
 # Residual 17   3.5635                 
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
































