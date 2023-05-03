#Rhizosphere PcoA plot
#Date: 24.04.2023
#Leaf repeat Sequencing samples processing
#DADA2 done
#load packages
libs<-list("ggplot2","dplyr","readxl","vegan","phyloseq","GUniFrac")
lapply(libs, require,character.only=T)

#read in ASV tab, taxa tab and metadata
asv_tab<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/ASV_table_master.xlsx",
                    sheet = "rhizo_outrem_rrf",col_names = T,skip = 0)
head(asv_tab)
taxa_tab<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/ASV_table_master.xlsx",
                     sheet = "taxa_outrem_rhizo",col_names = T,skip = 0)
metadata<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/ASV_table_master.xlsx",
                     sheet = "master_metadata",col_names = T,skip = 0)

asv_tax_tab<-merge(asv_tab,taxa_tab,by="ASVs")
head(asv_tax_tab)
dim(metadata)#181 x 5
head(metadata)
metadata<-metadata%>%filter(Niche=="soil")
names(asv_tax_tab)
metadata<-metadata%>%filter(Samples%in%colnames(asv_tax_tab[,2:61]))
dim(metadata)#Now the metdata has 60 samples same as the number of samples for asv table
##filter 
asv_tax_tab<-asv_tax_tab%>%filter(Order!="Chloroplast"&Family!="Mitochondria")
asv_tax_tab<-asv_tax_tab%>%filter(Kingdom=="Bacteria")
asv_tax_tab<-asv_tax_tab%>%filter(Phylum!="NA")
dim(asv_tax_tab)#2569 ASVs



set.seed(1513)
dim(metadata)
metadata$Genotype<-as.factor(metadata$Genotype)
metadata$plate<-as.factor(metadata$plate)
metadata$Experiment<-as.factor(metadata$Experiment)
head(metadata)
#Now the metadata is ready
###Remove the rows containing chloroplast/mitochondria/no kingdom assignments
dim(asv_tax_tab)
#now remove the taxonomic info of all the ASVs
asv_tax_tab<-asv_tax_tab[,1:61]
class(asv_tax_tab)
rownames(asv_tax_tab)<-asv_tax_tab$ASVs
asv_tax_tab<-asv_tax_tab[,-1]
dim(asv_tax_tab)
#Now let's perform CAP based on at least 10 total reads and and with at least 1 read in samples
#First with at least 10 reads
asv_tax_tab$sum<-rowSums(asv_tax_tab)
asv_tab10<-asv_tax_tab%>%filter(sum>=10)
dim(asv_tab10)#1643 ASVs
names(asv_tab10)
asv_tab10<-asv_tab10[,-61]
asv_tab10<-t(asv_tab10)
head(asv_tab10)

#try without removing any ASV
head(asv_tax_tab)
asv_tab<-asv_tax_tab[,-61]


#Now perform CAP on this table
asv_tab10_ra<-sweep(asv_tab10,1,rowSums(asv_tab10),"/")
asv_tab10_ra


cs_rhizo10<-capscale(asv_tab_ra~Genotype+Condition(Experiment)+Condition(plate),
                              data = metadata,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_rhizo10,permutations = 999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab10_ra ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)    
#Model     4   0.5690 1.3136  0.001 ***
  #Residual 52   5.6306                  
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(cs_rhizo10)

#Partitioning of Bray distance:
  #Inertia Proportion
#Total          10.143    1.00000
#Conditioned     3.943    0.38877
#Constrained     0.569    0.05609
#Unconstrained   5.631    0.55513
eig=cs_rhizo10$CCA$eig
var1=format(100 * eig[1] / sum(eig))
var2=format(100 * eig[2] / sum(eig))
var3=format(100 * eig[3] / sum(eig))
var4=format(100 * eig[4] / sum(eig))
var5=format(100 * eig[5] / sum(eig))
vars<-c(var1,var2,var3,var4,var5)
vars
#CAP1       CAP2       CAP3       CAP4        
#"42.25123" "22.98929"  "18.3023" "16.45718"       

cs_rhizo10_df<-cs_rhizo10$CCA$wa
class(cs_rhizo10_df)
cs_rhizo10_df<-as.data.frame(cs_rhizo10_df)
head(cs_rhizo10_df)
head(metadata)
cs_rhizo10_df$Genotype<-metadata$Genotype
cs_rhizo10_df$Experiment<-metadata$Experiment
head(cs_rhizo10_df)
write.table(cs_rhizo10_df,"rhizo10_cap_df.tsv",sep = "\t")

####plot the CAP plot based on this result

cols <- c('#000000',"#008000","#DC143C","#000099","#808000")
class(cs_rhizo10_df$Genotype)


library(ggplot2)
p<-ggplot(cs_rhizo10_df,aes(x=CAP1,y=CAP2,color=Genotype))+
  geom_point(aes(shape=Experiment,size=0.4,alpha=0.6))+
  theme_classic()+
  xlab("CAP1 (42.25%)")+
  ylab("CAP2 (22.98%)")+
  scale_color_manual(values=cols)+
  stat_ellipse(level = 0.8,linetype=2)+
  theme(axis.text.x = element_text(size = 14,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
p
ggsave(
  "CAP_rhiz10.tiff",
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


saveRDS(p,"CAP_rhizo10_plot.rds")
plot<-readRDS("CAP_rhizo10_plot.rds")

#perform pairwise PERMANOVA
metadata_msa$
#msa
set.seed(12341)
metadata_msa<-metadata%>%filter(Genotype%in%c("Col-0","msa"))
asv_tab10_ra_msa<-asv_tab10_ra[metadata_msa$Samples,]

cs_rhizo_msa<-capscale(asv_tab10_ra_msa~Genotype+Condition(Experiment)+Condition(plate),
                      data = metadata_msa,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_msa,nperm=999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab10_ra_msa ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_msa, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)  
#Model     1  0.16549 1.4903  0.012 *
 # Residual 24  2.66503                
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#sdi2-1
set.seed(12342)
metadata_sdi<-metadata%>%filter(Genotype%in%c("Col-0","sdi 2;1"))
asv_tab10_ra_sdi<-asv_tab10_ra[metadata_sdi$Samples,]

cs_rhizo_sdi<-capscale(asv_tab10_ra_sdi~Genotype+Condition(Experiment)+Condition(plate),
                       data = metadata_sdi,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_sdi,nperm=999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab10_ra_sdi ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_sdi, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)    
#Model     1  0.20892 1.8727  0.001 ***
  #Residual 23  2.56587                  
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#sultr2;1
set.seed(12343)
metadata_sultr21<-metadata%>%filter(Genotype%in%c("Col-0","sultr2;1"))
asv_tab10_ra_sultr21<-asv_tab10_ra[metadata_sultr21$Samples,]

cs_rhizo_sultr21<-capscale(asv_tab10_ra_sultr21~Genotype+Condition(Experiment)+Condition(plate),
                       data = metadata_sultr21,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_sultr21,nperm=999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999
#
#Model: capscale(formula = asv_tab10_ra_sultr21 ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_sultr21, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)
#Model     1  0.09312 0.9129    0.7
#Residual 17  1.73404   

#sultr2;2
set.seed(12344)
metadata_sultr22<-metadata%>%filter(Genotype%in%c("Col-0","sultr2;2"))
asv_tab10_ra_sultr22<-asv_tab10_ra[metadata_sultr22$Samples,]

cs_rhizo_sultr22<-capscale(asv_tab10_ra_sultr22~Genotype+Condition(Experiment)+Condition(plate),
                           data = metadata_sultr22,add=F,sqrt.dist=T,distance="bray")
anova(cs_rhizo_sultr22,nperm=999)
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab10_ra_sultr22 ~ Genotype + Condition(Experiment) + Condition(plate), data = metadata_sultr22, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)
#Model     1  0.11598 1.0855   0.28
#Residual 21  2.24361



















