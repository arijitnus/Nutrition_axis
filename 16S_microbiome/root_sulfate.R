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
metadata
metadata<-na.omit(metadata)

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
asv_tab_outrem_rrf_ra$samples<-rownames(asv_tab_outrem_rrf_ra)

asv_tab_outrem_rrf_ra<-asv_tab_outrem_rrf_ra%>%filter(samples%in%metadata$Samples)
names(asv_tab_outrem_rrf_ra)[1527]

asv_tab_outrem_rrf_ra<-asv_tab_outrem_rrf_ra[,-1527]

#Now perform CAP based on ANOVA
set.seed(1232)
cs_root<-capscale(asv_tab_outrem_rrf_ra~sulfate+Condition(Experiment)+Condition(plate),
                              data = metadata,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_root,permutations = 999)#NS
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab_outrem_rrf_ra ~ sulfate + Condition(Experiment) + Condition(plate), data = metadata, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs     F Pr(>F)
#Model     1   0.2684 1.172  0.204
#Residual 30   6.8717


summary(cs_root)
#Call:
  #capscale(formula = asv_tab_outrem_rrf_ra ~ sulfate + Condition(Experiment) +      Condition(plate), data = metadata, distance = "bray", sqrt.dist = T,      add = F) 

#Partitioning of Bray distance:
  #Inertia Proportion
#Total          8.3197    1.00000
#Conditioned    1.1796    0.14178
#Constrained    0.2684    0.03227
#Unconstrained  6.8717    0.82596



















