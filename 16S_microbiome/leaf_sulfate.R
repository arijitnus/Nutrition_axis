#Date: 19.05.2022
#Leaf repeat Sequencing samples processing
#DADA2 done
#load packages
libs<-list("ggplot2","dplyr","readxl","vegan","phyloseq","GUniFrac")
lapply(libs, require,character.only=T)

#read in ASV tab, taxa tab and metadata
asv_tab<-read_excel("/Users/arijitmukherjee/Desktop/Leaf_repeat/Leaf_repeat_master_sheet.xlsx",
                   sheet = "asv_tab",col_names = T,skip = 0)
taxa_tab<-read_excel("/Users/arijitmukherjee/Desktop/Leaf_repeat/Leaf_repeat_master_sheet.xlsx",
                        sheet = "taxa_tab",col_names = T,skip = 0)
metadata<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/ASV_table_master.xlsx",
                     sheet = "master_metadata",col_names = T,skip = 0)
head(asv_tab)
head(taxa_tab)
head(metadata)
metadata$Niche<-as.factor(metadata$Niche)
metadata$Genotype<-as.factor(metadata$Genotype)
metadata$plate<-as.factor(metadata$plate)
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
##remove outlier samples for leaves where you have less than 3 shannon diversity
#remove samples with less than three shannon index
#sample names #AMLEAF27C, #AMLEAF68Y, #AMLEAF47B, #AMLEAF31Y, AMLEAF80Y
rem_index<-c(33,46,23,19,55,69:80)
asv_tax_tab_outrem<-asv_tax_tab[,-rem_index]
names(asv_tax_tab_outrem)



#rarefy and then plot
set.seed(1514)
asv_tax_tab_outrem_rrf<-Rarefy(t(asv_tax_tab_outrem[,2:63]),
                               depth = min(rowSums(t(asv_tax_tab_outrem[,2:63]))))$otu.tab.rff
rowSums(asv_tax_tab_outrem_rrf)
rownames(asv_tax_tab_outrem_rrf)
metadata<-metadata%>%filter(Niche=="leaf")
metadata<-na.omit(metadata)
dim(metadata)

asv_tax_tab_outrem_rrf<-as.data.frame(asv_tax_tab_outrem_rrf)


asv_tax_tab_outrem_rrf<-asv_tax_tab_outrem_rrf%>%filter(rownames(asv_tax_tab_outrem_rrf)%in%metadata$Samples)

rownames(asv_tax_tab_outrem_rrf)
#CAP plot based on this
#remove ND sample
rownames(asv_tax_tab_outrem_rrf)
class(asv_tax_tab_outrem_rrf)
asv_tax_tab_outrem_rrf<-as.data.frame(asv_tax_tab_outrem_rrf)
asv_tax_tab_outrem_rrf<-asv_tax_tab_outrem_rrf[-50,]
rownames(asv_tax_tab_outrem_rrf)

dim(metadata_filt)
dim(asv_tax_tab_outrem_rrf)
asv_tax_tab_outrem_rrf<-as.data.frame(asv_tax_tab_outrem_rrf)
dim(metadata_filt)
metadata_filt<-metadata_filt%>%filter(sample%in%rownames(asv_tax_tab_outrem_rrf))
head(asv_tax_tab_outrem_rrf)
asv_tax_tab_outrem_rrf_ra<-sweep(asv_tax_tab_outrem_rrf,1,rowSums(asv_tax_tab_outrem_rrf),"/")

unique(metadata_filt$Genotype)
asv_tax_tab_outrem_rrf_ra<-as.data.frame(asv_tax_tab_outrem_rrf_ra)
metadata$Samples

asv_tax_tab_outrem_rrf_ra<-asv_tax_tab_outrem_rrf_ra[metadata$Samples,]

####Now we have performed ANOVA with bulk soil samples
# we shall now perform CAP w/o bulk soil samples
#plot the CAP plot with out bulk soil and only among the genotypes
unique(metadata_filt$Genotype)
metadata_genotype<-metadata_filt%>%filter(Genotype%in%c("Col-0","msa1-3","sdi2-1","sultr2;1","sultr2;2"))
asv_tax_tab_outrem_rrf_ra_genotypes<-asv_tax_tab_outrem_rrf_ra[metadata_genotype$sample,]

unique(metadata_genotype$Genotype)



set.seed(1231)
cs_leaf_withoutbulk<-capscale(asv_tax_tab_outrem_rrf_ra~sulfate+Condition(Experiment)+Condition(plate),
                  data = metadata,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_leaf_withoutbulk,permutations = 999)#
#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tax_tab_outrem_rrf_ra ~ sulfate + Condition(Experiment) + Condition(plate), data = metadata, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)
#Model     1   0.2606 1.0875  0.251
#Residual 35   8.3860


#summary(cs_leaf_withoutbulk)
#Call:
#capscale(formula = asv_tax_tab_outrem_rrf_ra ~ sulfate + Condition(Experiment) +      Condition(plate), data = metadata, distance = "bray", sqrt.dist = T,      add = F) 

#Partitioning of Bray distance:
  #Inertia Proportion
#Total         11.3534    1.00000
#Conditioned    2.7068    0.23841
#Constrained    0.2606    0.02295
#Unconstrained  8.3860    0.73864

#Eigenvalues, and their contribution to the Bray distance 
#after removing the contribution of conditiniong variables






























