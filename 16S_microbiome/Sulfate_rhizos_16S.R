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

metadata<-na.omit(metadata)
dim(metadata)
asv_tab2<-select(asv_tab,metadata$Samples)
dim(asv_tab2)
taxa_tab
asv_tab2$ASVs<-asv_tab$ASVs

asv_tax_tab<-merge(asv_tab2,taxa_tab,by="ASVs")
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
asv_tax_tab<-asv_tax_tab[,1:43]
class(asv_tax_tab)
rownames(asv_tax_tab)<-asv_tax_tab$ASVs
asv_tax_tab<-asv_tax_tab[,-1]

dim(asv_tax_tab)
names(asv_tax_tab)
#Now let's perform CAP based on at least 10 total reads and and with at least 1 read in samples
#First with at least 10 reads
asv_tax_tab$sum<-rowSums(asv_tax_tab)
asv_tab10<-asv_tax_tab%>%filter(sum>=10)
dim(asv_tab10)#1643 ASVs
names(asv_tab10)
asv_tab10<-asv_tab10[,-43]
asv_tab10<-t(asv_tab10)
head(asv_tab10)


#Now perform CAP on this table
asv_tab10_ra<-sweep(asv_tab10,1,rowSums(asv_tab10),"/")
asv_tab10_ra
metadata$sulfate
set.seed(1514)
cs_rhizo10<-capscale(asv_tab10_ra~sulfate+Condition(Experiment)+Condition(plate),
                     data = metadata,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_rhizo10,permutations = 999)


#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tab10_ra ~ sulfate + Condition(Experiment) + Condition(plate), data = metadata, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)  
#Model     1   0.1315 1.1819  0.094 .
#Residual 37   4.1166                
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1    

summary(cs_rhizo10)

#Call:
 # capscale(formula = asv_tab10_ra ~ sulfate + Condition(Experiment) +      Condition(plate), data = metadata, distance = "bray", sqrt.dist = T,      add = F) 

#Partitioning of Bray distance:
 # Inertia Proportion
#Total          7.5689    1.00000
#Conditioned    3.3209    0.43875
#Constrained    0.1315    0.01737
#Unconstrained  4.1166    0.54388


















