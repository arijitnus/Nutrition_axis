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
metadata_filt<-metadata%>%filter(sample%in%rownames(asv_tax_tab_outrem_rrf))
dim(metadata_filt)
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
asv_tax_tab_outrem_rrf_ra<-asv_tax_tab_outrem_rrf_ra[metadata_filt$sample,]

####Now we have performed ANOVA with bulk soil samples
# we shall now perform CAP w/o bulk soil samples
#plot the CAP plot with out bulk soil and only among the genotypes
unique(metadata_filt$Genotype)
metadata_genotype<-metadata_filt%>%filter(Genotype%in%c("Col-0","msa1-3","sdi2-1","sultr2;1","sultr2;2"))
asv_tax_tab_outrem_rrf_ra_genotypes<-asv_tax_tab_outrem_rrf_ra[metadata_genotype$sample,]

unique(metadata_genotype$Genotype)

set.seed(1231)
cs_leaf_withoutbulk<-capscale(asv_tax_tab_outrem_rrf_ra_genotypes~Genotype+Condition(Experiment)+Condition(Plate),
                  data = metadata_genotype,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_leaf_withoutbulk,permutations = 999)#0.008 pvalue
#Model: capscale(formula = asv_tax_tab_outrem_rrf_ra_genotypes ~ Genotype + Condition(Experiment) + Condition(Plate), data = metadata_genotype, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)   
#Model     4   1.1514 1.2126  0.008 **
 # Residual 46  10.9197                 
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


eig=cs_leaf_withoutbulk$CCA$eig
var1=format(100 * eig[1] / sum(eig))
var2=format(100 * eig[2] / sum(eig))
var3=format(100 * eig[3] / sum(eig))
var4=format(100 * eig[4] / sum(eig))
var5=format(100 * eig[5] / sum(eig))
vars<-c(var1,var2,var3,var4,var5)
vars
#CAP1       CAP2       CAP3       CAP4       <NA> 
#"40.24422" "22.44522" "20.10937" "17.20119"       "NA"
cs_withoutbulk_df<-cs_leaf_withoutbulk$CCA$wa
class(cs_withoutbulk_df)
cs_withoutbulk_df<-as.data.frame(cs_withoutbulk_df)
cs_withoutbulk_df$Genotype<-metadata_genotype$Genotype
cs_withoutbulk_df$Experiment<-metadata_genotype$Experiment
head(cs_withoutbulk_df)
write.table(cs_withoutbulk_df,"lead_cap_df.tsv",sep = "\t")

#without bulk soil
cols <- c('#000000',"#008000","#DC143C","#000099","#808000")
x<-cs_withoutbulk_df%>%group_by(Genotype)%>%summarise(mean=mean(CAP1))
y<-cs_withoutbulk_df%>%group_by(Genotype)%>%summarise(mean=mean(CAP2))
x
y

library(ggplot2)
p<-ggplot(cs_withoutbulk_df,aes(x=CAP1,y=CAP2,color=Genotype))+
  geom_point(aes(shape=Experiment,size=0.4,alpha=0.6))+
  theme_classic()+
  xlab("CAP1 (40.24%)")+
  ylab("CAP2 (22.44%)")+
  scale_color_manual(values=cols)+
  stat_ellipse(level = 0.8,linetype=2)+
  theme(axis.text.x = element_text(size = 14,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-p+geom_segment(aes(x = -0.114, y = -0.0726, xend = 0.0689, yend = 0.0711),
                  arrow = arrow(length = unit(0.5, "cm")),colour="black")
r<-q+geom_segment(aes(x = -0.114, y = -0.0726, xend = 0.291, yend = -0.0488),
                  arrow = arrow(length = unit(0.5, "cm")),colour="black")
r

saveRDS(p,"CAP_leaf_plot.rds")
plot<-readRDS("CAP_leaf_plot.rds")

ggsave(
  "CAP1_CAP2_leaf_wosoil_final_with_arrow.tiff",
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

#msa
set.seed(12341)
metadata_genotype_msa<-metadata_genotype%>%filter(Genotype%in%c("Col-0","msa1-3"))
asv_tax_tab_outrem_rrf_ra_msa<-asv_tax_tab_outrem_rrf_ra_genotypes[metadata_genotype_msa$sample,]

cs_leaf_msa<-capscale(asv_tax_tab_outrem_rrf_ra_msa~Genotype+Condition(Experiment)+Condition(Plate),
                      data = metadata_genotype_msa,add=F,sqrt.dist=T,distance="bray")
anova(cs_leaf_msa,nperm=999)#0.609; NS; F=0.98

#sdi
metadata_genotype_sdi<-metadata_genotype%>%filter(Genotype%in%c("Col-0","sdi2-1"))
asv_tax_tab_outrem_rrf_ra_sdi<-asv_tax_tab_outrem_rrf_ra_genotypes[metadata_genotype_sdi$sample,]

cs_leaf_sdi<-capscale(asv_tax_tab_outrem_rrf_ra_sdi~Genotype+Condition(Experiment)+Condition(Plate),
                      data = metadata_genotype_sdi,add=F,sqrt.dist=T,distance="bray")
anova(cs_leaf_sdi,nperm=999)#0.221; NS; F=1.03

#sultr 21
metadata_genotype_sultr21<-metadata_genotype%>%filter(Genotype%in%c("Col-0","sultr2;1"))
asv_tax_tab_outrem_rrf_ra_sultr21<-asv_tax_tab_outrem_rrf_ra_genotypes[metadata_genotype_sultr21$sample,]

cs_leaf_sultr21<-capscale(asv_tax_tab_outrem_rrf_ra_sultr21~Genotype+Condition(Plate),
                      data = metadata_genotype_sultr21,add=F,sqrt.dist=T,distance="bray")
anova(cs_leaf_sultr21,nperm=999)#F=1.55,p=0.035

#sultr 22
metadata_genotype_sultr22<-metadata_genotype%>%filter(Genotype%in%c("Col-0","sultr2;2"))
asv_tax_tab_outrem_rrf_ra_sultr22<-asv_tax_tab_outrem_rrf_ra_genotypes[metadata_genotype_sultr22$sample,]

cs_leaf_sultr22<-capscale(asv_tax_tab_outrem_rrf_ra_sultr22~Genotype+Condition(Plate)+Condition(Experiment),
                          data = metadata_genotype_sultr22,add=F,sqrt.dist=T,distance="bray")
anova(cs_leaf_sultr22,nperm=999)#0.001
#Permutation: free
#Number of permutations: 999

#Model: capscale(formula = asv_tax_tab_outrem_rrf_ra_sultr22 ~ Genotype + Condition(Plate) + Condition(Experiment), data = metadata_genotype_sultr22, distance = "bray", sqrt.dist = T, add = F)
#Df SumOfSqs      F Pr(>F)    
#Model     1   0.4061 1.7101  0.001 ***
 # Residual 17   4.0372 
#combine results from all pairwise permanova for leaves



summary(cs_leaf_withoutbulk)
#Partitioning of Bray distance:
#Inertia Proportion
#Total          14.985    1.00000
#Conditioned     2.913    0.19443
#Constrained     1.151    0.07684
#Unconstrained  10.920    0.72873


















