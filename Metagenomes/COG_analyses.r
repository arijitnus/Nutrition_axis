#Date: 21.11.2022
#Arijit Metagenome COG functions analyses
library(readxl)
library(tidyverse)
tab_19Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "19Y",col_names = T,skip = 0)
tab_25Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "25Y",col_names = T,skip = 0)
tab_37Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "37Y",col_names = T,skip = 0)
tab_42Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "42Y",col_names = T,skip = 0)
tab_68Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "68Y",col_names = T,skip = 0)
tab_75Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "75Y",col_names = T,skip = 0)
tab_80Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "80Y",col_names = T,skip = 0)
tab_85Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "85Y",col_names = T,skip = 0)
tab_109Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "109Y",col_names = T,skip = 0)
tab_113Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "113Y",col_names = T,skip = 0)
tab_123Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "123Y",col_names = T,skip = 0)
tab_127Y<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "127Y",col_names = T,skip = 0)

df_list<-list(tab_19Y,tab_25Y,tab_37Y,tab_42Y,tab_68Y,tab_75Y,tab_80Y,tab_85Y,
              tab_109Y,tab_113Y,tab_123Y,tab_127Y)

merged_df<-reduce(df_list,full_join,by="genes")
dim(merged_df)
merged_df<-merged_df[-c(1,2),]
write.table(merged_df,"merged_df.tsv",sep = "\t")

library(metagenomeSeq)
class(merged_df)
merged_df<-as.data.frame(merged_df)
rownames(merged_df)<-merged_df$genes
colnames(merged_df)
merged_df_no_NA<-na.omit(merged_df)
dim(merged_df_no_NA)#5206 COG functions have no NA
rownames(merged_df_no_NA)<-merged_df_no_NA$genes
write.table(merged_df_no_NA,"merged_df_na_removed.tsv",sep = "\t")

#read in the final table with NA and dupliocated gene names removed
df<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "final_merged1",col_names = T,skip = 0)
head(df)
class(df)
df<-as.data.frame(df)
rownames(df)<-df$...1
head(df)
df<-df[,-1]


COG_counts<-loadMeta("final_df_msa_col.tsv",sep = "\t")
head(COG_counts$taxa)

metadata<-read_excel("/Users/arijitmukherjee/Documents/Metagenome/Prokka/COG_final.xlsx",sheet = "metadata2",col_names = T,skip = 0)
head(metadata)
metadata<-as.data.frame(metadata)
rownames(metadata)<-metadata$samples
rownames(metadata)
metadata
write.table(metadata,"metadata.tsv",sep = "\t")

ord<-match(colnames(df),rownames(metadata))
metadata<-metadata[ord,]
metadata<-AnnotatedDataFrame(metadata)

rownames(df)

obj<-newMRexperiment(df,phenoData=metadata,featureData = AnnotatedDataFrame(COG_counts$taxa),
                     libSize = c(13353270,5769952,11803981,9190849,9190849,9097727,10192831,6508191))
head(libSize(obj))
rownames(AnnotatedDataFrame(COG_counts$taxa))
#Normalization 
obj = cumNorm(obj, p = cumNormStatFast(obj))
settings = zigControl(tol=1e-10,maxit = 30, verbose = TRUE)
genotype = pData(obj)$genotype
mod = model.matrix(~genotype)
head(mod)
class(mod)
colnames(mod) = levels(genotype)
colnames(mod)

#export data of normalized counts for CAP plots
mat = MRcounts(obj, norm = TRUE, log = TRUE)
mat
#Exporting Normalization####
exportMat(mat, file = "CSS_msa_col.txt")

#contrast
res = fitZig(obj = obj, mod = mod, useCSSoffset = TRUE, control = settings)
zigFit = slot(res,"fit")
finalMod= slot(res,"fit")$design
finalMod
colnames(finalMod)<-c("Col","msa","scalingFactor")
#contrast sdi vs col-0
contrast.matrix =makeContrasts(msa - Col, levels = finalMod)
fit2 = contrasts.fit(zigFit, contrast.matrix)
fit3 = eBayes(fit2)
res<-topTable(fit3, coef=1,adjust.method ="BH",n=Inf)
write.table(res,"res_msa_col.tsv",sep = "\t")


library(vegan)
#####Read in the normalized matrix for PcoA
data_norm<-read.delim("CSS_msa_col.txt",sep = "\t",header = T)
head(data_norm)
rownames(data_norm)<-data_norm$Taxa.and.Samples
head(data_norm)
data_norm<-data_norm[,-1]
data_norm<-data_norm[,-c(2,8)]
dim(data_norm)
abund_table<-as.matrix(data_norm)
head(abund_table)
distance<-vegdist(t(abund_table),method = "bray")
sol_MDS<-cmdscale(distance,k=2,eig = T)
meta<-data.frame(genotypes=c("Col-0","msa","msa","msa","Col-0","Col-0","msa","Col-0"),fractions=rep("rhizosphere",8))
rownames(meta)<-colnames(css[,-1])
meta

###
sol_MDS$points[,1]
MDS.df<-data.frame(x=sol_MDS$points[,1],y=sol_MDS$points[,2],
                   Genus=as.factor(genotypes))
MDS.df

explainedvar1<-round(sol_MDS$eig[1]/sum(sol_MDS$eig),2)*100
explainedvar2<-round(sol_MDS$eig[2]/sum(sol_MDS$eig),2)*100
sum_var<-explainedvar1+explainedvar2
sum_var
explainedvar1
explainedvar2
adonis2(distance~as.factor(genotypes),data = data_norm,nperm=999)###P-value 0.214; F=1.16, R2=0.162

library(wesanderson)
library(ggplot2)

ggplot(MDS.df,aes(x=x,y=y,col=Genus))+
  geom_point()

sol_MDS$points
pcoA_genus<-ggplot(MDS.df,aes(x=x,y=y,col=Genus))+
  geom_point(size=1,alpha=0.7)+
  theme_classic()+
  xlab("PCo1 (27%)")+
  ylab("PCo2 (19%)")+
  scale_color_manual(values=cols)+
  stat_ellipse(level = 0.8)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))+
  theme(legend.position = "top",legend.text = element_text(size = 12))+
  theme(legend.title = element_blank())+
  theme(legend.key.height = unit(0.8,'cm'))+
  theme(legend.key.width = unit(0.8,'cm'))+
  annotate(geom = "text",x=0.4,y=0.4,label=expression("R"^2*"=0.16; P=0.214"))
pcoA_genus
#PcoA is non significant
###performing CAP analyses
library(vegan)
#read in the normalized count data
css<-read.delim("CSS.txt",sep = "\t",header = T)
head(css)



meta<-data.frame(genotypes=c("Col-0","sdi","msa","sdi","msa","msa","Col-0","sdi","Col-0","msa","sdi","Col-0"),fractions=rep("rhizo",12))
rownames(meta)<-colnames(css[,-1])
meta
sol_cap_COG<-vegan::capscale(vegdist(t(css[,-1]),method = "cao")~genotypes,data = meta)
summary(sol_cap_COG)
#Partitioning of squared Cao distance:
#Inertia Proportion
#Total         0.012912     1.0000
#Constrained   0.002807     0.2174
#Unconstrained 0.010105     0.7826
#anova(sol_cap_COG)

#Permutation test for capscale under reduced model
#Permutation: free
#Number of permutations: 999

#Model: vegan::capscale(formula = vegdist(t(css[, -1]), method = "cao") ~ genotypes, data = meta)
#Df  SumOfSqs      F Pr(>F)  
#Model     2 0.0028068 1.2499   0.05 *
#Residual  9 0.0101053                

cols<-c('#000000',"#008000","#DC143C")

library(ggplot2)
p1<-ggplot(as.data.frame(sol_cap_COG$CCA$wa),
aes(x=CAP1,y=CAP2,color=as.factor(meta$genotypes)))+
geom_point(aes(alpha=0.6),size=3.5)+
theme_classic()+
scale_color_manual(values=cols)+
  stat_ellipse(level = 0.8)+
  xlab("CAP1 (13.7%)")+
  ylab("CAP2 (8%)")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  theme(legend.position = "top",legend.text = element_text(size = 12))+
  theme(legend.title = element_blank())+
  theme(legend.key.height = unit(0.8,'cm'))+
  theme(legend.key.width = unit(0.8,'cm'))+
  annotate(geom = "text",x=-0.3,y=0.8,label=expression("PERMANOVA; F=1.25, p=0.05"))
p1
ggsave(
  "CAP_COG_functions.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 5,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()












































































