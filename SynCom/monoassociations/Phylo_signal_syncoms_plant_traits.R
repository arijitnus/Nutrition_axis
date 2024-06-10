library(picante)
library(ape)
library(adephylo)
library(ade4)
library(phylobase)
library(geiger)
library(phytools)


library(readxl)
library(dplyr)
library(ggplot2)

Dat<-read_excel("/Users/arijitmukherjee/Downloads/hm_all_pheno.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
Dat

Dat_bm<-Dat%>%group_by(Strains)%>%summarise(avg_bm=mean(rel_bm))
Dat_bm
Dat_sa<-Dat%>%group_by(Strains)%>%summarise(avg_sa=mean(rel_sa))
Dat_rl<-Dat%>%group_by(Strains)%>%summarise(avg_rl=mean(rel_rl))
Dat_rn<-Dat%>%group_by(Strains)%>%summarise(avg_rn=mean(rel_rn))
Dat_lr<-Dat%>%group_by(Strains)%>%summarise(avg_lr=mean(rel_lr))
df<-cbind(Dat_bm,Dat_sa,Dat_rl,Dat_rn,Dat_lr)
df<-df[,-c(3,5,7,9)]
df

rownames(df)<-df$Strains
df<-df[,-1]

df

tree<-read.tree("SPAF18.rooted.tree")

set.seed(1231)
tree$tip.label
rownames(df)<-c("10B2","1A1","3C1","s3c2","4C","6A1","6A2",
                "'7F2-1'","'8A-2'","8E1","8E4","9B1","9B2","9F3",
                "9E2","P31D","P32B1","P33G")

rownames(df)
#Reorder the dataframe based on the tip labels of the tree
df_ord<-df[match(tree$tip.label,rownames(df)),]
df_ord
#biomass lambda
res_bm<-phylosig(tree,df$avg_bm, method="lambda", test=TRUE, nsim=999)
res_bm
#Phylogenetic signal lambda : 7.33137e-05 
#logL(lambda) : 8.96671 
#LR(lambda=0) : -0.000894062 
#P-value (based on LR test) : 1 

res_sa<-phylosig(tree,df$avg_sa, method="lambda", test=TRUE, nsim=999)
res_sa
#Phylogenetic signal lambda : 7.33137e-05 
#logL(lambda) : 7.01161 
#LR(lambda=0) : -0.000768907 
#P-value (based on LR test) : 1 

res_rl<-phylosig(tree,df$avg_rl, method="lambda", test=TRUE, nsim=999)
res_rl
#Phylogenetic signal lambda : 7.33137e-05 
#logL(lambda) : 20.7209 
#LR(lambda=0) : -0.000455995 
#P-value (based on LR test) : 1


res_rn<-phylosig(tree,df$avg_rn, method="lambda", test=TRUE, nsim=999)
res_rn
#Phylogenetic signal lambda : 7.33137e-05 
#logL(lambda) : 12.0405 
#LR(lambda=0) : -0.000562924 
#P-value (based on LR test) : 1 

res_lr<-phylosig(tree,df$avg_lr, method="lambda", test=TRUE, nsim=999)
res_lr
#Phylogenetic signal lambda : 7.33137e-05 
#logL(lambda) : 4.41434 
#LR(lambda=0) : -0.000703684 
#P-value (based on LR test) : 1 






































