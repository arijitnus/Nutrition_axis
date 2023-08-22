#Testing for significant differences in relative growth promotions compared to 
#SPAF18
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/mono_data.xlsx",sheet = "Sheet2",col_names = T,skip = 0)
head(dat)
dat$Microbiome<-as.factor(dat$Microbiome)
dat$Strain<-as.factor(dat$Strain)
Res<-NULL
mstrains<-unique(dat$Strain)
mstrains

for (st in mstrains) {
  mdf<-dat%>%filter(Strain==st)
  hkavgbm<-mean(mdf[mdf$Microbiome=="Heat killed",]$biomass)
  hkavgsa<-mean(mdf[mdf$Microbiome=="Heat killed",]$shoot_area)
  hkavgrl<-mean(mdf[mdf$Microbiome=="Heat killed",]$root_length)
  mdf$rbm<-(mdf$biomass-hkavgbm)/hkavgbm
  mdf$rsa<-(mdf$shoot_area-hkavgsa)/hkavgsa
  mdf$rrl<-(mdf$root_length-hkavgrl)/hkavgrl
  temp <- data.frame(Strains = st,rel_bm=mdf$rbm,
                     rel_sa=mdf$rsa,rel_rl=mdf$rrl,Microbiome=mdf$Microbiome
                     )
  Res<-rbind(Res,temp)
}

Res_filt<-Res%>%filter(Microbiome!="Heat killed")
Res_filt

write.table(Res_filt,"rel_increase.tsv",sep = "\t")


#Now from this relative increase, we need to perform statistical test for 
#significant differences from SPAF18 for each of the strains for biomass
Res_filt<-read_excel("/Users/arijitmukherjee/Downloads/combined_rel_increase.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
Res_filt
Res<-NULL
mstrains <- Res_filt$Strains%>%unique()
mstrains<-mstrains[-19]
Res_filt$Strains
mstrains
for (st in mstrains) {
  mdf<-subset(Res_filt,Strains%in%c(st,"SPAF18"))
  mdf$Strains<-as.factor(mdf$Strains)
  x<-subset(mdf,Strains==st)$total_rel
  y<-subset(mdf,Strains=='SPAF18')$total_rel
  m1 <- car::leveneTest(total_rel~Strains,mdf)
  pvalue <- m1$`Pr(>F)`[1]
  if(pvalue < 0.05){
    m1 <- t.test(x,y,exact = F,var.equal = F)
    m_w <- wilcox.test(x,y,exact = F,var.equal = F)
  }else{
    m1 <- t.test(x,y,exact = F,var.equal = T)
    m_w <- wilcox.test(x,y,exact = F,var.equal = T)
  }
  pval <- m1$p.value
  temp <- data.frame(Strains = st,lavene = pvalue,
                     p.value = pval,
                     p.valueW = m_w$p.value
  )
  Res <- rbind(Res,temp)
}
Res


# all of the levene test variance values are >0.05, so keep p-values from 
#normal t-test only
Res$padj<-p.adjust(Res$p.value,method = "fdr")
Res$padjW<-p.adjust(Res$p.valueW,method = "fdr")
Res
write.table(Res,"perc_rel_changes_stats.tsv",sep = "\t")





































