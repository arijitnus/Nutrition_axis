#Data analyses for monoassociations of individual strains and their effect on plant 
#phenotypes 
libs<-c("ggplot2","dplyr","readxl","tidyr")
lapply(libs, require, character.only = TRUE)
Dat<-read_excel("/Users/arijitmukherjee/Downloads/monoassociation_final.xlsx",sheet = "Sheet1",
                col_names = T,skip = 0)
head(Dat)

Dat$Microbiome<-as.factor(Dat$Microbiome)
Dat$Strain<-as.factor(Dat$Strain)

View(Dat)
hk_mean <- Dat %>% subset(Microbiome == "Heat killed") %$% biomass %>% mean
Res <- NULL
mstrains <- Dat$Strain %>% unique 
mstrains
for (st in mstrains) {
  mdf<-Dat%>%filter(Strain==st)
  x<-subset(mdf,Microbiome== "Heat killed")$biomass
  y<-subset(mdf,Microbiome=="Active")$biomass
  m1 <- car::leveneTest(biomass ~ Microbiome,mdf)#perform levene test within that df
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
Res# all of the levene test variance values are >0.05, so keep p-values from 
#normal t-test only
Res$padj<-p.adjust(Res$p.value,method = "fdr")
Res
write.table(Res,"final_total_mono_testresults.tsv",sep = "\t",row.names = F)

#####======================##-======
#-----==================----======Biomass above
#================================Now do root length

Dat<-read_excel("/Users/arijitmukherjee/Downloads/monoassociation_final.xlsx",sheet = "Sheet1",
                col_names = T,skip = 0)
head(Dat)

Dat$Microbiome<-as.factor(Dat$Microbiome)
Dat$Strain<-as.factor(Dat$Strain)

View(Dat)
hk_mean <- Dat %>% subset(Microbiome == "Heat killed") %$% biomass %>% mean
Res <- NULL
mstrains <- Dat$Strain %>% unique 
mstrains
for (st in mstrains) {
  mdf<-Dat%>%filter(Strain==st)
  x<-subset(mdf,Microbiome== "Heat killed")$root_length
  y<-subset(mdf,Microbiome=="Active")$root_length
  m1 <- car::leveneTest(root_length ~ Microbiome,mdf)#perform levene test within that df
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
Res# all of the levene test variance values are >0.05, so keep p-values from 
#normal t-test only
Res$padj<-p.adjust(Res$p.value,method = "fdr")
Res$padjW<-p.adjust(Res$p.valueW,method = "fdr")
Res
write.table(Res,"final_total_mono_testresults_rootlength.tsv",sep = "\t",row.names = F)




#####======================##-======
#-----==================----======Biomass above
#================================Now do shoot area

Dat<-read_excel("/Users/arijitmukherjee/Downloads/monoassociation_final.xlsx",sheet = "Sheet1",
                col_names = T,skip = 0)
head(Dat)

Dat$Microbiome<-as.factor(Dat$Microbiome)
Dat$Strain<-as.factor(Dat$Strain)

View(Dat)
hk_mean <- Dat %>% subset(Microbiome == "Heat killed") %$% biomass %>% mean
Res <- NULL
mstrains <- Dat$Strain %>% unique 
mstrains
for (st in mstrains) {
  mdf<-Dat%>%filter(Strain==st)
  x<-subset(mdf,Microbiome== "Heat killed")$shoot_area
  y<-subset(mdf,Microbiome=="Active")$shoot_area
  m1 <- car::leveneTest(shoot_area ~ Microbiome,mdf)#perform levene test within that df
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
Res# all of the levene test variance values are >0.05, so keep p-values from 
#normal t-test only
Res$padj<-p.adjust(Res$p.value,method = "fdr")
Res$padjW<-p.adjust(Res$p.valueW,method = "fdr")
Res
write.table(Res,"final_total_mono_testresults_shootarea.tsv",sep = "\t",row.names = F)

















































