#sulfate content of all genotypes

library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "sulfate",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")

head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)


#Sulfate
sulfur<-ggplot(data,aes(x=Genotype,y=`Sulphate (uM/mgDM)`))+
  geom_boxplot(aes(col=Genotype),lwd=0.8)+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot sulphate content (" * mu*"mol per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(10,55), expand = expansion(mult = c(0, 0)))
sulfur
saveRDS(sulfur,"sulfate_plot_final.rds")
ggsave(
  "sulfate_final.tiff",
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
#Fit anova model based on genotype and experiment
m_sulf<-aov(`Sulphate (uM/mgDM)`~Genotype+Experiment)
gr <- multcomp::cld(multcomp::glht(m_sulf,linfct = multcomp::mcp(Genotype = "Tukey")))
gr
#Col-0   msa1-3   sdi2-1  slim1-1 sultr1;1 sultr1;2 sultr2;1 
#"ab"      "a"     "ab"     "ab"     "ab"     "ab"     "ab" 
#sultr2;2 
#"b"

#phosphate

library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "Phosphate",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")

head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)

data$`Phosphate (uM/mg DM)`
#phosphate
phosphate<-ggplot(data,aes(x=Genotype,y=`Phosphate (uM/mg DM)`))+
  geom_boxplot(aes(col=Genotype),lwd=0.8)+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot phosphate content (" * mu*"mol per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(0,50), expand = expansion(mult = c(0, 0)))
phosphate
saveRDS(sulfur,"phosphate_plot_final.rds")
ggsave(
  "phosphate_final.tiff",
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

shapiro.test(data$`Phosphate (uM/mg DM)`)#non normal, hence apply KW test
#check for homogeneity of variance
library(lawstat)
levene.test(data$`Phosphate (uM/mg DM)`,data$Genotype)#homogeneous variance

#We can perform Mann-whitney U-test followed by a Dunn's test as indicated in 
#Shiji et al. Nature plants (2022)
library(dunn.test)
kruskal.test(data$`Phosphate (uM/mg DM)`,data$Genotype)
stat_phosphate<-dunn.test(data$`Phosphate (uM/mg DM)`,data$Genotype,wrap = TRUE, method = "bh")
head(stat_phosphate$P.adjusted)


#compact letter display 
library(rcompanion)
dt <- dunn.test(data$`Phosphate (uM/mg DM)`,data$Genotype, wrap=TRUE, method="bh")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#None is significant


#Nitrate

library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "Nitrate",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")

head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)

data$`Nitrate (uM/mg DM)`
#phosphate
nitrate<-ggplot(data,aes(x=Genotype,y=`Nitrate (uM/mg DM)`))+
  geom_boxplot(aes(col=Genotype),lwd=0.8)+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot nitrate content (" * mu*"mol per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(0,220), expand = expansion(mult = c(0, 0)))
nitrate

ggsave(
  "nitrate_final.tiff",
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

shapiro.test(data$`Nitrate (uM/mg DM)`)#non normal, hence apply KW test
#check for homogeneity of variance
m_nitrate<-aov(`Nitrate (uM/mg DM)`~Genotype+Experiment,data = data)
gr <- multcomp::cld(multcomp::glht(m_nitrate,linfct = multcomp::mcp(Genotype = "Tukey")))
gr



library(lawstat)
levene.test(data$`Nitrate (uM/mg DM)`,data$Genotype)#homogeneous variance

#We can perform Mann-whitney U-test followed by a Dunn's test as indicated in 
#Shiji et al. Nature plants (2022)
library(dunn.test)
kruskal.test(data$`Nitrate (uM/mg DM)`,data$Genotype)
stat_nitrate<-dunn.test(data$`Nitrate (uM/mg DM)`,data$Genotype,wrap = TRUE, method = "bh")
head(stat_nitrate$P.adjusted)


#compact letter display 
library(rcompanion)
dt <- dunn.test(data$`Nitrate (uM/mg DM)`,data$Genotype, wrap=TRUE, method="bh")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#None is significant

#Plot for shoot biomass

library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "biomass",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")

head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)
data$Leaf
data
#biomass
shoot<-ggplot(data,aes(x=Genotype,y=Leaf))+
  geom_boxplot(aes(col=Genotype),lwd=0.8)+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot biomass (g)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-shoot+scale_y_continuous(limits = c(0, 3), expand = expansion(mult = c(0, 0)))
q
ggsave(
  "shoot_biomass_final.tiff",
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

shapiro.test(data$Leaf)
m_biomass<-aov(Leaf~Genotype+Experiment,data = data)
m_biomass.tuk<-TukeyHSD(m_biomass)
m_biomass.tuk$Genotype
gr <- multcomp::cld(multcomp::glht(m_biomass,linfct = multcomp::mcp(Genotype = "Tukey")))
gr#all NS
biomass<-emmeans::emmeans(m_biomass,"Genotype")
cld(biomass, Letter="abcdefg")



#Calculate effect size based on sulfate, phosphate, nitrate and biomass
### calculate the effect size of BFO on shoot fresh weight under NC, LP, and EODFR
library(effsize)
#sulfur
Col <- subset(data,Genotype=="Col-0")
msa <- subset(data,Genotype=="msa1-3")
sdi <- subset(data,Genotype=="sdi2-1")
slim11 <- subset(data,Genotype=="slim1-1")
sultr11<- subset(data,Genotype=="sultr1;1")
sultr12<- subset(data,Genotype=="sultr1;2")
sultr21 <- subset(data,Genotype=="sultr2;1")
sultr22 <- subset(data,Genotype=="sultr2;2")


msa_eff <- cohen.d(msa$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
msa_eff#large
sdi_eff <- cohen.d(sdi$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
sdi_eff#medium
slim11_eff<-cohen.d(slim11$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
slim11_eff#small
sultr11_eff<-cohen.d(sultr11$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
sultr11_eff#negligible
sultr12_eff<-cohen.d(sultr12$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
sultr12_eff#negligible
sultr21_eff<-cohen.d(sultr21$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
sultr21_eff#large
sultr22_eff<-cohen.d(sultr22$`Sulphate (uM/mgDM)`,Col$`Sulphate (uM/mgDM)`)
sultr22_eff#medium


sulfate_eff <- c(msa_eff$estimate,sdi_eff$estimate,slim11_eff$estimate,
                 sultr11_eff$estimate,sultr12_eff$estimate,sultr21_eff$estimate,sultr22_eff$estimate)
sulfate_eff <- as.data.frame(sulfate_eff)
sulfate_eff
rownames(sulfate_eff) <- c("msa1-3","sdi2-1","slim1-1","sultr1;1","sultr1;2","sultr2;1","sultr2;2")
write.table(sulfate_eff, "sulfate_effsize.txt", sep="\t", quote=FALSE) 

sulfate_eff

#Calculate biomass effect size in the same manner


library(effsize)
#biomass
Col <- subset(data,Genotype=="Col-0")
msa <- subset(data,Genotype=="msa1-3")
sdi <- subset(data,Genotype=="sdi2-1")
slim11 <- subset(data,Genotype=="slim1-1")
sultr11<- subset(data,Genotype=="sultr1;1")
sultr12<- subset(data,Genotype=="sultr1;2")
sultr21 <- subset(data,Genotype=="sultr2;1")
sultr22 <- subset(data,Genotype=="sultr2;2")


msa_eff <- cohen.d(msa$Leaf,Col$Leaf)
msa_eff
sdi_eff <- cohen.d(sdi$Leaf,Col$Leaf)
sdi_eff
slim11_eff<-cohen.d(slim11$Leaf,Col$Leaf)
slim11_eff
sultr11_eff<-cohen.d(sultr11$Leaf,Col$Leaf)
sultr11_eff
sultr12_eff<-cohen.d(sultr12$Leaf,Col$Leaf)
sultr12_eff
sultr21_eff<-cohen.d(sultr21$Leaf,Col$Leaf)
sultr21_eff
sultr22_eff<-cohen.d(sultr22$Leaf,Col$Leaf)
sultr22_eff
class(msa_eff$estimate)

biomass_eff <- c(msa_eff$estimate,sdi_eff$estimate,slim11_eff$estimate,
                 sultr11_eff$estimate,sultr12_eff$estimate,sultr21_eff$estimate,sultr22_eff$estimate)
biomass_eff <- as.data.frame(biomass_eff)
biomass_eff
rownames(biomass_eff) <- c("msa1-3","sdi2-1","slim1-1","sultr1;1","sultr1;2","sultr2;1","sultr2;2")
write.table(biomass_eff, "biomass_effsize.txt", sep="\t", quote=FALSE) 

biomass_eff

#similarly calculate for nitrate and phosphate

#nitrate
Col <- subset(data,Genotype=="Col-0")
msa <- subset(data,Genotype=="msa1-3")
sdi <- subset(data,Genotype=="sdi2-1")
slim11 <- subset(data,Genotype=="slim1-1")
sultr11<- subset(data,Genotype=="sultr1;1")
sultr12<- subset(data,Genotype=="sultr1;2")
sultr21 <- subset(data,Genotype=="sultr2;1")
sultr22 <- subset(data,Genotype=="sultr2;2")


msa_eff <- cohen.d(msa$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
msa_eff
sdi_eff <- cohen.d(sdi$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
sdi_eff
slim11_eff<-cohen.d(slim11$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
slim11_eff
sultr11_eff<-cohen.d(sultr11$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
sultr11_eff
sultr12_eff<-cohen.d(sultr12$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
sultr12_eff
sultr21_eff<-cohen.d(sultr21$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
sultr21_eff
sultr22_eff<-cohen.d(sultr22$`Nitrate (uM/mg DM)`,Col$`Nitrate (uM/mg DM)`)
sultr22_eff
class(msa_eff$estimate)

nitrate_eff <- c(msa_eff$estimate,sdi_eff$estimate,slim11_eff$estimate,
                 sultr11_eff$estimate,sultr12_eff$estimate,sultr21_eff$estimate,sultr22_eff$estimate)
nitrate_eff <- as.data.frame(nitrate_eff)
nitrate_eff
rownames(nitrate_eff) <- c("msa1-3","sdi2-1","slim1-1","sultr1;1","sultr1;2","sultr2;1","sultr2;2")
write.table(nitrate_eff, "nitrate_effsize.txt", sep="\t", quote=FALSE) 

nitrate_eff


#phosphate===========
#====================
#===================

Col <- subset(data,Genotype=="Col-0")
msa <- subset(data,Genotype=="msa1-3")
sdi <- subset(data,Genotype=="sdi2-1")
slim11 <- subset(data,Genotype=="slim1-1")
sultr11<- subset(data,Genotype=="sultr1;1")
sultr12<- subset(data,Genotype=="sultr1;2")
sultr21 <- subset(data,Genotype=="sultr2;1")
sultr22 <- subset(data,Genotype=="sultr2;2")


msa_eff <- cohen.d(msa$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
msa_eff
sdi_eff <- cohen.d(sdi$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
sdi_eff
slim11_eff<-cohen.d(slim11$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
slim11_eff
sultr11_eff<-cohen.d(sultr11$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
sultr11_eff
sultr12_eff<-cohen.d(sultr12$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
sultr12_eff
sultr21_eff<-cohen.d(sultr21$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
sultr21_eff
sultr22_eff<-cohen.d(sultr22$`Phosphate (uM/mg DM)`,Col$`Phosphate (uM/mg DM)`)
sultr22_eff


phosphate_eff <- c(msa_eff$estimate,sdi_eff$estimate,slim11_eff$estimate,
                 sultr11_eff$estimate,sultr12_eff$estimate,sultr21_eff$estimate,sultr22_eff$estimate)
phosphate_eff <- as.data.frame(phosphate_eff)
phosphate_eff
rownames(phosphate_eff) <- c("msa1-3","sdi2-1","slim1-1","sultr1;1","sultr1;2","sultr2;1","sultr2;2")
write.table(phosphate_eff, "phosphate_effsize.txt", sep="\t", quote=FALSE) 

###Plot the effect sizes of all anions and shoot biomass for all the genotypes
library(ggplot2)
library(dplyr)

sul<-read.table("sulfate_effsize.txt",sep = "\t",header = T)
phos<-read.table("phosphate_effsize.txt",sep = "\t",header = T)
nit<-read.table("nitrate_effsize.txt",sep = "\t",header = T)
bm<-read.table("biomass_effsize.txt",sep = "\t",header = T)
df<-cbind(sul,phos,nit,bm)
df$Genotype<-rownames(df)
df$Genotype<-as.factor(df$Genotype)
cols <- c("#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")
df
#melt the data into long data format
library(tidyr)
#pivot the data frame into a long format
df2<-df %>% pivot_longer(cols=c('sulfate_eff', 'phosphate_eff','nitrate_eff','biomass_eff'),
                    names_to='vars',
                    values_to='effsize')

df2$vars<-as.factor(df2$vars)
df2$vars
df2<-df2%>%filter(vars!="phosphate_eff")

effsize<-ggplot(df2,aes(x=Genotype,y=effsize,group=vars))+
  geom_point(aes(col=vars,size=2.5,alpha=0.7))+
  geom_line(aes(col=vars,alpha=0.7),linewidth=1.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Cohen's effect size")+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-effsize+scale_y_continuous(limits = c(-1, 1), expand = expansion(mult = c(0, 0)))
q+geom_hline(yintercept = c(-0.5,0.5),linetype="dashed")
ggsave(
  "effsize_final.tiff",
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




