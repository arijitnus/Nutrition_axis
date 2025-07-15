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

Dat<-read_excel("/Users/arijitmukherjee/Downloads/relative_increase_chems.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
Dat$Treatment<-as.factor(Dat$Treatment)
Dat$Conc<-as.factor(Dat$Conc)
Dat$Relative_increase
Dat$Conc2<-as.factor(Dat$Conc2)

p1<-ggplot(Dat,aes(x=tot,y=Relative_increase))+
  geom_boxplot()+
  geom_jitter(size=2.5,width = 0.4, alpha=0.5)+
  theme_classic()+
  xlab("")+
  ylab("Relative increase in biomass")+
  theme(axis.text.x = element_text(size = 14,angle = 40,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(-0.5, 0.6), expand = expansion(mult = c(0, 0)))
q<-p1+geom_hline(yintercept = 0,linetype=2)
q
ggsave(
  "relative_chem_supp.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 11,
  height = 5,
  units = "in",
  dpi = 400,
)
dev.off()



