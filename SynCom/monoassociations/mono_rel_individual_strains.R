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
Dat$Phyla<-as.factor(Dat$Phyla)
cols<-c("#004C99","#99004C","#994C00")
levels<-c("Proteobacteria","Actinobacteria","Firmicutes")
Dat$Phyla<-factor(Dat$Phyla,levels = levels)
Dat

Dat$Strains<-reorder()

Dat
Dat$rel_rn
p1<-ggplot(Dat,aes(x=reorder(Strains,rel_lr),y=rel_lr))+
  geom_boxplot(aes(col=Phyla))+
  geom_jitter(aes(col=Phyla),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Relative increase in lateral roots")+
  theme(axis.text.x = element_text(size = 14,angle = 40,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(-0.5, 0.7), expand = expansion(mult = c(0, 0)))
q<-p1+geom_hline(yintercept = 0,linetype=2)
q
ggsave(
  "relative_lr_mono.tiff",
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





