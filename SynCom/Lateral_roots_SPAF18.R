#SynCom growth rescue plot and script
#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
rescue<-read_excel("/Users/arijitmukherjee/Downloads/LR_plot.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(rescue)
rescue$Sulfur<-as.factor(rescue$Sulfur)
rescue$Microbiome<-as.factor(rescue$Microbiome)
rescue$label<-as.factor(rescue$label)
cols=c('#000000',"#DC143C")

level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")

rescue$Sulfur<-factor(rescue$Sulfur,levels = level_order2)
rescue$`biomass (mg)`
rescue$Exp<-as.factor(rescue$Exp)

res<-ggplot(rescue,aes(x=factor(Microbiome,levels=level_order),y=LR))+
  geom_boxplot(aes(col=Sulfur))+
  geom_jitter(aes(col=Sulfur,shape=Exp),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Average number of lateral roots")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 6.5), expand = expansion(mult = c(0, 0)))
q<-res+facet_grid(.~Sulfur)
q
ggsave(
  "LR_SPAF18.tiff",
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
shapiro.test(rescue$LR)#normaL dist.

#check for homogeneity of variance
library(lawstat)
levene.test(rescue$`biomass with tube (mg)`,rescue$label)#homogeneous variance

#We can perform Mann-whitney U-test followed by a Dunn's test as indicated in 
#Shiji et al. Nature plants (2022)
m<-aov(LR~Microbiome*Sulfur+Exp,data = rescue)
m.tuk<-TukeyHSD(m)
m.tuk$`Microbiome:Sulfur`
#write down all the results of comparisons
write.table(m.tuk$`Microbiome:Sulfur`,"tukey_LR.tsv",sep = "\t")

#p adj
#Heat killed:Sufficient-Active:Sufficient     9.917253e-01
#Active:Deficient-Active:Sufficient           5.708786e-01
#Heat killed:Deficient-Active:Sufficient      3.582433e-03
##Active:Deficient-Heat killed:Sufficient      4.012681e-01
#Heat killed:Deficient-Heat killed:Sufficient 7.758694e-03
#Heat killed:Deficient-Active:Deficient       8.343183e-05






