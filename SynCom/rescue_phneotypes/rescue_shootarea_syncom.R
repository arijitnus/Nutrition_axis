#SynCom growth rescue plot and script
#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
rescue<-read_excel("/Users/arijitmukherjee/Downloads/phenotypes_SynCom.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(rescue)
rescue$Medium<-as.factor(rescue$Medium)
rescue$Microbiome<-as.factor(rescue$Microbiome)
rescue$Experiment<-as.factor(rescue$Experiment)
rescue$label<-as.factor(rescue$label)
cols=c('#000000',"#DC143C")

level_order<-c("Heat killed","Active")
level_order2<-c("S1500","S0")

rescue$Medium<-factor(rescue$Medium,levels = level_order2)
range(rescue$`shoot area`)

rescue

sa<-ggplot(rescue,aes(x=factor(Microbiome,levels = level_order),y=`shoot area`))+
  geom_boxplot(aes(col=Medium))+
  geom_jitter(aes(col=Medium,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression('Shoot area in cm'^2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0.1, 0.6), expand = expansion(mult = c(0, 0)))
q<-sa+facet_grid(.~Medium)
q
ggsave(
  "rescue_syncom_shootarea_final.tiff",
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


shapiro.test(rescue$`shoot area`)#Non-Normal distribution

#check for homogeneity of variance
library(lawstat)
levene.test(rescue$`shoot area`,rescue$label)#homogeneous variance

#We can perform Mann-whitney U-test followed by a Dunn's test as indicated in 
#Shiji et al. Nature plants (2022)
library(dunn.test)
kruskal.test(`shoot area`~label,data = rescue)
stat_rescue_sa<-dunn.test(rescue$`shoot area`,rescue$label,wrap = TRUE, method = "bh")
head(stat_rescue_sa$P.adjusted)


#data: x and group
#Kruskal-Wallis chi-squared = 10.428, df = 3, p-value = 0.02


Kruskal-Wallis rank sum test

data: x and group
Kruskal-Wallis chi-squared = 10.428, df = 3, p-value = 0.02


Comparison of x by group                            
(Benjamini-Hochberg)                              
Col Mean-|
  Row Mean |   S0Active       S0HK   S1500Act
---------+---------------------------------
  S0HK |   3.134150
|    0.0052*
  |
  S1500Act |   0.845259  -2.254979
|     0.2388     0.0362
|
  S1500HK |   1.191831  -1.862758   0.355552
|     0.1750     0.0625     0.3611






