#SynCom growth rescue plot and script
#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
rescue<-read_excel("/Users/arijitmukherjee/Downloads/SynCom_rescue.xlsx",sheet = "rescue",col_names = T,skip = 0)
head(rescue)
rescue$sulfur<-as.factor(rescue$sulfur)
rescue$microbiome<-as.factor(rescue$microbiome)
rescue$label<-as.factor(rescue$label)
cols=c('#000000',"#DC143C")

level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")

rescue$sulfur<-factor(rescue$sulfur,levels = level_order2)
rescue$`biomass (mg)`


res<-ggplot(rescue,aes(x=factor(microbiome,levels=level_order),y=`biomass (mg)`))+
  geom_boxplot(aes(col=microbiome))+
  geom_jitter(aes(col=microbiome),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Biomass in mg")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(0, 22), expand = expansion(mult = c(0, 0)))
q<-res+facet_grid(.~sulfur)
q
ggsave(
  "rescue_syncom_final.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 400,
)

shapiro.test(rescue$`biomass (mg)`)

#Need to perform later , it is complicated in case of non parametric anova
qqnorm(rescue$`biomass (mg)`)
qqline(rescue$`biomass (mg)`)


#Shapiro test says not nortmal , need to perform alternative tests
shapiro.test(rescue$`biomass (mg)`)
#check for homogeneity of variance
library(lawstat)
levene.test(rescue$`biomass with tube (mg)`,rescue$label)#homogeneous variance

#We can perform Mann-whitney U-test followed by a Dunn's test as indicated in 
#Shiji et al. Nature plants (2022)
library(dunn.test)
kruskal.test(`biomass (mg)`~label,data = rescue)
stat_rescue_biomass<-dunn.test(rescue$`biomass (mg)`,rescue$label,wrap = TRUE, method = "bh")
head(stat_rescue_biomass$P.adjusted)


#compact letter display 
library(rcompanion)
dt <- dunn.test(rescue$`biomass (mg)`,rescue$label, wrap=TRUE, method="bh")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

#Group Letter MonoLetter
#1   D+A      b          b
#2   D+H      a         a 
#3   S+A      b          b
#4   S+H      b          b







