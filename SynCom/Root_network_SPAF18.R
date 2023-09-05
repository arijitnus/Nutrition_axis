#plotting the results of root length SPAF18 in Arabidopsis Exp1
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/Root_area.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(dat)
dat$Sulfur<-as.factor(dat$Sulphur)
dat$microbiome<-as.factor(dat$Microbiome)
dat$Experiment<-as.factor(dat$Experiment)
head(dat)
shapiro.test(dat$`root area`)#normally distributed
m<-aov(`root area`~microbiome*Sulfur,data = dat)
m.tuk<-TukeyHSD(m)
m.tuk$`microbiome:Sulfur`
#write down all the results of comparisons
write.table(m.tuk$`Microbiome:Sulphur`,"tukey_root_area.tsv",sep = "\t")

#p adj
#Heat killed:Deficient-Active:Deficient       0.08797090
#Active:Sufficient-Active:Deficient           0.36155819
#Heat killed:Sufficient-Active:Deficient      0.96560893
#Active:Sufficient-Heat killed:Deficient      0.86213672
#Heat killed:Sufficient-Heat killed:Deficient 0.03033631
#Heat killed:Sufficient-Active:Sufficient     0.16509141
cols=c('#000000',"#DC143C")
head(dat)
level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")
dat$Sulfur<-factor(dat$Sulfur,levels = level_order2)
dat$microbiome<-as.factor(dat$microbiome)
dat$`root area`
dat$Experiment<-as.factor(dat$Experiment)

ra<-ggplot(dat,aes(x=factor(microbiome,levels = level_order),y=`root area`))+
  geom_boxplot(aes(col=Sulfur))+
  geom_jitter(aes(col=Sulfur,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Root network in cm"^2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 0.03), expand = expansion(mult = c(0, 0)))
q<-ra+facet_grid(.~Sulfur)
q
ggsave(
  "root_network_combined.tiff",
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





