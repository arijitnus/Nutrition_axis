#plotting the results of root length SPAF18 in Arabidopsis Exp1
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/Root_length_Arabidopsis1.xlsx",sheet = "root_length",col_names = T,skip = 0)
head(dat)
dat$Sulphur<-as.factor(dat$Sulphur)
dat$Microbiome<-as.factor(dat$Microbiome)

head(dat)
shapiro.test(dat$`Average length (cm)`)#normally distributed
m<-aov(`Average length (cm)`~Microbiome*Sulphur,data = dat)
m.tuk<-TukeyHSD(m)
m.tuk$`Microbiome:Sulphur`
#write down all the results of comparisons
write.table(m.tuk$`Microbiome:Sulphur`,"tukey_root_length.tsv",sep = "\t")
#diff         lwr        upr
#Heat killed:Deficient-Active:Deficient       -0.47592 -0.98969906 0.03785906
#Active:Sufficient-Active:Deficient           -0.23088 -0.74465906 0.28289906
#Heat killed:Sufficient-Active:Deficient       0.12484 -0.38893906 0.63861906
#Active:Sufficient-Heat killed:Deficient       0.24504 -0.26873906 0.75881906
#Heat killed:Sufficient-Heat killed:Deficient  0.60076  0.08698094 1.11453906
#Heat killed:Sufficient-Active:Sufficient      0.35572 -0.15805906 0.86949906
#p adj
#Heat killed:Deficient-Active:Deficient       0.07445553
#Active:Sufficient-Active:Deficient           0.58451867
#Heat killed:Sufficient-Active:Deficient      0.89743202
#Active:Sufficient-Heat killed:Deficient      0.53788287
#Heat killed:Sufficient-Heat killed:Deficient 0.01930009
#Heat killed:Sufficient-Active:Sufficient     0.23578662

#P-adj for syncom deficient active vs heat killed is 0.07


#plot the boxplot for the comparisons

cols=c("#DC143C",'#000000')
head(dat)
level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")
dat$Sulphur<-factor(dat$Sulphur,levels = level_order2)
dat$`median length (cm)`

cs<-ggplot(dat,aes(x=factor(Microbiome,levels=level_order),y=`median length (cm)`))+
  geom_boxplot(aes(col=Microbiome),lwd=0.8)+
  geom_jitter(aes(col=Microbiome),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Primary root length (cm)")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(1, 3), expand = expansion(mult = c(0, 0)))
q<-cs+facet_grid(.~Sulphur)
q
ggsave(
  "Root_length.tiff",
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


#stats
m<-aov(`root length (cm)`~microbiome*Sulfur+Experiment,data = dat)#Normally distributed
m.tuk<-TukeyHSD(m)
m.tuk$`microbiome:Sulfur`

diff         lwr         upr       p adj
Heat killed:S15-Active:S15        -0.401240 -0.75565222 -0.04682778 0.021314127
Active:S1500-Active:S15           -0.183385 -0.53779722  0.17102722 0.511458582
Heat killed:S1500-Active:S15       0.085640 -0.26877222  0.44005222 0.914583316
Active:S1500-Heat killed:S15       0.217855 -0.13655722  0.57226722 0.361566534
Heat killed:S1500-Heat killed:S15  0.486880  0.13246778  0.84129222 0.003827143
Heat killed:S1500-Active:S1500     0.269025 -0.08538722  0.62343722 0.191062900









