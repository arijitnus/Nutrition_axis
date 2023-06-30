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
#diff         lwr         upr
#Heat killed:S15-Active:S15        -0.401240 -0.74822816 -0.05425184
#Active:S1500-Active:S15           -0.183385 -0.53037316  0.16360316
#Heat killed:S1500-Active:S15       0.085640 -0.26134816  0.43262816
#Active:S1500-Heat killed:S15       0.217855 -0.12913316  0.56484316
#Heat killed:S1500-Heat killed:S15  0.486880  0.13989184  0.83386816
#Heat killed:S1500-Active:S1500     0.269025 -0.07796316  0.61601316
                                        p adj
#Heat killed:S15-Active:S15        0.018160012
#Active:S1500-Active:S15           0.492504008
#Heat killed:S1500-Active:S15      0.909276958
#Active:S1500-Heat killed:S15      0.342395482
#Heat killed:S1500-Heat killed:S15 0.003112208
#Heat killed:S1500-Active:S1500    0.175946305

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












