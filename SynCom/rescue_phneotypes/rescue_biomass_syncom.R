#SynCom growth rescue plot and script
#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
rescue<-read_excel("/Users/arijitmukherjee/Downloads/Source_data_Fig.3.xlsx",sheet = "Sheet2",col_names = T,skip = 0)
head(rescue)
rescue$Sulfur<-as.factor(rescue$Sulfur)
rescue$Microbiome<-as.factor(rescue$Microbiome)
rescue$Experiment<-as.factor(rescue$Experiment)
rescue$label<-as.factor(rescue$label)
cols=c('#000000',"#DC143C")

level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")

rescue$Sulfur<-factor(rescue$Sulfur,levels = level_order2)


bm<-ggplot(rescue,aes(x=factor(Microbiome,levels = level_order),y=avg_biomass))+
  geom_boxplot(aes(col=Sulfur))+
  geom_jitter(aes(col=Sulfur,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Average biomass of individual plant (mg)")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(3, 13), expand = expansion(mult = c(0, 0)))
q<-bm+facet_grid(.~Sulfur)
q
ggsave(
  "rescue_syncom_biomass_final.tiff",
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


shapiro.test(rescue$avg_biomass)#Normal distribution
m<-aov(avg_biomass~Microbiome*Sulfur+Experiment,data = rescue)
m.tuk<-TukeyHSD(m)
m.tuk$`Microbiome:Sulfur`

#> m.tuk$`Microbiome:Sulfur`
#diff        lwr         upr        p adj
#Heat killed:Deficient-Active:Deficient       -2.5398829 -3.5430806 -1.53668509 5.625372e-08
#Active:Sufficient-Active:Deficient           -0.4547309 -1.4260729  0.51661120 6.053644e-01
#Heat killed:Sufficient-Active:Deficient      -1.0922990 -2.0954968 -0.08910121 2.770924e-02
#Active:Sufficient-Heat killed:Deficient       2.0851520  1.0819542  3.08834977 5.306293e-06
#Heat killed:Sufficient-Heat killed:Deficient  1.4475839  0.4135113  2.48165647 2.619501e-03
#Heat killed:Sufficient-Active:Sufficient     -0.6375681 -1.6407659  0.36562966 3.428540e-01






