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
rescue$`root length`
rescue$Medium<-factor(rescue$Medium,levels = level_order2)
range(rescue$`root length`)

rescue

rl<-ggplot(rescue,aes(x=factor(Microbiome,levels = level_order),y=`root length`))+
  geom_boxplot(aes(col=Medium))+
  geom_jitter(aes(col=Medium,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab('Primary root length (cm)')+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(5, 9), expand = expansion(mult = c(0, 0)))
q<-rl+facet_grid(.~Medium)
q
ggsave(
  "rescue_syncom_rootlength_final.tiff",
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


shapiro.test(rescue$`root length`)#Normal distribution

m<-aov(`root length`~Microbiome*Medium+Experiment,data = rescue)
m.tuk<-TukeyHSD(m)
m.tuk$`Microbiome:Medium`

#diff         lwr       upr      p adj
#Heat killed:S1500-Active:S1500   -0.05954702 -0.67507319 0.5559791 0.99405913
#Active:S0-Active:S1500            0.51249151 -0.08405432 1.1090373 0.11646695
#Heat killed:S0-Active:S1500       0.07046250 -0.53505444 0.6759794 0.98977808
#Active:S0-Heat killed:S1500       0.57203853 -0.03466454 1.1787416 0.07144755
#Heat killed:S0-Heat killed:S1500  0.13000952 -0.48551665 0.7455357 0.94379922
#Heat killed:S0-Active:S0         -0.44202901 -1.03857484 0.1545168 0.21507721






