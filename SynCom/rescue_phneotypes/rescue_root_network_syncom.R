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
rescue$`root network`
rescue$Medium<-factor(rescue$Medium,levels = level_order2)
range(rescue$`root network`)



rescue

rn<-ggplot(rescue,aes(x=factor(Microbiome,levels = level_order),y=`root network`))+
  geom_boxplot(aes(col=Medium))+
  geom_jitter(aes(col=Medium,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression('Average root network per plant in cm'^2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0.08, 0.18), expand = expansion(mult = c(0, 0)))
q<-rn+facet_grid(.~Medium)
q
ggsave(
  "rescue_syncom_rootnetwork_final.tiff",
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


shapiro.test(rescue$`root network`)#Normal distribution

m<-aov(`root network`~Microbiome*Medium+Experiment,data = rescue)
m.tuk<-TukeyHSD(m)
m.tuk$`Microbiome:Medium`

#diff          lwr         upr     p adj
#Heat killed:S1500-Active:S1500    0.0093365175 -0.006143997 0.024817032 0.3889598
#Active:S0-Active:S1500            0.0099193096 -0.005083849 0.024922468 0.3081390
#Heat killed:S0-Active:S1500       0.0036506500 -0.011829865 0.019131165 0.9239258
#Active:S0-Heat killed:S1500       0.0005827921 -0.014675821 0.015841405 0.9996257
#Heat killed:S0-Heat killed:S1500 -0.0056858676 -0.021414086 0.010042351 0.7743282
#Heat killed:S0-Active:S0         -0.0062686597 -0.021527273 0.008989954 0.6986833






