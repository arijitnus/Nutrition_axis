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
rescue$`avg number of lat roots`
rescue$Medium<-factor(rescue$Medium,levels = level_order2)
range(rescue$`avg number of lat roots`)



rescue

lr<-ggplot(rescue,aes(x=factor(Microbiome,levels = level_order),y=`avg number of lat roots`))+
  geom_boxplot(aes(col=Medium))+
  geom_jitter(aes(col=Medium,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab('Average number of lateral roots per plant')+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(5, 16), expand = expansion(mult = c(0, 0)))
q<-lr+facet_grid(.~Medium)
q
ggsave(
  "rescue_syncom_lateralroots_final.tiff",
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


shapiro.test(rescue$`avg number of lat roots`)#Normal distribution

m<-aov(`avg number of lat roots`~Microbiome*Medium+Experiment,data = rescue)
m.tuk<-TukeyHSD(m)
m.tuk$`Microbiome:Medium`


#diff        lwr       upr      p adj
#Heat killed:S1500-Active:S1500    0.4905853 -1.3687640 2.3499346 0.89732581
#Active:S0-Active:S1500            1.6833100 -0.1187045 3.4853245 0.07530193
#Heat killed:S0-Active:S1500       0.1535725 -1.7057768 2.0129218 0.99627057
#Active:S0-Heat killed:S1500       1.1927247 -0.6399722 3.0254217 0.32179113
#Heat killed:S0-Heat killed:S1500 -0.3370128 -2.2261136 1.5520880 0.96489701
#Heat killed:S0-Active:S0         -1.5297375 -3.3624345 0.3029595 0.13309649






