#SynCom growth rescue plot and script
#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
rescue<-read_excel("/Users/arijitmukherjee/Downloads/plot.xlsx",sheet = "rel_increase",col_names = T,skip = 0)
head(rescue)

rescue$Sulfur<-as.factor(rescue$Sulfur)
rescue$Treatment<-as.factor(rescue$Treatment)
#rescue$label<-as.factor(rescue$label)
cols=c('#000000',"#DC143C")

level_order<-c("Active_SPAF18","cell_free_extract")
level_order2<-c("Sufficient","Deficient")

rescue$Sulfur<-factor(rescue$Sulfur,levels = level_order2)
rescue$Treatment<-factor(rescue$Treatment,levels = level_order)
rescue$avg_biomass
rescue
rescue$Experiment<-as.factor(rescue$Experiment)
rescue

rescue$Treatment

cs<-ggplot(rescue,aes(x=factor(Sulfur,levels=level_order2),y=Relative_increase))+
  geom_boxplot(aes(col=Treatment),lwd=0.8)+
  geom_jitter(aes(col=Treatment,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Relative increase in biomass")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(-0.4, 0.8), expand = expansion(mult = c(0, 0)))
q<-cs+geom_hline(yintercept = 0,linetype=2)+facet_grid(.~Treatment)
q
rescue$Relative_increase

shapiro.test(rescue$Relative_increase)#normal distribution
#post hoc tukey test

#perform the tukey test
m<-aov(Relative_increase~Treatment*Sulfur+Experiment,data = rescue)
m.tuk<-TukeyHSD(m)
m.tuk$`Treatment:Sulfur`

ggsave(
  "relative_increase_final.tiff",
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

















