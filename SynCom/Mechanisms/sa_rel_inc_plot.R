library(readxl)
library(ggplot2)
dat<-read_excel("/Users/arijitmukherjee/Downloads/shoot_area_plot.xlsx",
                sheet = "Sheet1",col_names = T,skip = 0)

head(dat)


#SynCom growth rescue plot and script
#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)

dat$`S-medium`<-as.factor(dat$`S-medium`)
dat$Treatment<-as.factor(dat$Treatment)

#rescue$label<-as.factor(rescue$label)
cols=c('#000000',"#DC143C")

level_order<-c("6A2_ex","del_gshA")
level_order2<-c("Sufficient","Deficient")

dat$Experiment<-as.factor(dat$Experiment)
dat$Relative_increase



cs<-ggplot(dat,aes(x=factor(`S-medium`,levels=level_order2),y=Relative_increase))+
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
  scale_y_continuous(limits = c(-0.5, 0.8), expand = expansion(mult = c(0, 0)))
q<-cs+geom_hline(yintercept = 0,linetype=2)+facet_grid(.~Treatment)
q

ggsave(
  "relative_increase_shoot_area.tiff",
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


shapiro.test(dat$Relative_increase)#normal distribution
#post hoc tukey test
rescue$`S-medium`

dat$Sulfur<-dat$`S-medium`
#perform the tukey test
m<-aov(Relative_increase~Treatment*Sulfur+Experiment,data = dat)
m.tuk<-TukeyHSD(m)
m.tuk$`Treatment:Sulfur`
#> m.tuk$`Treatment:Sulfur`
#diff         lwr         upr      p adj
#del_gshA:Deficient-6A2_ex:Deficient    -0.180491495 -0.32837862 -0.03260437 0.01088154
#6A2_ex:Sufficient-6A2_ex:Deficient     -0.119639679 -0.26752680  0.02824744 0.15211957
#del_gshA:Sufficient-6A2_ex:Deficient   -0.111981258 -0.26248606  0.03852354 0.21092523
#6A2_ex:Sufficient-del_gshA:Deficient    0.060851816 -0.08703531  0.20873894 0.69626895
#del_gshA:Sufficient-del_gshA:Deficient  0.068510237 -0.08199456  0.21901504 0.62494067
#del_gshA:Sufficient-6A2_ex:Sufficient   0.007658421 -0.14284638  0.15816322 0.99911089



















