#Calculate the effect size of all chemicals in S0 conditions
library(readxl)
library(ggplot2)
library(ggpubr)
dat<-read_excel('/Users/arijitmukherjee/Downloads/std_curve.xlsx',
                sheet="Sheet1",col_names = T,skip = 0)
dat

p<-ggplot(dat,aes(x=Conc,y=`peak area`/10^3))+
  geom_point()+
  geom_smooth(method = 'lm',se=FALSE)+
  stat_regline_equation(label.y = 600, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 550, aes(label = ..rr.label..))+
  theme_classic()+
  xlab("Concentration of GSH (ppb)")+
  ylab("Quantifier ion peak area")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(0,700), expand = expansion(mult = c(0, 0)))

p

ggsave(
  "std_curve_GSH.tiff",
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


