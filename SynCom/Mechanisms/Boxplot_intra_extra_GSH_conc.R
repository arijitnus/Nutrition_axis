library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel('/Users/arijitmukherjee/Downloads/Peak_area_box.xlsx',sheet="Sheet2",
                col_names = T,skip = 0)

head(dat)

dat$gneotype<-as.factor(dat$gneotype)
head(dat)
shapiro.test(dat$Conc)#non-normal

cols=c('#000000',"#DC143C")
head(dat)

level_order2<-c("wild_type","dgshA")
dat$gneotype<-factor(dat$gneotype,levels = level_order2)



pa2<-ggplot(dat,aes(x=factor(gneotype,levels = level_order2),y=Conc))+
  geom_boxplot(aes(col=gneotype))+
  geom_jitter(aes(col=gneotype),size=3.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Concentration of GSH (nM)")+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.title.x=element_text(size=16),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 80), expand = expansion(mult = c(0, 0)))
pa2


ggsave(
  "Extracellular_GSH_conc_box.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 7,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()







