library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "sulfate",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#FF1493","#808000")

head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)


#Sulfate
sulfur<-ggplot(data,aes(x=Genotype,y=`Sulphate (uM/mgDM)`))+
  geom_boxplot(aes(col=Genotype))+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylim(c(10,55))+
  ylab(expression("Shoot sulphate content (" * mu*"M per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))
sulfur

ggsave(
  "sulfate_final.tiff",
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


#phosphate

phosphate<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "Phosphate",col_names = T,skip = 0)
phosphate$`Phosphate (uM/mg DM)`
phosphate$Genotype<-as.factor(phosphate$Genotype)
phosphate$Experiment<-as.factor(phosphate$Experiment)
class(phosphate$`Phosphate (uM/mg DM)`)
phosphate_plot<-ggplot(phosphate,aes(x=Genotype,y=`Phosphate (uM/mg DM)`))+
  geom_boxplot(aes(col=Genotype))+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylim(c(0,50))+
  ylab(expression("Shoot phosphate content (" * mu*"M per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))

phosphate_plot
ggsave(
  "phosphate_final.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300,
)
dev.off()



####Nitrate
nitrate<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "Nitrate",col_names = T,skip = 0)

nitrate$Genotype<-as.factor(nitrate$Genotype)
nitrate$Experiment<-as.factor(nitrate$Experiment)
nitrate$`Nitrate (uM/mg DM)`
nitrate_plot<-ggplot(nitrate,aes(x=Genotype,y=`Nitrate (uM/mg DM)`))+
  geom_boxplot(aes(col=Genotype))+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylim(c(0,220))+
  ylab(expression("Shoot nitrate content (" * mu*"M per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))

nitrate_plot
ggsave(
  "nitrate_final.tiff",
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

saveRDS(nitrate_plot,"Nitrateplot.rds")
saveRDS(phosphate_plot,"phosphateplot.rds")
saveRDS(sulfur,"sufateplot.rds")




