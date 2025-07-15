library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "sulfate",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")

head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)


#Sulfate
sulfur<-ggplot(data,aes(x=Genotype,y=`Sulphate (uM/mgDM)`))+
  geom_boxplot(aes(col=Genotype),lwd=0.8)+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot sulphate content (" * mu*"mol per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(10,55), expand = expansion(mult = c(0, 0)))
sulfur
saveRDS(sulfur,"sulfate_plot_final.rds")
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
#Fit anova model based on genotype and experiment
m_sulf<-aov(`Sulphate (uM/mgDM)`~Genotype+Experiment)
gr <- multcomp::cld(multcomp::glht(m_sulf,linfct = multcomp::mcp(Genotype = "Tukey")))
gr
#Col-0   msa1-3   sdi2-1  slim1-1 sultr1;1 sultr1;2 sultr2;1 
#"ab"      "a"     "ab"     "ab"     "ab"     "ab"     "ab" 
#sultr2;2 
#"b"
