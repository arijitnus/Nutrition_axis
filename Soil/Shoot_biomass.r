#Plotting biomass of root, shoot and total biomass for all the genotypes across experiments
library(readxl)
data<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "biomass",col_names = T,skip = 0)
library(ggplot2)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#FF1493","#808000")
head(data)
data$Genotype<-as.factor(data$Genotype)
data$Experiment<-as.factor(data$Experiment)
#first check normality for root, shoot and total biomass
qqnorm(data$Leaf)
qqline(data$Leaf)
shapiro.test(data$Leaf)
#total biomass follow normal distribution
#root doesn't follow normality
#shoot follows normality
m_shoot_biomass<-aov(Leaf~Genotype+Experiment, data = data)
summary(m_shoot_biomass)
summary.lm(m_shoot_biomass)
m_shoot.tuk<-TukeyHSD(m_shoot_biomass)
m_shoot.tuk$Genotype

#If we look at the p-values from the compariosns, sultr2;1 vs Col-0 is significant for experiment 2
#sultr2;2 is overall significant at padj =0.06, therefore these two can be considered

summary.lm(m_shoot_biomass)
data$total

###plot the boxplot for biomass
shoot<-ggplot(data,aes(x=Genotype,y=Leaf))+
  geom_boxplot(aes(col=Genotype))+
  geom_jitter(aes(shape=Experiment,col=Genotype),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot biomass (g)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-shoot+scale_y_continuous(limits = c(0, 3), expand = expansion(mult = c(0, 0)))
q
ggsave(
  "shoot_final.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 400,
)
dev.off()



