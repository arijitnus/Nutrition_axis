#Plot the correlation between shoot biomass and sulfate content
corr_df<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet = "corr",col_names = T,skip = 0)
head(corr_df)
corr_df$Genotype<-as.factor(corr_df$Genotype)
corr_df$Experiment<-as.factor(corr_df$Experiment)
head(corr_df)
cols <- c('#000000',"#008000","#DC143C","#1E90FF", "#BA55D3","#D2691E","#000099","#808000")
corr_df$`Sulphate (uM/mgDM)`
library(ggpubr)
p<-ggplot(corr_df,aes(x=leaf,y=`Sulphate (uM/mgDM)`))+
  geom_point(aes(col=Genotype),alpha=0.5,size=2.5)+
  scale_color_manual(values = cols)+
  geom_smooth(method = "lm",se=TRUE,color="black")+
  theme_classic()+
  xlab("Shoot biomass (g)")+
  ylab(expression("Shoot sulphate content (" * mu*"M per mg of dry biomass)"))+
  theme(axis.text.x = element_text(size = 14,angle = 30,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
  scale_y_continuous(limits = c(0,60), expand = expansion(mult = c(0, 0)))
p
ggsave(
  "correlation_bm_sulf_final.tiff",
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
shapiro.test(corr_df$`Sulphate (uM/mgDM)`)#normal distribution
shapiro.test(corr_df$leaf)#normal distribution
#calculate correlation co-efficient
cor.test(corr_df$leaf, corr_df$`Sulphate (uM/mgDM)`, method="pearson")
#Pearson's product-moment correlation

#data:  corr_df$leaf and corr_df$`Sulphate (uM/mgDM)`
#t = 3.1787, df = 76, p-value = 0.00214
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1299344 0.5250652
#sample estimates:
#  cor 
#0.3425602 


