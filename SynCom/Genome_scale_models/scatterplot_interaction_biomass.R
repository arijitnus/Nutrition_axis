library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel('/Users/arijitmukherjee/Downloads/New_data.xlsx',sheet = "sum_I_vs_biomass",
                col_names = T,skip = 0)
head(dat)

p<-ggplot(dat,aes(x=sum_I,y=mean_inc))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = '',
       x='Sum of interaction scores',
       y='Mean relative increase in biomass to heat killed control')+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.title.x=element_text(size=16),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())

p

ggsave(
  "scatterplot_interactions_biomass.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 7,
  height = 7,
  units = "in",
  dpi = 400,
)
dev.off()


summary(lm(dat$mean_inc~dat$sum_I))#
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -0.04661    0.15598  -0.299   0.7685  
#dat$sum_I   -0.64634    0.28336  -2.281   0.0349 *

  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  
  
#cor.test(dat$mean_inc,dat$sum_I)#both are notmal distribution and 
#pearson correlation co-efficient is -0.47, p=0.03


