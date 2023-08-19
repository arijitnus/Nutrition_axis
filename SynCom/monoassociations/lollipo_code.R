#The scoring is based on the following eqn: 
#score= (ESbm + ESrl + ESsul + ESsa)/sum(ES[i=1....>n])
# we shall be plotting lollipop plot for individual strains with color of dots indicating the 
#number of parameters significantly affected by these strains
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/lollipop.xlsx",sheet="Sheet1",col_names=T,skip=0)
dat
dat$Strain<-as.factor(dat$Strain)
library(ggplot2)

p<-ggplot(dat, aes(x = Strain, y = score)) +
  geom_segment(aes(x = reorder(Strain, -score),
                   xend = reorder(Strain, -score),
                   y = 0, yend = score),
               color = "gray", lwd = 1) +
  geom_point(size = 4, pch = 21, bg = 4, col = 1) +
  theme_classic()+
  xlab("")+
  ylab("Score")+
  theme(axis.text.x = element_text(size = 14,angle = 60,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 0.15), expand = expansion(mult = c(0, 0)))

p

ggsave(
  "rscore_individual_strains.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()

#correlation among the phentoypes
cor_data<-read_excel("/Users/arijitmukherjee/Downloads/Corr_plot.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(cor_data)

pairs(cor_data)

#all of these parameters are not normal even after log transformation
shapiro.test(log(cor_data$biomass))
shapiro.test(log(cor_data$shoot_area))
shapiro.test(log(cor_data$root_length))
library(Hmisc)
res2 <- rcorr(as.matrix(cor_data),type = "spearman")
res2

#r=0.68 (biomass vs root length) P<0.05
#r=0.38 (biomass vs shoot length) P<0.05

#res2
#biomass shoot_area root_length
#biomass        1.00       0.38        0.68
#shoot_area     0.38       1.00       -0.15
#root_length    0.68      -0.15        1.00

#P
#biomass shoot_area root_length
#biomass             0.0000     0.0000     
#shoot_area  0.0000             0.0595     
#root_length 0.0000  0.0595    










