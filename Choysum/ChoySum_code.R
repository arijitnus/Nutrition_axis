#plotting and testing the Choysum results shoopt biomass
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/CS_shoot_biomass.xlsx",sheet = "CS",col_names = T,skip = 0)
head(dat)
dat$TREATMENT<-as.factor(dat$TREATMENT)
dat$MEDIUM<-as.factor(dat$MEDIUM)
dat$Experiment<-as.factor(dat$Experiment)

head(dat)
shapiro.test(dat$BIOMASS)#normally distributed
m<-aov(BIOMASS~TREATMENT*MEDIUM+Experiment,data = dat)
m.tuk<-TukeyHSD(m)
m.tuk$`TREATMENT:MEDIUM`
#write down all the results of comparisons
write.table(m.tuk$`TREATMENT:MEDIUM`,"CS_comparisons.tsv",sep = "\t")
#diff        lwr       upr        p adj
#HK:S0-ACTIVE:S0        -32.89 -56.076591 -9.703409 0.0028155257
#ACTIVE:S1500-ACTIVE:S0   5.57 -17.616591 28.756591 0.9152381641
#HK:S1500-ACTIVE:S0     -12.96 -36.146591 10.226591 0.4430721953
#ACTIVE:S1500-HK:S0      38.46  15.273409 61.646591 0.0004478001
#HK:S1500-HK:S0          19.93  -3.256591 43.116591 0.1130565076
#HK:S1500-ACTIVE:S1500  -18.53 -41.716591  4.656591 0.1556083281

#plot the boxplot for the comparisons

cols=c("#DC143C",'#000000')
head(dat)
level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")
dat$MEDIUM<-factor(dat$MEDIUM,levels = level_order2)

cs<-ggplot(dat,aes(x=factor(TREATMENT,levels=level_order),y=BIOMASS))+
  geom_boxplot(aes(col=TREATMENT),lwd=0.8)+
  geom_jitter(aes(col=TREATMENT,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Shoot biomass in mg")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))+
  scale_y_continuous(limits = c(50, 250), expand = expansion(mult = c(0, 0)))
q<-cs+facet_grid(.~MEDIUM)
q
ggsave(
  "choysum_final.tiff",
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













