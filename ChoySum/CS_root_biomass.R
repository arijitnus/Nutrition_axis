#Calculate root biomass of choysum SPAF18 plants as well
#plotting and testing the Choysum results shoopt biomass
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/CS_biomass.xlsx",sheet = "CS_combined_root",col_names = T,skip = 0)
head(dat)
dat$TREATMENT<-as.factor(dat$TREATMENT)
dat$MEDIUM<-as.factor(dat$MEDIUM)
dat$Experiment<-as.factor(dat$Experiment)
head(dat)


head(dat)
shapiro.test(dat$BIOMASS)#normally distributed
m<-aov(BIOMASS~TREATMENT*MEDIUM+Experiment,data = dat)
m.tuk<-TukeyHSD(m)
m.tuk

#write down all the results of comparisons
write.table(m.tuk$`TREATMENT:MEDIUM`,"CS_root_biomass_comparison.tsv",sep = "\t")
#$`TREATMENT:MEDIUM`
#diff        lwr        upr     p adj
#HK:S0-ACTIVE:S0        -4.94  -9.993088 0.11308829 0.0572994
#ACTIVE:S1500-ACTIVE:S0 -5.01 -10.063088 0.04308829 0.0526784
#HK:S1500-ACTIVE:S0     -4.12  -9.173088 0.93308829 0.1431325
#ACTIVE:S1500-HK:S0     -0.07  -5.123088 4.98308829 0.9999808
#HK:S1500-HK:S0          0.82  -4.233088 5.87308829 0.9714058
#HK:S1500-ACTIVE:S1500   0.89  -4.163088 5.94308829 0.9639347


cols=c('#000000','#DC143C')
head(dat)
level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")
dat$MEDIUM<-factor(dat$MEDIUM,levels = level_order2)

range(dat$BIOMASS)
cs<-ggplot(dat,aes(x=factor(TREATMENT,levels=level_order),y=BIOMASS))+
  geom_boxplot(aes(col=MEDIUM),lwd=0.8)+
  geom_jitter(aes(col=MEDIUM,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Root biomass in mg")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(5, 30), expand = expansion(mult = c(0, 0)))
q<-cs+facet_grid(.~MEDIUM)
q
ggsave(
  "choysum_final_root.tiff",
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


