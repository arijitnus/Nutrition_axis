# Simple line plot
# Change point shapes and line types by groups
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/biomass_rnaseq_time.xlsx",sheet="Sheet1",
                col_names = T,skip = 0)
dat$Microbiome<-as.factor(dat$Microbiome)
dat$Day<-as.factor(dat$Day)
df3 <- dat%>%group_by(Day,Microbiome)%>%summarise(mean=mean(biomass),sd=sd(biomass))
df3

p<-ggplot(df3, aes(x=Day, y=mean, group = Microbiome, shape=Microbiome, linetype=Microbiome))+ 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() +
  geom_point()+
  labs(title="",x="Days after SPAF18 treatment", y = "Biomass of five plants (mg)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))
q<-p+geom_point(data = dat,aes(x=Day,y=biomass,shape=Microbiome))
ggsave(
  "rnaseq_timeline1.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 7.5,
  height = 7,
  units = "in",
  dpi = 400,
)
dev.off()



