#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
LA<<-read_excel("/Users/arijitmukherjee/Downloads/SynCom_LA_Col.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(LA)
LA$Sulphur<-as.factor(LA$Sulphur)
LA$SynCom<-as.factor(LA$SynCom)
LA$`Area (Total)`
cols=c('#000000',"#DC143C")
head(LA)
level_order<-c("Heat killed","Active")
level_order2<-c("Sufficient","Deficient")
LA$Sulphur<-factor(LA$Sulphur,levels = level_order2)

la<-ggplot(LA,aes(x=factor(SynCom,levels=level_order),y=`Area (Total)`))+
  geom_boxplot(aes(col=Sulphur))+
  geom_jitter(aes(col=Sulphur),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Area in cm"^2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0)))
q<-la+facet_grid(.~Sulphur)
q
ggsave(
  "shoot_area_final.tiff",
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











