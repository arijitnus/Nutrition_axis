#plotting growth curves for all the selected SynCom members
data<-read_excel("/Users/arijitmukherjee/Downloads/Syncom_growth_curve.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(data)
colnames(data)
library(data.table)
library(tidyr)
long<-data%>%pivot_longer(
  cols = '9B2' : '9F3',
  names_to = "strain",
  values_to = "OD"
)
long
long$hrs<-long$Time/60
p<-ggplot(long,aes(x=hrs,y=OD))+
  geom_point(aes(col=as.factor(strain)),size=1,alpha=0.5)+
  xlim(0,30)+
  ylim(0,1.1)+
  theme_classic()+
  xlab("Time (hrs)")+
  geom_vline(xintercept = c(4,24),linetype="dotted",color="blue",size=1)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.text.y.right = element_text(size = 14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  theme(legend.text = element_text(size=11))+
  scale_y_continuous(limits = c(0, 1.1), expand = expansion(mult = c(0, 0)))+
  scale_x_continuous(limits = c(0, 30), expand = expansion(mult = c(0, 0)))
p
ggsave(
  "growth_curve_all18_strains.tiff",
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