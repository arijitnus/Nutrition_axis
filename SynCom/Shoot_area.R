#script for plotting the boxplots of leaf area based on Syncom data
library(readxl)
library(ggplot2)
library(dplyr)
LA<<-read_excel("/Users/arijitmukherjee/Downloads/Shoot_area_combined.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(LA)
LA$Sulfur<-as.factor(LA$Sulfur)
LA$microbiome<-as.factor(LA$microbiome)

LA


cols=c('#000000',"#DC143C")
head(LA)
level_order<-c("Heat killed","Active")
level_order2<-c("S1500","S15")
LA$Sulfur<-factor(LA$Sulfur,levels = level_order2)
LA$microbiome<-factor(LA$microbiome,levels = level_order)
LA$Experiment<-as.factor(LA$Experiment)

la<-ggplot(LA,aes(x=factor(microbiome,levels = level_order),y=shoot_area))+
  geom_boxplot(aes(col=Sulfur))+
  geom_jitter(aes(col=Sulfur,shape=Experiment),size=2.5,width = 0.4, alpha=0.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab(expression("Shoot area in cm"^2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0)))
q<-la+facet_grid(.~Sulfur)
q

ggsave(
  "shoot_area_combined.tiff",
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


m<-aov(shoot_area~microbiome*Sulfur+Experiment,data = LA)
m.tuk<-TukeyHSD(m)
m.tuk$`microbiome:Sulfur`
#write down all the results of comparisons
write.table(m.tuk$`Microbiome:Sulphur`,"tukey_root_length.tsv",sep = "\t")

#diff          lwr         upr
#Heat killed:S1500-Active:S1500    -0.122081 -0.233337182 -0.01082482
#Active:S15-Active:S1500           -0.020100 -0.131356182  0.09115618
#Heat killed:S15-Active:S1500      -0.331300 -0.442556182 -0.22004382
#Active:S15-Heat killed:S1500       0.101981 -0.009275182  0.21323718
#Heat killed:S15-Heat killed:S1500 -0.209219 -0.320475182 -0.09796282
#Heat killed:S15-Active:S15        -0.311200 -0.422456182 -0.19994382
#p adj
#Heat killed:S1500-Active:S1500    2.692317e-02
#Active:S15-Active:S1500           9.614314e-01
#Heat killed:S15-Active:S1500      1.112038e-08
#Active:S15-Heat killed:S1500      8.220129e-02
#Heat killed:S15-Heat killed:S1500 7.373388e-05
#Heat killed:S15-Active:S15        4.541424e-08









