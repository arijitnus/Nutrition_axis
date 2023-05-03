#Plot the linear regression for the three kind of niches with first CAP axis
library(readxl)
library(dplyr)
library(ggplot2)
df<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/16S_rhizosphere/CAP_sulfate_corr.xlsx",sheet = "combined",col_names = T,skip = 0)
head(df)
library(ggplot2)
rhizo<-df%>%filter(Niche=="rhizosphere")
root<-df%>%filter(Niche=="root")
leaf<-df%>%filter(Niche=="leaf")

rhizo$Genotype<-as.factor(rhizo$Genotype)
library("ggpubr")
ggscatter(leaf, x = "sulfate", y = "CAP1",
          add = "reg.line", conf.int = TRUE,
          cor.method = "pearson",cor.coef = TRUE,
          xlab = "Shoot Sulphate content", ylab = "CAP1")
#Correaltion coefficents:
#Rhizosphere: R=0.38; p=0.014
#Root: R=-0.26; p=0.14
#leaf: R=-0.36, p =0.023

cols<-c('#000000',"#008000","#DC143C","#000099","#808000")
p<-ggplot(rhizo,aes(x=sulfate,y=CAP1,col=Genotype))+
  geom_point(aes(col=Genotype),size=2.5,alpha=0.6)+
  scale_color_manual(values = cols)+
  geom_smooth(method=lm , color="black", se=TRUE)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab(expression("Shoot sulphate content (" * mu*"M per mg of dry biomass)"))+
  ylab("CAP1")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))
  #scale_y_continuous(limits = c(1, 3), expand = expansion(mult = c(0, 0)))
p
ggsave(
  "rhizo_sulfate_corr.tiff",
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

root
##Root
q<-ggplot(root,aes(x=sulfate,y=CAP1,col=Genotype))+
  geom_point(aes(col=Genotype),size=2.5,alpha=0.6)+
  scale_color_manual(values = cols)+
  geom_smooth(method=lm , color="black", se=TRUE)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab(expression("Shoot sulphate content (" * mu*"M per mg of dry biomass)"))+
  ylab("CAP1")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))
#scale_y_continuous(limits = c(1, 3), expand = expansion(mult = c(0, 0)))
q
ggsave(
  "root_sulfate_corr.tiff",
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



##Leaf
r<-ggplot(leaf,aes(x=sulfate,y=CAP1,col=Genotype))+
  geom_point(aes(col=Genotype),size=2.5,alpha=0.6)+
  scale_color_manual(values = cols)+
  geom_smooth(method=lm , color="black", se=TRUE)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab(expression("Shoot sulphate content (" * mu*"M per mg of dry biomass)"))+
  ylab("CAP1")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7))
#scale_y_continuous(limits = c(1, 3), expand = expansion(mult = c(0, 0)))
r
ggsave(
  "leaf_sulfate_corr.tiff",
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











