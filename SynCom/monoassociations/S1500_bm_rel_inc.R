library(readxl)
mono_dat<-read_excel('/Users/arijitmukherjee/Downloads/S1500_mono.xlsx',
                     sheet="Sheet1",col_names = T,skip = 0)

mono_dat<-as.data.frame(mono_dat)
library(dplyr)
avg_mono<-mono_dat%>%group_by(Strains)%>%summarise(mean_rel=mean(rel_inc))
avg_mono

cols<-c("#004C99","#99004C","#994C00")
levels<-c("Proteobacteria","Actinobacteria","Firmicutes")
mono_dat$Phylum<-factor(mono_dat$Phylum,levels = levels)

mono_dat

library(ggplot2)
p1<-ggplot(mono_dat, aes(x = Strains, y = rel_inc, color = Phylum, group = Strains)) +
  geom_line() +          # Line plot
  geom_point() +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Relative increase in fresh biomass to heat killed control")+
  theme(axis.text.x = element_text(size = 14,angle = 40,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(-0.6, 0.85), expand = expansion(mult = c(0, 0)))
q<-p1+geom_hline(yintercept = 0,linetype=2)
q
ggsave(
  "relative_bm_mono_S1500.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()



