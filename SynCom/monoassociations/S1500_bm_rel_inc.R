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


# Calculate the mean and standard error for each group
data_summary <- mono_dat %>%
  group_by(Strains, Phylum) %>%
  summarise(
    mean_rel_bm = mean(rel_inc),  # mean of rel_bm
    se_rel_bm = sd(rel_inc) / sqrt(n())  # standard error of rel_bm
  )

data_summary <- data_summary %>%
  mutate(Strains = factor(Strains, levels = data_summary$Strains[order(data_summary$mean_rel_bm)]))




library(ggplot2)
p1<-# Create the plot with lines and error bars
  ggplot(data_summary, aes(x = Strains, y = mean_rel_bm, group = Phylum, color = Phylum)) + # Add lines for each group (Phylum)
  geom_point() +  # Add points for each mean value
  geom_errorbar(aes(ymin = mean_rel_bm - se_rel_bm, ymax = mean_rel_bm + se_rel_bm), 
                width = 0.2) +  # Add error bars (mean ± SE) 
  geom_point(data = mono_dat, aes(x = Strains, y = rel_inc, color = Phylum),
             shape = 16, alpha = 0.5, size = 2) +  
  xlab("")+
  theme_classic()+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  ylab("Relative increase in fresh biomass to heat killed control")+
  theme(axis.text.x = element_text(size = 14,angle = 40,hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(-0.55, 0.85), expand = expansion(mult = c(0, 0)))
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



