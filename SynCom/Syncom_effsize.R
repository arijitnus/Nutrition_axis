#plot the effect size based on syncom effect on parameters of shoot area, root length,
# and biomass when compared by heat killed and acrive syncom
#First calculate the effect size based on the biomass for active SPAF18 treatment
library(readxl)
library(effsize)
biomass<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/effsize_input_SPAF18.xlsx",sheet = "biomass",col_names = T,skip = 0)
LA<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/effsize_input_SPAF18.xlsx",sheet = "shoot_area",col_names = T,skip = 0)
rl<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/effsize_input_SPAF18.xlsx",sheet = "root_length",col_names = T,skip = 0)

LA$sulfur<-as.factor(LA$sulfur)
LA$microbiome<-as.factor(LA$microbiome)

rl$sulfur<-as.factor(rl$sulfur)
rl$microbiome<-as.factor(rl$microbiome)
#calculate the effect size for these three parameters across the sulphur deficient and sufficient conditions
biomass_suff<-subset(biomass,sulfur=="Sufficient")
biomass_def<-subset(biomass,sulfur=="Deficient")
biomass_suff_active<-subset(biomass_suff,microbiome=="Active")
biomass_suff_hk<-subset(biomass_suff,microbiome=="Heat killed")
biomass_suff_eff<-cohen.d(biomass_suff_active$`biomass (mg)`,biomass_suff_hk$`biomass (mg)`)
biomass_suff_eff

biomass_def_active<-subset(biomass_def,microbiome=="Active")
biomass_def_hk<-subset(biomass_def,microbiome=="Heat killed")
biomass_def_eff<-cohen.d(biomass_def_active$`biomass (mg)`,biomass_def_hk$`biomass (mg)`)

#Calculate effect size for leaf area
LA_suff_hk<-subset(LA,sulfur=="Sufficient"&microbiome=="Heat killed")
LA_suff_active<-subset(LA,sulfur=="Sufficient"&microbiome=="Active")
LA_def_active<-subset(LA,sulfur=="Deficient"&microbiome=="Active")
LA_def_hk<-subset(LA,sulfur=="Deficient"&microbiome=="Heat killed")

LA_suff_eff<-cohen.d(LA_suff_active$area,LA_suff_hk$area)
LA_def_eff<-cohen.d(LA_def_active$area,LA_def_hk$area)
  
#calculate for root length
rl_suff_hk<-subset(rl,sulfur=="Sufficient"&microbiome=="Heat killed")
rl_suff_active<-subset(rl,sulfur=="Sufficient"&microbiome=="Active")
rl_def_active<-subset(rl,sulfur=="Deficient"&microbiome=="Active")
rl_def_hk<-subset(rl,sulfur=="Deficient"&microbiome=="Heat killed")

rl_suff_eff<-cohen.d(rl_suff_active$root_length,rl_suff_hk$root_length)
rl_def_eff<-cohen.d(rl_def_active$root_length,rl_def_hk$root_length)


###Create dataframe based on this 
df<-data.frame(biomass=c(biomass_suff_eff$estimate,biomass_def_eff$estimate),
               LA=c(LA_suff_eff$estimate,LA_def_eff$estimate),
               rl=c(rl_suff_eff$estimate,rl_def_eff$estimate))

rownames(df)<-c("Sufficient","Deficient")
df

library(tidyr)
#pivot the data frame into a long format
df2<-df %>% pivot_longer(cols=c('biomass', 'LA','rl'),
                         names_to='vars',
                         values_to='effsize')
df2

df2$vars<-as.factor(df2$vars)
df2$vars
df2$sulphur<-c(rep("Sufficient",3),rep("Deficient",3))
df2$sulphur<-as.factor(df2$sulphur)
cols<-c("#CC0066","#4C9900","#606060")
level_order<-c("Sufficient","Deficient")
df2$sulphur<-factor(df2$sulphur,levels = level_order)


library(ggplot2)
effsize_syn<-ggplot(df2,aes(x=sulphur,y=effsize,group=vars))+
  geom_point(aes(col=vars,size=2.5,alpha=0.7))+
  geom_line(aes(col=vars,alpha=0.7),linewidth=1.5)+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic()+
  xlab("")+
  ylab("Cohen's effect size of SPAF18")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14,vjust = 2),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.7))
q<-effsize_syn+scale_y_continuous(limits = c(-2, 8), expand = expansion(mult = c(0, 0)))
q
ggsave(
  "effsize_syncom.tiff",
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






  