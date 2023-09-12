#plot the effect size based on syncom effect on parameters of shoot area, root length,
# and biomass when compared by heat killed and acrive syncom
#First calculate the effect size based on the biomass for active SPAF18 treatment
library(readxl)
library(effsize)
biomass<-read_excel("/Users/arijitmukherjee/Downloads/effsize_input_SPAF18.xlsx",sheet = "biomass",col_names = T,skip = 0)
LA<-read_excel("/Users/arijitmukherjee/Downloads/effsize_input_SPAF18.xlsx",sheet = "shoot_area",col_names = T,skip = 0)
rl<-read_excel("/Users/arijitmukherjee/Downloads/effsize_input_SPAF18.xlsx",sheet = "root_length",col_names = T,skip = 0)
ra<-read_excel("/Users/arijitmukherjee/Downloads/effsize_input_SPAF18.xlsx",sheet = "root_area",col_names = T,skip = 0)
lr<-read_excel("/Users/arijitmukherjee/Downloads/effsize_input_SPAF18.xlsx",sheet = "LR",col_names = T,skip = 0)


LA$Sulphur<-as.factor(LA$Sulphur)
LA$Microbiome<-as.factor(LA$Microbiome)

rl$Sulphur<-as.factor(rl$Sulphur)
rl$Microbiome<-as.factor(rl$Microbiome)

ra$Sulphur<-as.factor(ra$Sulphur)
ra$Microbiome<-as.factor(ra$Microbiome)

biomass$Sulphur<-as.factor(biomass$Sulphur)
biomass$Microbiome<-as.factor(biomass$Microbiome)

#calculate the effect size for these three parameters across the sulphur deficient and sufficient conditions
biomass_suff<-subset(biomass,Sulphur=="Sufficient")
biomass_def<-subset(biomass,Sulphur=="Deficient")
biomass_suff_active<-subset(biomass_suff,Microbiome=="Active")
biomass_suff_hk<-subset(biomass_suff,Microbiome=="Heat killed")
biomass_suff_eff<-cohen.d(biomass_suff_active$biomass,biomass_suff_hk$biomass)
biomass_suff_eff

biomass_def_active<-subset(biomass_def,Microbiome=="Active")
biomass_def_hk<-subset(biomass_def,Microbiome=="Heat killed")
biomass_def_eff<-cohen.d(biomass_def_active$biomass,biomass_def_hk$biomass)

#Calculate effect size for leaf area
LA_suff_hk<-subset(LA,Sulphur=="Sufficient"&Microbiome=="Heat killed")
LA_suff_active<-subset(LA,Sulphur=="Sufficient"&Microbiome=="Active")
LA_def_active<-subset(LA,Sulphur=="Deficient"&Microbiome=="Active")
LA_def_hk<-subset(LA,Sulphur=="Deficient"&Microbiome=="Heat killed")

LA_suff_eff<-cohen.d(LA_suff_active$shoot_area,LA_suff_hk$shoot_area)
LA_def_eff<-cohen.d(LA_def_active$shoot_area,LA_def_hk$shoot_area)

#calculate for root length
rl_suff_hk<-subset(rl,Sulphur=="Sufficient"&Microbiome=="Heat killed")
rl_suff_active<-subset(rl,Sulphur=="Sufficient"&Microbiome=="Active")
rl_def_active<-subset(rl,Sulphur=="Deficient"&Microbiome=="Active")
rl_def_hk<-subset(rl,Sulphur=="Deficient"&Microbiome=="Heat killed")

rl_suff_eff<-cohen.d(rl_suff_active$root_length,rl_suff_hk$root_length)
rl_def_eff<-cohen.d(rl_def_active$root_length,rl_def_hk$root_length)

#calculate for root area
ra_suff_hk<-subset(ra,Sulphur=="Sufficient"&Microbiome=="Heat killed")
ra_suff_active<-subset(ra,Sulphur=="Sufficient"&Microbiome=="Active")
ra_def_active<-subset(ra,Sulphur=="Deficient"&Microbiome=="Active")
ra_def_hk<-subset(ra,Sulphur=="Deficient"&Microbiome=="Heat killed")

ra_suff_eff<-cohen.d(ra_suff_active$`Root area`,ra_suff_hk$`Root area`)
ra_def_eff<-cohen.d(ra_def_active$`Root area`,ra_def_hk$`Root area`)

#calculate for Lateral roots

lr_suff_hk<-subset(lr,Sulphur=="Sufficient"&Microbiome=="Heat killed")
lr_suff_active<-subset(lr,Sulphur=="Sufficient"&Microbiome=="Active")
lr_def_active<-subset(lr,Sulphur=="Deficient"&Microbiome=="Active")
lr_def_hk<-subset(lr,Sulphur=="Deficient"&Microbiome=="Heat killed")

lr_suff_eff<-cohen.d(lr_suff_active$LR,lr_suff_hk$LR)
lr_def_eff<-cohen.d(lr_def_active$LR,lr_def_hk$LR)


###Create dataframe based on this 
df<-data.frame(biomass=c(biomass_suff_eff$estimate,biomass_def_eff$estimate),
               LA=c(LA_suff_eff$estimate,LA_def_eff$estimate),
               rl=c(rl_suff_eff$estimate,rl_def_eff$estimate),
               ra=c(ra_suff_eff$estimate,ra_def_eff$estimate),
               lr=c(lr_suff_eff$estimate,lr_def_eff$estimate))

rownames(df)<-c("Sufficient","Deficient")
df

library(tidyr)
#pivot the data frame into a long format
df2<-df %>% pivot_longer(cols=c('biomass', 'LA','rl','ra','lr'),
                         names_to='vars',
                         values_to='effsize')
df2

df2$vars<-as.factor(df2$vars)
df2$vars
df2$sulphur<-c(rep("Sufficient",5),rep("Deficient",5))
df2$sulphur<-as.factor(df2$sulphur)
cols<-c("#CC0066","#4C9900","#606060","#FF8000","#0080FF")
level_order<-c("Sufficient","Deficient")
df2$sulphur<-factor(df2$sulphur,levels = level_order)
df2

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
q<-effsize_syn+scale_y_continuous(limits = c(-2, 4.5), expand = expansion(mult = c(0, 0)))
q
ggsave(
  "effsize_syncom_revised.tiff",
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
