# Load necessary libraries
library(dplyr)
library(effsize)
library(readxl)
biomass<-read_excel('/Users/arijitmukherjee/Downloads/Effsize_biomass.xlsx',sheet = "others",col_names = T,skip = 0)

biomass$Microbiome<-as.factor(biomass$Microbiome)
biomass$Sulfur<-as.factor(biomass$Sulfur)
# Example data
# df <- data.frame(
#   sample_id = 1:100,
#   biomass = rnorm(100, mean=10, sd=5),
#   heat_microbiome = sample(c(0, 1), 100, replace=TRUE),
#   media = sample(c("Media1", "Media2"), 100, replace=TRUE)
# )

# Function to calculate effect size for each media
calculate_effect_size <- function(data, media_type) {
  subset_data <- data %>% dplyr::filter(Sulfur == media_type)
  effect_size <- cohen.d(subset_data$avg_biomass ~ subset_data$Microbiome)
  return(effect_size)
}


# Calculate effect size for S1500
effect_size_S1500 <- calculate_effect_size(biomass, "S1500")
print(effect_size_S1500)

# Calculate effect size for S0
effect_size_S0 <- calculate_effect_size(biomass, "S0")
print(effect_size_S0)

#calculate this for shoot area

others<-read_excel('/Users/arijitmukherjee/Downloads/Effsize_biomass.xlsx',sheet = "others",col_names = T,skip = 0)
others

others$Microbiome<-as.factor(others$Microbiome)
others$Medium<-as.factor(others$Medium)


#for shoot area
calculate_effect_size <- function(data, media_type) {
  subset_data <- data %>% dplyr::filter(Medium == media_type)
  effect_size <- cohen.d(subset_data$`shoot area`~ subset_data$Microbiome)
  return(effect_size)
}

S1500_sa<-calculate_effect_size(others, "S1500")
S0_sa<-calculate_effect_size(others, "S0")
S0_sa$estimate



#for rootlength
calculate_effect_size <- function(data, media_type) {
  subset_data <- data %>% dplyr::filter(Medium == media_type)
  effect_size <- cohen.d(subset_data$`root length`~ subset_data$Microbiome)
  return(effect_size)
}
S1500_rl<-calculate_effect_size(others, "S1500")
S0_rl<-calculate_effect_size(others, "S0")

others<-na.omit(others)
#for root network
calculate_effect_size <- function(data, media_type) {
  subset_data <- data %>% dplyr::filter(Medium == media_type)
  effect_size <- cohen.d(subset_data$`root network`~ subset_data$Microbiome)
  return(effect_size)
}


S1500_rn<-calculate_effect_size(others, "S1500")
S0_rn<-calculate_effect_size(others, "S0")

#for lateral roots
calculate_effect_size <- function(data, media_type) {
  subset_data <- data %>% dplyr::filter(Medium == media_type)
  effect_size <- cohen.d(subset_data$`avg number of lat roots`~ subset_data$Microbiome)
  return(effect_size)
}


S1500_lr<-calculate_effect_size(others, "S1500")
S0_lr<-calculate_effect_size(others, "S0")


class(name)
class(S_sufficient)
name = c('biomass','shoot area','root length','root network','lateral roots')
S_sufficient<-c(effect_size_S1500$estimate,S1500_sa$estimate,S1500_rl$estimate,S1500_rn$estimate,S1500_lr$estimate)
S_deficient=c(effect_size_S0$estimate,S0_sa$estimate,S0_rl$estimate,S0_rn$estimate,S0_lr$estimate)

df<-data.frame(name=name,S_sufficient=S_sufficient,S_deficient=S_deficient)
df
rownames(df)<-df$name
df<-df[,-1]
df<-t(df)
df
class(df)
df<-as.data.frame(df)
library(tidyr)
#pivot the data frame into a long format
df2<-df %>% pivot_longer(cols=c('biomass', 'shoot area','root length','root network','lateral roots'),
                         names_to='vars',
                         values_to='effsize')
df2

df2$vars<-as.factor(df2$vars)
df2$vars
df2$sulphur<-c(rep("S-Sufficient",5),rep("S-Deficient",5))
df2$sulphur<-as.factor(df2$sulphur)
cols<-c("#CC0066","#4C9900","#606060","#FF8000","#0080FF")
level_order<-c("S-Sufficient","S-Deficient")
df2$sulphur<-factor(df2$sulphur,levels = level_order)
df2
range(df2$effsize)

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
q<-effsize_syn+scale_y_continuous(limits = c(-1, 2), expand = expansion(mult = c(0, 0)))
r<-q+geom_hline(yintercept = 0,linetype="dashed",size=0.4)
r
ggsave(
  "effsize_syncom_final.tiff",
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
write.table(df,'effsize_all.tsv',sep = "\t")














