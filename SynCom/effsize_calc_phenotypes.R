library(readxl)
library(ggplot2)
library(dplyr)
data<-read_excel("/Users/arijitmukherjee/Downloads/eff_size.xlsx",sheet = "shoot_area",col_names = T,skip = 0)
data
data$bacteria<-as.factor(data$bacteria)
data$microbiome<-as.factor(data$microbiome)
datas<-split(data,data$bacteria)
library(effsize)
datas$`1A1`
datas$


effsize_10B2<-cohen.d(datas$`10B2`$biomass,datas$`10B2`$microbiome)$estimate
effsize_SPAF18<-cohen.d(datas$SPAF18$biomass,datas$SPAF18$microbiome)$estimate
effsize_8A2<-cohen.d(datas$`8A2`$biomass,datas$`8A2`$microbiome)$estimate
effsize_P33G<-cohen.d(datas$`P33G`$biomass,datas$`P33G`$microbiome)$estimate#
effsize_9X2<-cohen.d(datas$`9X2`$biomass,datas$`9X2`$microbiome)$estimate
effsize_1A1<-cohen.d(datas$`1A1`$biomass,datas$`1A1`$microbiome)$estimate
effsize_4C<-cohen.d(datas$`4C`$biomass,datas$`4C`$microbiome)$estimate
effsize_8X1<-cohen.d(datas$`8X1`$biomass,datas$`8X1`$microbiome)$estimate
effsize_9B2<-cohen.d(datas$`9B2`$biomass,datas$`9B2`$microbiome)$estimate
#effsize_10B2<-cohen.d(datas$`10B2`$biomass,datas$`10B2`$microbiome)$estimate
effsize_P31D<-cohen.d(datas$`P31D`$biomass,datas$`P31D`$microbiome)$estimate
effsize_7F21<-cohen.d(datas$`7F21`$biomass,datas$`7F21`$microbiome)$estimate
effsize_6A2<-cohen.d(datas$`6A2`$biomass,datas$`6A2`$microbiome)$estimate
effsize_8X4<-cohen.d(datas$`8X4`$biomass,datas$`8X4`$microbiome)$estimate

effsize_10B2#4.57
effsize_SPAF18#2.2595
effsize_8A2#3.04
effsize_P33G#0.47
effsize_9X2#1.36
effsize_1A1#0.55
effsize_4C#-0.15
effsize_8X1#2.22
effsize_9B2#2.9
effsize_10B2#4.57
effsize_P31D#3.81
effsize_7F21#6.05
effsize_6A2#6.17
effsize_8X4#1.1

df_sa_effsize<-data.frame(biomass=
                                 c(effsize_10B2,#4.57,
                                   effsize_SPAF18,#2.2595
                                   effsize_8A2,#3.04
                                   effsize_P33G,#0.47
                                   effsize_9X2,#1.36
                                   effsize_1A1,#0.55
                                   effsize_4C,#-0.15
                                   effsize_8X1,#2.22
                                   effsize_9B2,#2.9
                                   effsize_P31D,#3.81
                                   effsize_7F21,#6.05
                                   effsize_6A2,#6.17
                                   effsize_8X4),row.names = c("10B2","SPAF18","8A2","P33G",
                                                              "9X2","1A1","4C","8X1","9B2",
                                                              "P31D","7F21","6A2","8X4"))

df_sa_effsize

write.table(df_biomass_effsize,"shoot_area_effsize.tsv",sep = "\t")










