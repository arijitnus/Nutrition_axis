# Calculate SNR for each of the phenotypes tested. 
#Paredes et al. 2019 says signal is the mean of the phenotype 
#We define the signal of a specific input condition as the mean measurement over all replicates in that condition.
#In our case, we have strains and their activity as the input conditions
#Therefore, a total of 38 conditions (19 starins x 2 activities)
#Calculate the mean values for all phenotypes for all these 38 conditions and then 
#calculate variance of these signals
library(readxl)
library(dplyr)
library(ggplot2)
dat<-read_excel("/Users/arijitmukherjee/Downloads/SNR_calc.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dat

Signals_bm<-dat%>%group_by(Strain,Microbiome)%>%summarise(avg_bm=mean(biomass))
Signals_bm
Signals_sa<-dat%>%group_by(Strain,Microbiome)%>%summarise(avg_sa=mean(shoot_area))
Signals_sa
Signals_rl<-dat%>%group_by(Strain,Microbiome)%>%summarise(avg_rl=mean(root_length))
Signals_rl
Signals_ra<-dat%>%group_by(Strain,Microbiome)%>%summarise(avg_ra=mean(root_area))
Signals_ra

singal_df<-cbind(Signals_bm,Signals_sa,Signals_rl,Signals_ra)
names(singal_df)
signal_df<-singal_df[,-c(4,5,7,8,10,11)]

var_bm<-var(signal_df$avg_bm)
var_sa<-var(signal_df$avg_sa)
var_rl<-var(signal_df$avg_rl)
var_ra<-var(signal_df$avg_ra)

#Calculate noise for each of the conditions
# We need to substract the average values for each of the conditions
strains<-unique(dat$Strain)
df=list()
for (st in strains) {
  mdf=subset(dat,Strain==st)
  mdf.hk=subset(mdf,Microbiome=="Heat killed")
  mdf.active=subset(mdf,Microbiome=="Active")
  mdf.hk$bm=mdf.hk$biomass-mean(mdf.hk$biomass)
  mdf.active$bm=mdf.active$biomass-mean(mdf.active$biomass)
  mdf.hk$sa=mdf.hk$shoot_area-mean(mdf.hk$shoot_area)
  mdf.active$sa=mdf.active$shoot_area-mean(mdf.active$shoot_area)
  mdf.hk$rl=mdf.hk$root_length-mean(mdf.hk$root_length)
  mdf.active$rl=mdf.active$root_length-mean(mdf.active$root_length)
  mdf.hk$ra=mdf.hk$root_area-mean(mdf.hk$root_area)
  mdf.active$ra=mdf.active$root_area-mean(mdf.active$root_area)
  df[[st]]<-rbind(mdf.hk,mdf.active)
}
merged_df<-rbind(df$`8A2`,df$P33G,df$`9X2`,df$`1A1`,df$`4C`,df$`8X1`,
                 df$`9B2`,df$`10B2`,df$P31D,df$`7F21`,df$`7F21`,
                 df$`8X4`,df$`6A2`,df$`6A2`,df$`9B1`,df$`3C2`,
                 df$P32B1,df$`6A1`,df$`3C1`,df$`9F3`,df$SPAF18)
merged_df

var_bm_N<-var(merged_df$bm)
var_sa_N<-var(merged_df$sa)
var_rl_N<-var(merged_df$rl)
var_ra_N<-var(merged_df$ra)

SNR_bm<-var_bm/var_bm_N
SNR_sa<-var_sa/var_sa_N
SNR_rl<-var_rl/var_rl_N
SNR_ra<-var_ra/var_ra_N
SNR_bm#7.914729
SNR_sa#6.486614
SNR_rl#5.722307
SNR_ra#5.657536























































