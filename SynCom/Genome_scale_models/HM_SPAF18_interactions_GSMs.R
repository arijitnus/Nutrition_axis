library(ComplexHeatmap)
library(readxl)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/hm_interactions.xlsx",sheet = "Sheet2",col_names = T,skip = 0)
class(dat)
dat<-as.data.frame(dat)
dat
rownames(dat)<-dat$strains
dat<-dat[,-1]
class(dat$`10B2`)
dat$`10B2`<-as.numeric(dat$`10B2`)
dat$`3C1`<-as.numeric(dat$`3C1`)
dat$`3C2`<-as.numeric(dat$`3C2`)
dat$`4C`<-as.numeric(dat$`4C`)
dat$`6A1`<-as.numeric(dat$`6A1`)
dat$`6A2`<-as.numeric(dat$`6A2`)
dat$`7F2-1`<-as.numeric(dat$`7F2-1`)
dat$`8X4`<-as.numeric(dat$`8X4`)
dat$`9F3`<-as.numeric(dat$`9F3`)
dat$P31D<-as.numeric(dat$P31D)
dat$P32B1<-as.numeric(dat$P32B1)
dat$`1A1`<-as.numeric(dat$`1A1`)
dat$`8A-2`<-as.numeric(dat$`8A-2`)
dat$`8X1`<-as.numeric(dat$`8X1`)
dat$`9B1`<-as.numeric(dat$`9B1`)
dat$`9B2`<-as.numeric(dat$`9B2`)
dat$`9X2`<-as.numeric(dat$`9X2`)
dat$P33G<-as.numeric(dat$P33G)

dat<-as.matrix(dat)
dat

class(dat$`10B2`)

clas
library(circlize)
col_fun = colorRamp2(c(-1, 0, 2), c("blue", "white", "red"))
col_fun
col_fun(seq(-3, 3))
library(RColorBrewer)
dat
tiff("heatmap_SPAF18_interactions.tiff",width = 4,height = 4,units="in",res = 400)
hm<-Heatmap(as.matrix(dat),cluster_rows = F,cluster_columns = F,rect_gp = gpar(col="black"),
            width = 4,height = 4,col=col_fun)
hm
dev.off()




