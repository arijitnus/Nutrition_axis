library(readxl)
library(ggplot2)
library(ComplexHeatmap)
library(RColorBrewer)
dat<-read_excel("/Users/arijitmukherjee/Downloads/correlation.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dat
shapiro.test(dat$`Nitrate (uM/mg DM)`))
shapiro.test((dat$`Phosphate (uM/mg DM)`))
shapiro.test((dat$`Sulphate (uM/mgDM)`))
shapiro.test(dat$leaf)#Normal distribution
#even when we log transform or sqrt transform they are not normal 

# we need to perform spearman correlation among the phenotypes that we measured
a<-cor.test(dat$`Nitrate (uM/mg DM)`,dat$leaf,method = "spearman")#r=-0.2,p=0.07
b<-cor.test(dat$`Phosphate (uM/mg DM)`,dat$leaf,method = "spearman")#r=-0.1,p=0.36
c<-cor.test(dat$`Sulphate (uM/mgDM)`,dat$leaf,method = "spearman")#r=0.23,p=0.03
d<-cor.test(dat$`Sulphate (uM/mgDM)`,dat$`Phosphate (uM/mg DM)`,method = "spearman")#r=0.57,p=<0.05
e<-cor.test(dat$`Nitrate (uM/mg DM)`,dat$`Phosphate (uM/mg DM)`,method = "spearman")#r=-0.29; p=0.01
f<-cor.test(dat$`Nitrate (uM/mg DM)`,dat$`Sulphate (uM/mgDM)`,method = "spearman")#r=-0.4,p=0.002


pairs(dat)

#plot the correlation coefficients in heatmap with stats

library(readxl)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
dat<-read_excel("/Users/arijitmukherjee/Downloads/correlation.xlsx",sheet="Sheet2",col_names=T,skip=0)
#plot the heatmaps based on range of values from ES
#we shall create five distinct characters for the range of values in matrix
dat
dat<-as.data.frame(dat)
rownames(dat)<-dat$corr
dat<-dat[,-1]
dat
dat<-as.matrix(dat)
col_fun<-colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
hm<-Heatmap(dat, cluster_rows = F,cluster_columns = F,col = col_fun,rect_gp = gpar(col="black"),
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%.2f", dat[i, j]), x, y, gp = gpar(fontsize = 24))
            })
hm

tiff(filename="correlation_hm_phenotypes.tiff", width=3000, height=3000, res=300)         # Paper size

# Creating a plot
hm

# Closing the graphical device
dev.off() 








