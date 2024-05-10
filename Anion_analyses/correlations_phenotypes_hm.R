# install and load the plotly package
library(plotly)
library(ggcorrplot)
library(readxl)
library(ComplexHeatmap)
dat<-read_excel("/Users/arijitmukherjee/Downloads/correlation.xlsx",sheet="Sheet2",col_names=T,skip=0)
#plot the heatmaps based on range of values from ES
#we shall create five distinct characters for the range of values in matrix
dat
dat<-as.data.frame(dat)
rownames(dat)<-dat$corr
dat<-dat[,-1]
dat
dat<-as.matrix(dat)
library(corrplot)

tiff(filename="correlation_hm_phenotypes.tiff", width=3000, height=3000, res=300)         # Paper size

corrplot(as.matrix(dat), method = "color",type = 'lower',cl.cex = 1.5)
# Closing the graphical device
dev.off() 








