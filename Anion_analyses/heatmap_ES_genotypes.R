library(readxl)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
dat<-read_excel("/Users/arijitmukherjee/Downloads/anion_plots.xlsx",sheet="effsize",col_names=T,skip=0)
#plot the heatmaps based on range of values from ES
#we shall create five distinct characters for the range of values in matrix
dat
dat<-as.data.frame(dat)
rownames(dat)<-dat$Genotype
dat<-dat[,-1]
dat
dat<-as.matrix(dat)

hm<-Heatmap(dat, cluster_rows = F,cluster_columns = F,col = col_fun,rect_gp = gpar(col="black"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", dat[i, j]), x, y, gp = gpar(fontsize = 24))
        })
hm

tiff(filename="test.tiff", width=3800, height=2000, res=300)         # Paper size

# Creating a plot
hm

# Closing the graphical device
dev.off() 













