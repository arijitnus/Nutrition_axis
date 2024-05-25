#New_heatmap for selected genes
library(ComplexHeatmap)
library(dplyr)
library(ggplot2)
library(readxl)
input<-read_excel("/Users/arijitmukherjee/Downloads/hm_metagenome_final.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
head(input)
input<-as.data.frame(input)
rownames(input)<-input$gene
input2<-input[,2:ncol(input)]
head(input2)
input2_scaled<-t(scale(t(input2)))

library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
hm<-Heatmap(as.matrix(input2_scaled),cluster_rows = F,cluster_columns = F,column_title = NA,
            column_split = c(rep("Col-0",1),rep("msa1-3",1),rep("sdi2;1",1)),rect_gp = gpar(col="black"),
            show_column_names = F,col=col_fun)
tiff("final_hm.tiff",units = "in",height = 5,width =5,res = 300)
hm
dev.off()
