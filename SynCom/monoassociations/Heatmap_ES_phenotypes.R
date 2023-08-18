library(readxl)
dat<-read_excel("/Users/arijitmukherjee/Downloads/hm_plot.xlsx",sheet="Sheet1",col_names=T,skip=0)
#plot the heatmaps based on range of values from ES
#we shall create five distinct characters for the range of values in matrix
dat
dat<-as.data.frame(dat)
rownames(dat)<-dat$Strain
dat<-dat[,-1]
dat
dat<-as.matrix(dat)
# Define ranges and corresponding characters
ranges <- c(-1, 0, 2, 4, 6, 20)
characters <- c("A","B","C","D","E","F")

# Replace matrix values with characters based on ranges
result_matrix <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
for (i in 1:nrow(dat)) {
  for (j in 1:ncol(dat)) {
    value <- dat[i, j]
    for (k in 1:(length(ranges) - 1)) {
      if (value >= ranges[k] && value <= ranges[k + 1]) {
        result_matrix[i, j] <- characters[k]
        break
      }
    }
  }
}
result_matrix
rownames(result_matrix)<-rownames(dat)
colnames(result_matrix)<-c("Total biomass","Shoot area","Root length")
result_matrix
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
library(ComplexHeatmap)
hm<-Heatmap(result_matrix,name = "hm", col = YlOrBr,show_column_names = T,rect_gp = gpar(col="black"),
            width = 4,height = 4) 
hm

# Customizing the output
pdf("heatmap_phenotypes_monoassociation.pdf",         # File name
    width = 3, height = 8, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
hm

# Closing the graphical device
dev.off() 














