library(readxl)
library(dplyr)
library(ggplot2)

Dat<-read_excel("/Users/arijitmukherjee/Downloads/rel_change_hm.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
Dat
Dat_bm<-Dat%>%group_by(Strains)%>%summarise(avg_bm=mean(rel_bm))
Dat_bm
Dat_sa<-Dat%>%group_by(Strains)%>%summarise(avg_sa=mean(rel_sa))
Dat_rl<-Dat%>%group_by(Strains)%>%summarise(avg_rl=mean(rel_rl))
df<-cbind(Dat_bm,Dat_sa,Dat_rl)
df<-df[,-c(3,5)]
df

class(df)
rownames(df)<-df$Strains
df<-df[,-1]
hist(as.matrix(df))
df<-as.matrix(df)
ranges <- c(-0.1, 0, 0.25, 0.5, 1, 1.5,2)
characters <- c("A","B","C","D","E","F")


# Replace matrix values with characters based on ranges
result_matrix <- matrix(NA, nrow = nrow(df), ncol = ncol(df))
for (i in 1:nrow(df)) {
  for (j in 1:ncol(df)) {
    value <- df[i, j]
    for (k in 1:(length(ranges) - 1)) {
      if (value >= ranges[k] && value <= ranges[k + 1]) {
        result_matrix[i, j] <- characters[k]
        break
      }
    }
  }
}
result_matrix

rownames(result_matrix)<-rownames(df)
colnames(result_matrix)<-c("Total biomass","Shoot area","Root length")
result_matrix
YlOrBr <- brewer.pal(6,"YlOrBr")
library(ComplexHeatmap)
library(RColorBrewer)
hm<-Heatmap(result_matrix,name = "hm", col = YlOrBr,show_column_names = T,rect_gp = gpar(col="black"),
            width = 4,height = 4) 
hm

pdf("heatmap_phenotypes_monoassociation_rel_changes.pdf",         # File name
    width = 3, height = 8, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
hm

# Closing the graphical device
dev.off() 
















































