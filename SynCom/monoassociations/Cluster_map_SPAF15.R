library(readxl)
library(dplyr)
library(ggplot2)
library(ComplexHeatmap)

Dat<-read_excel("/Users/arijitmukherjee/Downloads/rel_change_hm.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
Dat
Dat_bm<-Dat%>%group_by(Strains)%>%summarise(avg_bm=mean(rel_bm))
Dat_bm
Dat_sa<-Dat%>%group_by(Strains)%>%summarise(avg_sa=mean(rel_sa))
Dat_rl<-Dat%>%group_by(Strains)%>%summarise(avg_rl=mean(rel_rl))
Dat_ra<-Dat%>%group_by(Strains)%>%summarise(avg_ra=mean(rel_ra))
df<-cbind(Dat_bm,Dat_sa,Dat_rl,Dat_ra)
df<-df[,-c(3,5,7)]
df
rownames(df)<-df$Strains
df<-df[,-1]
rownames(df)
df2<-df[-c(5,14,15),]
df2
hm2<-Heatmap(df2,row_dend_side = "right")
hm2
pdf("heatmap_rel_change_dendrogramSPAF15.pdf",         # File name
    width = 3, height = 8, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
hm2
dev.off()


#The order of the vector for all the strains based on their phenotypes:
vector<-c("P32B1","3C2","SPAF18","6A2","8X4",
          "10B2","P31D","9B1","7F21","6A1",
          "3C1","1A1","8X1","9B2",
          "8A2","P33G")

hist(as.matrix(df2))
df<-as.matrix(df2)
ranges <- c(0, 0.25, 0.5, 1, 1.5,2)
characters <- c("A","B","C","D","E")


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
colnames(result_matrix)<-c("Total biomass","Shoot area","Root length","Root network")
result_matrix
library(RColorBrewer)
YlOrBr <- brewer.pal(5,"YlOrBr")
library(ComplexHeatmap)
result_matrix<-result_matrix[vector,]
result_matrix
hm<-Heatmap(result_matrix,name = "hm", col = YlOrBr,show_column_names = T,rect_gp = gpar(col="black"),
            width = 4,height = 4) 
hm

pdf("heatmap_phenotypes_monoassociation_rel_changes_reordered_SPAF15.pdf",         # File name
    width = 3, height = 8, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
hm

# Closing the graphical device
dev.off() 






