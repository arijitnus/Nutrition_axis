library(readxl)
library(ComplexHeatmap)
library(circlize)
df<-read_excel('/Users/arijitmukherjee/Downloads/Predicted_growth.xlsx',sheet = 'Sheet3',
               col_names = T,skip = 0)
df<-as.data.frame(df)


library(RColorBrewer)

col1 = colorRamp2(c(0.15, 0.8), c("Yellow", "brown"))
rownames(df)<-df$Bac1Bac2
df<-df[,-1]
df
YlOrBr <- brewer.pal(16,"YlOrBr")

p<-Heatmap(as.matrix(df), rect_gp = gpar(type = "black"), cluster_columns = F,cluster_rows = F,col=YlOrBr,
           show_column_names = T,show_row_names = T,na_col = 'white',row_names_side = 'left',width = 4,height = 4,
           cell_fun = function(j, i, x, y, w, h, fill) {
             if(i <= j) {
               grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
             }
        })
p

pdf("heatmap_predicted_comb_growth.pdf",         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
p
dev.off()


