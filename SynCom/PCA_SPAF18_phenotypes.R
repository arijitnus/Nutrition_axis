library(readxl)
dat<-read_excel('/Users/arijitmukherjee/Downloads/PCA_SPAF18.xlsx',sheet = "Sheet1",
                col_names = T, skip = 0)

dat
library(factoextra)
library(FactoMineR)

dat<-as.data.frame(dat)
res.pca <- PCA(dat[,-c(6,7)], graph = FALSE)

cols<-c("#DC143C","#000000","#A0A0A0","#808080")


p<-fviz_pca_biplot(res.pca,
                col.ind = dat$type, palette = cols, 
                addEllipses = F, label = "var",
                col.var = "black", repel = TRUE,pointsize=4,labelsize=5
                )

# Customizing the output
tiff('PCA_SPAF18.tiff', units="in", width=8, height=8, res=300, compression = 'lzw')         # Paper size

# Creating a plot
p

# Closing the graphical device
dev.off() 


