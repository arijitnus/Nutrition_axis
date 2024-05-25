#combine two sheets of SynComs and MAGs sulfur genes into one
library(readxl)
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
mysheets <- read_excel_allsheets("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/Metagenome/New_metagenome/merged_KO_final.xlsx")
library(tidyverse)
merged_df<-reduce(mysheets,full_join,by="genes")
head(merged_df)#final dataframe of counts

#replace NA with 0 values, since those were not detected in those genomes
merged_df[is.na(merged_df)]=0
head(merged_df)
class(merged_df)#8150 rows
rownames(merged_df)<-merged_df$genes
#This dataframe contains some duplicate gene names, so need to remove them
#merged_df$duplicate<-duplicated(merged_df$product)
library(dplyr)
#merged_df_uniq<-filter(merged_df,duplicate==FALSE)
#rownames(merged_df_uniq)<-merged_df_uniq$product
#merged_df_uniq<-merged_df_uniq[,-c(1,67)]
class(merged_df)
merged_df<-merged_df[,-1]
merged_df$sum<-rowSums(merged_df)
merged_df<-merged_df%>%filter(sum!=0)
head(merged_df)

dim(merged_df)#144x 268
names(merged_df)
merged_df<-merged_df[,-268]

# we should now perform brayu curtis PcoA plot and permanova to see for differences
merged_df_norm<-merged_df/rowSums(merged_df)
head(merged_df_norm)
sum(colSums(merged_df_norm)==0)
#There coild be some columns with zero sums
column_sums <- colSums(merged_df_norm)

# Identify columns with sum equal to zero
columns_to_drop <- names(column_sums[column_sums == 0])

# Remove the identified columns from the dataframe
merged_df_norm <- merged_df_norm[, !(names(merged_df_norm) %in% columns_to_drop)]
abund_table<-as.matrix(merged_df_norm)
head(abund_table)
distance<-vegdist(t(abund_table),method = "bray")
sol_MDS<-cmdscale(distance,k=5,eig = T)
rownames(sol_MDS$points)
library(vegan)
#read in quality files for the MAGs
quality<-read_excel("/Users/arijitmukherjee/Downloads/MAG_quality.xlsx",sheet = "checkm_MAGs",col_names = T,skip = 0)
rownames(sol_MDS$points)

MDS.df<-data.frame(x=sol_MDS$points[,1],y=sol_MDS$points[,2],
                   type=as.factor(c(rep("MAG",249),rep("Syncom isolate",18))))
MDS.df$MAGS<-rownames(MDS.df)
#write.table(MDS.df,"MDS_df.tsv",sep = "\t")
head(quality)
MDS_merged<-merge(MDS.df,quality,by="MAGS")
MDS_merged

explainedvar1<-round(sol_MDS$eig[1]/sum(sol_MDS$eig),2)*100
explainedvar2<-round(sol_MDS$eig[2]/sum(sol_MDS$eig),2)*100
sum_var<-explainedvar1+explainedvar2
explainedvar1#9%
explainedvar2#5%
sum_var
library(wesanderson)
cols<-c("#A0A0A0","#3399FF","#000000","#CC0066")
pcoA1<-ggplot(MDS_merged,aes(x=x,y=y,col=quality))+
  geom_point(aes(size=type),alpha=0.6)+
  theme_classic()+
  xlab("PCo1 (13%)")+
  ylab("PCo2 (5%)")+
  scale_color_manual(values=cols)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14))+
  theme(legend.position = "top",legend.text = element_text(size = 12))+
  theme(legend.title = element_blank())+
  theme(legend.key.height = unit(0.8,'cm'))+
  theme(legend.key.width = unit(0.8,'cm'))
pcoA1
ggsave(
  "pcoa_sulf_final.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()

saveRDS(pcoA_genus,"PcoA_genus_final.rds")




