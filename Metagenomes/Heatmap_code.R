library(ComplexHeatmap)
library(dplyr)
library(ggplot2)
library(readxl)
input<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/Metagenome/New_metagenome/MAGs_sulfur_genes_input_HM.xlsx",sheet = "input",col_names = T,skip = 0)
head(input)
input<-as.data.frame(input)
rownames(input)<-input$genes
input2<-input[,4:ncol(input)]
head(input2)
input2 <- input2 %>% mutate(across(everything(), ~ifelse(. > 1, 1, .)))
qual_genomes<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/Metagenome/New_metagenome/MAGs_sulfur_genes_input_HM.xlsx",sheet = "quality_genomes",col_names = T,skip = 0)
head(qual_genomes)
qual_genomes$high_qual_MAGs
input2<-input2%>%select(all_of(qual_genomes$high_qual_MAGs))
dim(input2)

###Now section the heatmap based on the functional modules and also phylogenetic information of the MAGs
library(circlize)
col_fun<-colorRamp2(c(0,1),c("white","black"))
names(input2) <- gsub("_output", "", names(input2))#remove _output from the column names
#check rownames are identical to the initial input dataframe
rownames(input2)==input$genes
row_anno<-data.frame(input$module)
ha=rowAnnotation(category=c(rep("Cys/Met metabolism",65),rep("Glutathione metabolism",25),rep("Sulfur metabolism",73)
                            ,rep("Taurine metabolism",16)))
ha2=columnAnnotation(Phyla=c(rep("Acidobacteria",6),rep("Actinomycetes",2),rep("Bacteroidetes",21),
                             rep("Chloroflexi",4),rep("Firmicutes",1),rep("Gemmatimonadota",4),
                             rep("Myxococcota",2),rep("Others",7),rep("Patescibacteria",6),
                             rep("Proteobacteria",27),rep("Verrucomicrobiota",4)))

#reorder the columns based on the phyla ordered information
col_ord<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/Metagenome/New_metagenome/MAGs_sulfur_genes_input_HM.xlsx",sheet = "column_order",col_names = T,skip = 0)
head(col_ord)

names(input2)
write.table(names(input2),"MAG_names.tsv",sep = "\t")

class(input2)
input3<-input2[,col_ord$MAGs]


hm<-Heatmap(as.matrix(input3),col=col_fun,cluster_rows = F,cluster_columns = F,row_names_gp = gpar(fontsize=4),
        column_names_gp = gpar(fontsize=4),
        row_split = c(rep("Cys/Met metabolism",65),rep("Glutathione metabolism",25),rep("Sulfur metabolism",73),
                      rep("Taurine metabolism",16)),
        column_split = c(rep("Acidobacteria",6),rep("Actinomycetes",2),rep("Bacteroidetes",21),
                         rep("Chloroflexi",4),rep("Firmicutes",1),rep("Gemmatimonadota",4),
                         rep("Myxococcota",2),rep("Others",7),rep("Patescibacteria",6),
                         rep("Proteobacteria",27),rep("Verrucomicrobiota",4)),
        left_annotation = ha,
        top_annotation = ha2)
hm

ggsave(
  "Heatmap_MAGs_sulfur_genes.tiff",
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




















