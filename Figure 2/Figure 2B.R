Fisher's exact test for pathway enrichment: Adapted from # Gaia Cortinovis
#https://www.researchgate.net/post/How_can_I_perform_many_of_Fishers_exact_tests_for_a_big_number_of_genes_with_R
#Read in the freq dataframe for the dysregulated genes and then perform Fisher's test
library(readxl)
Data<-read_excel("/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/Metagenome/COG.xlsx",sheet = "msa_sdi_Fisher_test",col_names = T,skip = 0)
head(Data)

#if(!require(psych)){install.packages("psych")}
library(psych)
library(dplyr)

str(Data)
Data$Fisher.p   = NA
Data$Odds.ratio = NA
Data$phi        = NA
head(Data)

for(i in 1:length(Data$category)){ ## from first to last Gene
  Data$Fisher.p[i] = fisher.test(matrix(c(Data$count_MSA[i],Data$MSA_total[i],Data$count_total[i],Data$total_list[i]), nrow=2))$p.value
  Data$Odds.ratio[i] = fisher.test(matrix(c(Data$count_MSA[i],Data$MSA_total[i],Data$count_total[i],Data$total_list[i]), nrow=2))$estimate
  Data$phi[i] = phi(matrix(c(Data$count_MSA[i],Data$MSA_total[i],Data$count_total[i],Data$total_list[i]), nrow=2))
}
Data
Data$adjP<-p.adjust(Data$Fisher.p,method = "fdr")
Data_sig<-Data%>%filter(adjP<0.1)
#write.table(Data,'Fisher_test_msasdi.tsv',sep = "\t")
# plot side bar plot for the COG terms enrichment
library(ggplot2)
library(dplyr)
Data_sig$proportion<-Data_sig$count_MSA/396
write.table(Data_sig,"sig_data.tsv",sep = "\t")

Data_sig<-Data_sig[-c(9,10),]# remove general functions and functions unknown


q<-ggplot(data=Data_sig, aes(x=reorder(description,proportion), y=proportion))+
  geom_bar(stat="identity", width=0.7,fill="steelblue")+
  theme_classic()+
  xlab("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14,vjust = -0.2),
        axis.line = element_line(colour="black", size = 0.8))+
  scale_y_continuous(limits = c(0, 0.2), expand = expansion(mult = c(0, 0)))
r<-q+coord_flip()
r
ggsave(
  "msasdi_COG_enrichment.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 7.5,
  units = "in",
  dpi = 400,
)
dev.off()



