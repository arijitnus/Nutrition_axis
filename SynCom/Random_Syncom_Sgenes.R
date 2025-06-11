# Load required library
library(dplyr)
library(purrr)
library(tidyr)
library(combinat)
library(readxl)
# Assume:
# gene_df: rows are bacteria, columns are S-metabolic genes (values are 0/1 or counts)
# phylum_df: dataframe with columns "bacteria" and "phylum"

# Sample format of phylum_df:
# phylum_df <- data.frame(bacteria = rownames(gene_df), phylum = c("Proteobacteria", "Actinobacteria", ...))

gene_df<-read_excel('/Users/arijitm/Downloads/Genomes_info.xlsx',sheet = "one_replaced"
                    ,col_names = T,skip = 0)
metadata<-read_excel('/Users/arijitm/Downloads/Genomes_info.xlsx',sheet = "metdata"
                     ,col_names = T,skip = 0)
set.seed(123)  # for reproducibility

# Number of repetitions
n_iter <- 10000

# Prepare separate phylum-specific data
prot <- full_df %>% filter(Phylum == "Proteobacteria")
acti <- full_df %>% filter(Phylum == "Actinobacteria")
firm <- full_df %>% filter(Phylum == "Firmicutes")

# Select only gene columns
gene_cols <- setdiff(colnames(gene_df), "Bacteria")

# Function to generate one random community and count genes
sample_community <- function() {
  p_sample <- sample(prot$Bacteria, 8)
  a_sample <- sample(acti$Bacteria, 5)
  f_sample <- sample(firm$Bacteria, 5)
  
  all_bacs <- c(p_sample, a_sample, f_sample)
  
  sub_df <- gene_df %>% filter(Bacteria %in% all_bacs) %>% select(all_of(gene_cols))
  
  sum(colSums(sub_df) > 0)  # number of unique genes represented
}

# Repeat sampling
gene_counts <- replicate(n_iter, sample_community())

# Summary
summary(gene_counts)
tiff('N-genes_rand_SynComs.tiff', units="in", width=5, height=4, res=300, compression = 'lzw')
hist(gene_counts, breaks = 50, col = "skyblue", main = "",
     xlab = "Number of Unique S-Metabolic Genes",ylab = "Number of random SynComs")

dev.off()
# Plot distribution





