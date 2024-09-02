library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

df<-read_excel('/Users/arijitmukherjee/Documents/Sulfur_manuscript_II/Figures_tiff/GSMM/hm_interactions.xlsx',
               sheet = "Sheet1",col_names = T,skip = 0)
df



# Assuming your dataframe is named `df`

# Extract unique bacteria strains
bacteria <- unique(df$sample)

# Initialize an empty matrix
interaction_matrix <- matrix(NA, nrow = length(bacteria), ncol = length(bacteria))
rownames(interaction_matrix) <- bacteria
colnames(interaction_matrix) <- bacteria

# Loop over the dataframe by unique `source_df`
for(result in unique(df$source_df)) {
  # Get the indices of the current result
  idx <- which(df$source_df == result)
  
  if(length(idx) == 2) {  # Ensure there are exactly two bacteria per result
    strain1 <- df$sample[idx[1]]
    strain2 <- df$sample[idx[2]]
    
    # Upper triangle (interaction from strain1 to strain2)
    interaction_matrix[strain1, strain2] <- df$interaction_score[idx[1]]
    
    # Lower triangle (interaction from strain2 to strain1)
    interaction_matrix[strain2, strain1] <- df$interaction_score[idx[2]]
  }
}

# Print the resulting matrix
print(interaction_matrix)

#writhethe matrix

write.table(interaction_matrix,'interaction_matrix.tsv',sep = "\t")





