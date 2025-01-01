library(readxl)
df<-read_excel('/Users/arijitmukherjee/Downloads/traingle_hm_pairs.xlsx',sheet = "Sheet1",
               col_names = T,skip = 0)

library(dplyr)
library(tidyr)

# Sample input
# Get the unique strains (combining Strain_1 and Strain_2)
strains <- unique(c(df$Strain1, df$Strain2))
# Sort the strains to have a consistent order
strains <- sort(strains)

# Create an empty matrix to hold the predicted growth values (with NAs initially)
matrix <- matrix(NA, nrow = length(strains), ncol = length(strains), dimnames = list(strains, strains))

# Populate the lower triangle of the matrix
for (i in 1:nrow(df)) {
  strain_1 <- df$Strain1[i]
  strain_2 <- df$Strain2[i]
  growth_value <- df$comparison[i]
  
  # Find the row and column indices for the strains
  row_index <- match(strain_1, strains)
  col_index <- match(strain_2, strains)
  
  # Place the growth value in the lower triangle (i.e., row > col)
  if (row_index > col_index) {
    matrix[row_index, col_index] <- growth_value
  } else if (row_index < col_index) {
    matrix[col_index, row_index] <- growth_value
  }
}

print(matrix)

write.table(matrix, 'additive_pairs.tsv',sep = "\t")


library(ggplot2)
library(reshape2)
data<-matrix

# Convert to lower triangular matrix
data[upper.tri(data)] <- NA

# Melt the matrix into long format
data_melt <- melt(data, na.rm = TRUE)


color_map <- c("additive" = "darkgreen", 
               "semi additive" = "cornflowerblue", 
               "non additive" = "brown3")
# Plot the heatmap with YlOrBr color scale
p<-ggplot(data_melt, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = color_map) +
  theme_classic() +
  labs(title = "", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()
p
pdf("heatmap_predicted_additivity.pdf",         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
p

dev.off()














