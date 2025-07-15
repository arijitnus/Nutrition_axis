library(readxl)
df<-read_excel('/Users/arijitmukherjee/Downloads/hm_final_pred_ML.xlsx',sheet = "Sheet1",col_names = T,skip = 0)



# Load necessary libraries
library(dplyr)
library(tidyr)

# Sample input
# Get the unique strains (combining Strain_1 and Strain_2)
strains <- unique(c(df$Strain_1, df$Strain_2))

# Sort the strains to have a consistent order
strains <- sort(strains)

# Create an empty matrix to hold the predicted growth values (with NAs initially)
matrix <- matrix(NA, nrow = length(strains), ncol = length(strains), dimnames = list(strains, strains))

# Populate the lower triangle of the matrix
for (i in 1:nrow(df)) {
  strain_1 <- df$Strain_1[i]
  strain_2 <- df$Strain_2[i]
  growth_value <- df$pred_rel_inc[i]
  
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

# Print the lower triangular matrix
print(matrix)


write.table(matrix,'final_pred_rel_inc.tsv',sep = "\t")

#The above code converts the long dataframe into matrix for hm prep.

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Define the matrix (input data)
data <- matrix(c(
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.311788925, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.208481002, 0.510195022, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.09095511, 0.544919172, 0.486718792, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.328025848, 0.314378553, 0.212740787, 0.15678162, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.079500189, 0.544336993, 0.485554435, 0.553894236, 0.138340556, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.592044034, 0.333259621, 0.26692287, 0.117173725, 0.328025848, 0.107323851, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.257194105, 0.526910561, 0.43272603, 0.462591235, 0.26271579, 0.461426878, 0.298856998, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.257194105, 0.455181857, 0.448702669, 0.492039621, 0.26271579, 0.490875264, 0.298856998, 0.463510933, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.208481002, 0.510195022, 0.448702669, 0.486718792, 0.212740787, 0.485554435, 0.26692287, 0.43272603, 0.448702669, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  0.220854316, 0.337655936, 0.274074479, 0.126664619, 0.42593494, 0.117396924, 0.220854316, 0.308104766, 0.308104766, 0.274074479, NA, NA, NA, NA, NA, NA, NA, NA,
  0.06895171, 0.568676648, 0.492070055, 0.553894236, 0.127792077, 0.553894236, 0.098572069, 0.479599264, 0.509057677, 0.492070055, 0.108645142, NA, NA, NA, NA, NA, NA, NA,
  0.26471737, 0.516465168, 0.427830443, 0.443234361, 0.270239055, 0.442070004, 0.306380263, 0.434019145, 0.441285522, 0.427830443, 0.31562803, 0.457180895, NA, NA, NA, NA, NA, NA,
  0.328025848, 0.311125934, 0.209488167, 0.153246023, 0.310094229, 0.134804959, 0.410107227, 0.259463171, 0.259463171, 0.209488167, 0.309043508, 0.12425648, 0.266986435, NA, NA, NA, NA, NA,
  0.084455792, 0.568303324, 0.475730305, 0.553894236, 0.144448657, 0.553894236, 0.121016413, 0.462132016, 0.488542101, 0.475730305, 0.131206512, 0.539679637, 0.425006704, 0.140913061, NA, NA, NA, NA,
  0.263035197, 0.522485022, 0.426394072, 0.4391924, 0.268556882, 0.438028043, 0.30469809, 0.428070908, 0.439849151, 0.426394072, 0.313945857, 0.452053848, 0.441285522, 0.265304262, 0.419879657, NA, NA, NA,
  0.375698666, 0.483185422, 0.440261267, 0.474114516, 0.372730678, 0.473532338, 0.37984887, 0.455903668, 0.434974613, 0.440261267, 0.382038169, 0.493829176, 0.460869728, 0.371578179, 0.49309956, 0.462440847, NA, NA,
  0.387178595, 0.376545725, 0.442944501, 0.474572622, 0.384210606, 0.473990444, 0.3908936, 0.455903668, 0.434974613, 0.442944501, 0.393082899, 0.494287282, 0.463552963, 0.383058108, 0.493557666, 0.465124082, 0.376545725, NA
), ncol=18, byrow=TRUE)


# Convert to lower triangular matrix
data[upper.tri(data)] <- NA

# Melt the matrix into long format
data_melt <- melt(data, na.rm = TRUE)


# Plot the heatmap with YlOrBr color scale
p<-ggplot(data_melt, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "YlOrBr",direction=1,na.value = "grey") +
  theme_classic() +
  labs(title = "", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()
p

pdf("heatmap_predicted_comb_new.pdf",         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size

# Creating a plot
p

dev.off()


