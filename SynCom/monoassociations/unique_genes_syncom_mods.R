library(readxl)
sulf<-read_excel("/Users/arijitmukherjee/Downloads/Sulfur_dist_syncoms.xlsx",sheet = "Finer_modules",col_names = T,skip = 0)
#Arrange the columns based on both finer and higher modules
sulf<-as.data.frame(sulf)
rownames(sulf)<-sulf$gene
sulf<-sulf[,-1]
sulf
order<-c("P32B1","3C2","6A2","8X4","10B2","P31D",
               "9B1","7F21","6A1",
               "3C1","1A1","8X1","9B2",
               "8A2","P33G","9X2","9F3","4C")

sulf_ord<-sulf[,order]
names(sulf_ord)

write.table(sulf_ord,"ordered_clusters_syncoms.tsv",sep = "\t")

#Now we have finer and higher mods collapsed data for all genes presence absence

finer_mods<-read_excel("/Users/arijitmukherjee/Downloads/Sulfur_dist_syncoms.xlsx",sheet = "finer_mods_dist",col_names = T,skip = 0)
head(finer_mods)
finer_mods<-as.data.frame(finer_mods)

rownames(finer_mods)<-finer_mods$genes
finer_mods<-finer_mods[,-1]

finer_mods
#replace values greater than 0 with 1
finer_mods[finer_mods>0]<-1

#Get the unique genes present in each column
unique_rows_df <- data.frame(
  Column = character(0),
  Unique_Rows = character(0)
)

# Loop through each column in the original dataframe
for (col_name in colnames(finer_mods)) {
  # Extract the column
  column_data <- finer_mods[[col_name]]
  
  # Initialize a vector to store unique rows
  unique_rows <- character(0)
  
  # Loop through each row in the column
  for (i in 1:nrow(finer_mods)) {
    # Check if the row is unique in the current column
    if (sum(finer_mods[i, ] == 1) == 1 && column_data[i] == 1) {
      unique_rows <- c(unique_rows, toString(i))
    }
  }
  
  # Add the results to the new dataframe
  unique_rows_df <- rbind(unique_rows_df, data.frame(Column = col_name, Unique_Rows = toString(unique_rows)))
}

# Print the dataframe with unique rows
unique_rows_df






higher_mods<-read_excel("/Users/arijitmukherjee/Downloads/Sulfur_dist_syncoms.xlsx",sheet = "higher_mods_dist",col_names = T,skip = 0)
head(higher_mods)
higher_mods<-as.data.frame(higher_mods)

rownames(higher_mods)<-higher_mods$genes
higher_mods<-higher_mods[,-1]

#replace values greater than 0 with 1
higher_mods[higher_mods>0]<-1

#Get the unique genes present in each column
unique_rows_df <- data.frame(
  Column = character(0),
  Unique_Rows = character(0)
)

# Loop through each column in the original dataframe
for (col_name in colnames(higher_mods)) {
  # Extract the column
  column_data <- higher_mods[[col_name]]
  
  # Initialize a vector to store unique rows
  unique_rows <- character(0)
  
  # Loop through each row in the column
  for (i in 1:nrow(finer_mods)) {
    # Check if the row is unique in the current column
    if (sum(finer_mods[i, ] == 1) == 1 && column_data[i] == 1) {
      unique_rows <- c(unique_rows, toString(i))
    }
  }
  
  # Add the results to the new dataframe
  unique_rows_df <- rbind(unique_rows_df, data.frame(Column = col_name, Unique_Rows = toString(unique_rows)))
}

# Print the dataframe with unique rows
unique_rows_df

rownames(sulf_ord)











































