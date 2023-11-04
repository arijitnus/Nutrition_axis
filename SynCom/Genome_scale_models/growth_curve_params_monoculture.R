# set the directory
# List all bacteria names
library(BacArena)
library(sybil)
library(ggplot2)
library(dplyr)
bacteria_names <- c("Atlanti", "Achromo", "Agromyces","Bacillus",
                    "Bhargavaea","Brevundimonas","Cellulosimicrobium",
                    "Corynebacterium","Lysinibacillus","Mycoplana",
                    "Paenibacillus","Priestia","Pseudomonas",
                    "Pseudomonas_guariconensis","Pusillimonas",
                    "Stenotrophomonas","Streptomyces","Streptomyces_griseofuscus")  # Add all 18 bacteria names

# Initialize an empty list to store dataframes
all_data <- list()

# Loop through each bacteria and replicate
for (bacteria in bacteria_names) {
  for (replicate in 1:3) {
    # Create the file name
    file_name <- paste0(bacteria, "_25h_", replicate, ".RDS")
    
    # Read the .RDS file
    p <- readRDS(file_name)
    q <- plotGrowthCurve(p)
    data<-q$data
    # Extract the data
    data$bacteria <- bacteria
    data$replicate <- replicate
    # Append the dataframe to the list
    all_data[[length(all_data) + 1]] <- data
  }
}

# Merge all dataframes into one
final_dataframe <- do.call(rbind, all_data)
write.table(final_dataframe,'final_df.tsv',sep = "\t")

# Print or use the final_dataframe as needed
print(final_dataframe)
dim(final_dataframe)

averaged_df <- final_dataframe %>%
  group_by(bacteria,time) %>%
  summarise(avg_value = mean(value))
averaged_df

write.table(averaged_df,"averaged_df.tsv",sep = "\t")
#calculate growth params for all strains using these packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(growthcurver)
library(purrr)
library(readxl)
library(tidyr)
spread_avg_df<-spread(averaged_df,key = bacteria,value = avg_value)
spread_avg_df
spread_avg_df$Time<-spread_avg_df$time*60
names(spread_avg_df)
spread_avg_df<-spread_avg_df[,-1]

#filter the data for 30hrs=1800 mins for logistic growth curve This is based on 
#information of plotting the data previously

growth.values.plate <- SummarizeGrowthByPlate(spread_avg_df)
growth.values.plate
write.table(growth.values.plate,"growth-values-plate.tsv",sep = "\t")














