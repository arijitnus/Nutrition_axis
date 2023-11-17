# Get a list of all .RDS files in the directory
rds_files <- list.files(pattern = "\\.RDS$")
rds_files
# Create an empty list to store data
data_list <- list()

library(BacArena)
for (file_name in rds_files) {
  # Extract the first two texts separated by '_'
  file_parts <- strsplit(gsub("\\.RDS$", "", file_name), "_")[[1]]
  data_name <- paste(file_parts[1], file_parts[2], sep = "_")
  
  # Read the .RDS file
  p <- readRDS(file_name)
  
  # Assuming plotGrowthCurve and q$data are correct based on your code
  data <- plotGrowthCurve(p)[[1]]$data
  # Store the data with the specified name
  assign(data_name, data)
  
  # Add the data to the list
  data_list[[data_name]] <- data
}

length(data_list)

# Assuming you have the tidyverse and growthcurver packages installed
library(tidyverse)
library(growthcurver)
result_list<-list()
# Loop through each dataframe in the list
for (i in seq_along(data_list)) {
  # Spread the dataframe
  spread_df <- spread(data_list[[i]], key = species, value = value)
  spread_df$Time <- spread_df$time * 60
  spread_df <- spread_df[, -c(1, 2)]
  
  # Summarize growth by plate
  growth.values.plate <- SummarizeGrowthByPlate(spread_df)
  
  # Store the result in the list
  result_list[[paste("result_", i, sep = "")]] <- growth.values.plate
}

result_list
saveRDS(result_list,'growth_params_coculture.RDS')
#result_list<-readRDS('growth_params_coculture.RDS')


# Your list of coculture dataframes (result_list from the previous code)
# Replace this with the actual list of coculture dataframes
list_of_coculture_data <- result_list  

library(dplyr)
library(tidyr)
merged_data <- bind_rows(result_list, .id = "source_df")
head(merged_data)

write.table(merged_data,'coculture_all_data.tsv',sep = "\t")


# Your monoculture growth rate dataframe
monoculture_growth_rate <- read.table(text = "sample rm
9B2 0.007117046
8A2 0.00696666
P31D 0.006551774
1A1 0.006451706
9E2 0.006334414
P33G 0.006255983
P32B1 0.00577679
7F21 0.005727214
4C 0.005650374
6A2 0.005609764
3C2 0.005599487
9B1 0.005534226
8E1 0.00543748
9F3 0.005132645
3C1 0.005014225
8E4 0.004810937
6A1 0.004509544
10B2 0.003572014", header = TRUE, stringsAsFactors = FALSE)


library(dplyr)

# Assuming your main dataframe is named 'main_df'
merged_data2 <- merged_data %>%
  left_join(monoculture_growth_rate, by = "sample") %>%
  mutate(interaction_score =log2(r/rm))
merged_data2


write.table(merged_data2,'interaction_scores.tsv',sep = "\t")








