rds_files <- list.files(pattern = "\\.RDS$")
rds_files

data_list <- list()


library(BacArena)
for (file_name in rds_files) {
  # Read the .RDS file
  p <- readRDS(file_name)
  
  # Assuming plotGrowthCurve and q$data are correct based on your code
  data <- plotGrowthCurve(p)[[1]]$data
  # Store the data with the specified name
  data_name<-paste(unique(data$species),collapse = "_")
  assign(data_name, data)
  
  # Add the data to the list
  data_list[[data_name]] <- data
}

length(data_list)

#install.packages("tidyverse")
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



saveRDS(result_list,'growth_params_coculture_revised.RDS')

library(dplyr)
library(tidyr)


merged_data <- bind_rows(result_list, .id = "source_df")
head(merged_data)

write.table(merged_data,'coculture_all_data_revised.tsv',sep = "\t")

monoculture_growth_rate <- read.table(text = "sample rm
9B2 0.062327497
8A2 0.063320915
P31D 0.061099734
1A1 0.063703028
9E2 0.060168064
P33G 0.062831163
P32B1 0.063644073
7F21 0.06239739
4C 0.063386829
6A2 0.065349985
3C2 0.05628814
9B1 0.060958802
8E1 0.063294396
9F3 0.061326381
3C1 0.062540534
8E4 0.059749213
6A1 0.061891583
10B2 0.059540601", header = TRUE, stringsAsFactors = FALSE)

merged_data2 <- merged_data %>%
  left_join(monoculture_growth_rate, by = "sample") %>%
  mutate(interaction_score =log2(r/rm))

write.table(merged_data2,'interaction_scores.tsv',sep = "\t")

merged_data2



