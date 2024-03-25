library(readxl)
library(dplyr)
library(ggplot2)
mono<-read_excel("/Users/arijitmukherjee/Downloads/mono.xlsx",sheet = "Sheet1",
                 col_names = T,skip = 0)
head(mono)

# Assuming your dataframe is called 'df'

# Load required libraries
library(dplyr)

# Assuming your dataframe is called 'df'

# Function to identify outliers based on z-score

# Assuming your dataframe is called 'df'
mono<-as.data.frame(mono)
# Function to identify outliers based on interquartile range (IQR)
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  is_outlier <- x < lower_bound | x > upper_bound
  return(is_outlier)
}

# Group by strain and identify outliers
df_with_outliers <-mono %>%
  group_by(Strains) %>%
  mutate(is_outlier = identify_outliers(rel_bm)) %>%
  ungroup()

# Check the dataframe with outlier identification
sum(df_with_outliers$is_outlier==TRUE)

write.table(df_with_outliers,'mono_outrem.tsv',sep = "\t")











































