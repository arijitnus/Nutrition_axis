# Load required libraries
library(tidyverse)
library(readxl)
# Load coculture and monoculture data
coculture <- read_excel('/Users/arijitmukherjee/Downloads/Combined_stats_biomass.xlsx',sheet = "Sheet1",col_names = T,skip = 0)   # Replace with actual file paths
monoculture <- read_excel('/Users/arijitmukherjee/Downloads/Combined_stats_biomass.xlsx',sheet="mono_data",
                        col_names = T,skip = 0)


coculture_groups <- coculture %>%
  group_by(strain1, strain2) %>%
  group_split()
monoculture_groups <- monoculture %>%
  group_by(Strains) %>%
  group_split()


# Function to test normality and select the appropriate statistical test
compare_strains <- function(coculture_data, monoculture_data) {
  # Extract rel_inc for each group
  coculture_rel_inc <- coculture_data$rel_inc
  monoculture_rel_inc <- monoculture_data$rel_inc
  
  # Check normality
  coculture_normal <- shapiro.test(coculture_rel_inc)$p.value > 0.05
  monoculture_normal <- shapiro.test(monoculture_rel_inc)$p.value > 0.05
  
  # Perform appropriate test
  if (coculture_normal & monoculture_normal) {
    # If both are normal, use t-test
    test_result <- t.test(coculture_rel_inc, monoculture_rel_inc, alternative = "two.sided")
  } else {
    # If not normal, use Wilcoxon rank-sum test
    test_result <- wilcox.test(coculture_rel_inc, monoculture_rel_inc, alternative = "two.sided")
  }
  
  return(test_result$p.value)  # Return p-value
}

# Initialize a results dataframe
results <- data.frame()

# Loop through all combinations
for (coculture_group in coculture_groups) {
  for (monoculture_group in monoculture_groups) {
    # Perform the test
    p_value <- compare_strains(coculture_group, monoculture_group)
    
    # Append results
    results <- rbind(results, data.frame(
      coculture_strain1 = unique(coculture_group$strain1),
      coculture_strain2 = unique(coculture_group$strain2),
      monoculture_strain = unique(monoculture_group$Strains),
      p_value = p_value
    ))
  }
}


results$p_adj<-p.adjust(results$p_value,method = 'fdr')

write.table(results,'results_co_vs_mono.tsv',sep = "\t")






