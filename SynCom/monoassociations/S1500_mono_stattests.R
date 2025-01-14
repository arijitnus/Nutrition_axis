library(readxl)
library(dplyr)
dat2<-read_excel('/Users/arijitmukherjee/Downloads/mono_stats.xlsx',sheet = "Sheet2",col_names = T,skip = 0)

shapiro.test(dat1$avg_biomass)#Normal distribution


# Filter data for HK
hk_data <- dat2[dat2$bacteria == "HK", "avg_biomass"]

# Get unique bacteria names excluding HK
other_bacteria <- unique(dat2$bacteria[dat2$bacteria != "HK"])

# Perform t-tests
results <- lapply(other_bacteria, function(bac) {
  # Filter data for the current bacterium
  bac_data <- dat2[dat2$bacteria == bac, "avg_biomass"]
  
  # Perform t-test
  t_test_result <- t.test(hk_data, bac_data)
  
  # Return a summary of the results
  list(
    bacterium = bac,
    p_value = t_test_result$p.value,
    statistic = t_test_result$statistic,
    mean_difference = mean(hk_data) - mean(bac_data)
  )
})

# Convert results to a data frame for easier viewing
results_df <- do.call(rbind, lapply(results, as.data.frame))
print(results_df)

results_df$p_adj<-p.adjust(results_df$p_value,method = 'fdr')

write.table(results_df,'p_vals_S1500_mono_batch2.tsv',sep = "\t")



















































