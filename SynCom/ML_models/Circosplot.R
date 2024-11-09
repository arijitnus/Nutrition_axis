library(readxl)
df1<-read_excel('/Users/arijitmukherjee/Downloads/circos_in.xlsx',
                sheet = "Sheet1",col_names = T,skip = 0)

df2<-read_excel('/Users/arijitmukherjee/Downloads/circos_in.xlsx',
                sheet = "Sheet2",col_names = T,skip = 0)

# Merge df1 with df2 to get rel_inc for Strain1
df_merged <- merge(df1, df2, by.x = "Strain1", by.y = "Strain", suffixes = c("", "_Strain1"))

# Merge again to get rel_inc for Strain2
df_merged <- merge(df_merged, df2, by.x = "Strain2", by.y = "Strain", suffixes = c("", "_Strain2"))
df_merged


library(dplyr)

df_result <- df_merged %>%
  mutate(comparison = case_when(
    rel_inc > rel_inc_Strain1 & rel_inc > rel_inc_Strain2 ~ "greater than both",
    (rel_inc > rel_inc_Strain1 & rel_inc <= rel_inc_Strain2) | 
      (rel_inc <= rel_inc_Strain1 & rel_inc > rel_inc_Strain2) ~ "greater than one",
    rel_inc <= rel_inc_Strain1 & rel_inc <= rel_inc_Strain2 ~ "lower than both"
  ))

df_result


write.table(df_result,'compare_circos_in.tsv',sep = "\t")


library(circlize)
# Define colors for each comparison category
comparison_colors <- c("greater than both" = "darkgreen", 
                       "greater than one" = "cornflowerblue", 
                       "lower than both" = "brown3")

# Add a color column to df2_comparison based on the comparison value
df_result$color <- comparison_colors[df_result$comparison]

tiff("circos_plot.tiff", width = 6, height = 6, units = "in", res = 300)
# Initialize the Circos plot with each unique strain as a sector
# Initialize the Circos plot with each unique strain as a sector
circos.initialize(factors = unique(c(df_result$Strain1, df_result$Strain2)), xlim = c(0, 1))

# Add a track to represent each strain sector
circos.trackPlotRegion(factors = unique(c(df_result$Strain1, df_result$Strain2)), ylim = c(0, 1), 
                       panel.fun = function(x, y) {
                         sector.name <- get.cell.meta.data("sector.index")
                         circos.text(CELL_META$xcenter, CELL_META$ycenter, sector.name, facing = "clockwise", niceFacing = TRUE)
                       })

# Add wider ribbons between strain pairs with colors based on comparison
for (i in 1:nrow(df_result)) {
  circos.link(df_result$Strain1[i], 0.5, 
              df_result$Strain2[i], 0.5, 
              col = df_result$color[i], 
              border = "black", 
              lwd = 2)  # Set ribbon width (2 is wider; increase for more width)
}

# Clear Circos plot after use to reset parameters
circos.clear()
dev.off()

comparison_counts <- as.data.frame(table(df_result$comparison))
colnames(comparison_counts) <- c("comparison", "count")

#greater than both: 93, greater than one 33, less than both 27



library(ggplot2)
# Create the bar plot
p<-ggplot(comparison_counts, aes(x = comparison, y = count, fill = comparison)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "cornflowerblue", "brown3")) +  # Custom colors for each comparison type
  labs(title = "Count of Comparison Outcomes",
       x = "Comparison Outcome",
       y = "Count") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0)))

p
ggsave(
  "bar_dist_.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()
















