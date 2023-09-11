library(readxl)
library(ggplot2)
library(dplyr)
library(purrr)
excel_file <- "/Users/arijitmukherjee/Downloads/kofam_syncoms.xlsx"
sheet_names <- excel_sheets(excel_file)
list_of_dataframes <- list()
for (sheet_name in sheet_names) {
  df <- read_excel(excel_file, sheet = sheet_name)
  list_of_dataframes[[sheet_name]] <- df
}
count_tabs<-list()
for (i in 1:18) {
  count_tabs[[i]]=as.data.frame(table(list_of_dataframes[[i]]$Product))
  names(count_tabs[[i]])<-c("Gene",names(list_of_dataframes)[i])
}
count_tabs[[1]]

merged_df<-reduce(count_tabs,left_join,by="Gene")
merged_df

write.table(merged_df,"merged_kofam_syncoms.tsv",sep = "\t")








