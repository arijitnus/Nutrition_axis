library(readxl)
library(dplyr)
df<-read_excel("/Users/arijitmukherjee/Downloads/gr_S0.xlsx",
               sheet = "Batch3",col_names = T,skip = 0)
df
df$time<-seq(0,47.5,0.5)
nrow(df)

df<-as.data.frame(df)
rownames(df)<-df$time
names(df)
df<-df[,-41]
#set hte colnames of the df and calculate the average
# Select columns in groups of three
names(df)
df <- df %>%
  mutate(across(everything(), ~ . - blank))
names(df)

df<-df[,-10]

col_groups <- split.default(df, (seq_along(df) - 1) %/% 3)

plot(col_groups$`0`)
plot(col_groups$`7`$P32B1_3)#take 3rd rep







averages<-sapply(col_groups, rowMeans)
averages
# Extract column names from each group and remove underscore
# Extract column names from each group and remove anything after underscore
col_names <- sapply(col_groups, function(group) {
  names_without_suffix <- gsub("_.*$", "", names(group))
})

col_names<-col_names[-c(2,3),]
col_names

class(col_names)

colnames(averages)<-col_names
class(averages)

averages<-as.data.frame(averages)
averages$time<-rownames(averages)

averages

#calculate growth params for all strains using these packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(growthcurver)
library(purrr)
averages$time<-as.numeric(averages$time)
P32B1<-data.frame(growth=col_groups$`7`$P32B1_2,time=seq(0,47.5,0.5))


growth.values.plate <- SummarizeGrowthByPlate(P32B1)
growth.values.plate

write.table(growth.values.plate,'batch3.tsv',sep = "\t")


















