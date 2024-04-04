library(readxl)
library(dplyr)
df<-read_excel("/Users/arijitmukherjee/Downloads/CMS+S_batch1_04042024.xlsx",
               sheet = "Sheet2",col_names = T,skip = 0)
df
nrow(df)
df$time<-seq.int(0.5,45,0.5)
df<-df[,-1]
df<-as.data.frame(df)
rownames(df)<-df$time
df
#set hte colnames of the df and calculate the average
# Select columns in groups of three
names(df)
df<-df[,-49]

col_groups <- split.default(df, (seq_along(df) - 1) %/% 3)
col_groups
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
#blank subtraction
averages<-averages[,-17]
for (i in 1:ncol(averages)) {
  averages[i]=averages[i]-averages[1]
}
averages<-averages[,-1]
averages$time<-seq.int(0.5,45,0.5)
averages$`10B2`
averages$time
write.table(averages,'average.tsv',sep = "\t")

df

p<-ggplot(df,aes(x=time,y=`P33G`))+
  geom_point()+
  xlim(0,49)+
  ylim(0,0.45)+
  theme_classic()+
  xlab("Time (hrs)")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.text.y.right = element_text(size = 14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  theme(legend.text = element_text(size=11))+
  scale_y_continuous(limits = c(0, 0.45), expand = expansion(mult = c(0, 0)))+
  scale_x_continuous(limits = c(0.5, 48), expand = expansion(mult = c(0, 0)))
p
df$`10B2+P33G`

df$`9X2`


#calculate growth params for all strains using these packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(growthcurver)
library(purrr)

growth.values.plate <- SummarizeGrowthByPlate(df)
growth.values.plate

write.table(growth.values.plate,'results.tsv',sep = "\t")



















