library(readxl)
dat<-read_excel("/Users/arijitmukherjee/Downloads/input.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dat<-as.data.frame(dat)
library(dplyr)
df<-dat%>%group_by(Strains)%>%summarise(avg_bm=mean(rel_bm),
                                    avg_sa=mean(rel_sa),
                                    avg_rl=mean(rel_rl),
                                    avg_lr=mean(rel_rl),
                                    avg_ra=mean(rel_ra))%>%data.frame()

rownames(df)<-df$Strains
df<-df[,-1]
df

library(factoextra)
library(ggfortify)
df$group<-c("R","NR","NR","R","NR","R","R","R","NR","NR","R","R","NR","NR","NR",
            "R","R","NR","R")
names(df)<-c("Biomass","Shoot area","Root length","Avg. Lateral roots","Root network","group")

df_pca<-prcomp(df[,-6],scale. = T,center = T)
p<-autoplot(df_pca,data = df,main="Biplot",loadings=TRUE,
         loadings.label=TRUE,colour="group",
         loadings.colour = "black",
         loadings.label.colour="black",
         loadings.label.repel=TRUE)+
  xlab("PC1 (75.64%)")+ylab("PC2 (18.36%)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())
p
ggsave(
  "biplot_phenos.tiff",
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









