library(readxl)
library(dplyr)
library(ggplot2)
dat<-read_excel('/Users/arijitmukherjee/Downloads/New_data.xlsx',
                sheet = "int_hist",col_names = T,skip = 0)

class(dat)
dat<-as.data.frame(dat)
class(dat$interaction_score)


hist(dat$interaction_score)
library("ggplot2")
my_groups <- rep("A", nrow(dat))   

my_groups[dat$interaction_score < -0.152] <- "Negative"
my_groups[dat$interaction_score > -0.152] <- "Neutral"
data_ggp <- data.frame(x = dat$interaction_score,                   # Create data frame with values & groups
                       my_groups = my_groups)

class(data_ggp)
data_ggp2<-na.omit(data_ggp)
data_ggp2
cols<-c("#003366","#C0C0C0")
ggp <- ggplot(data_ggp2, aes(x, fill = my_groups)) +  # Create ggplot2 histogram with default colors
  geom_histogram(color="black",alpha=0.5)+
  theme_classic()+
  scale_fill_manual(values=cols)+
  xlab("Interaction scores")+
  ylab("Outcome frequency")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 40), expand = expansion(mult = c(0, 0)))
ggp  

ggsave(
  "histogram_interaction_scores.tiff",
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
