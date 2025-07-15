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







## my data
# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("Amensal", "Commensal", "Mutualistic","Competitive","Neutral","Parasitic"),
  count=c(1,14,88,3,1,9)
)

levels=c("Mutualistic","Commensal","Neutral","Parasitic","Competitive","Amensal")
data$category<-as.factor(data$category)
data$category<-factor(data$category,levels=levels)
# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))
cols<-c("#0066CC","#3399FF","#99CCFF","#FF8000","#FFB266","#FFE5CC")

# Make the plot
p<-ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  scale_color_manual(values=cols)+
  scale_fill_manual(values = cols)+
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(1, 4))+
  theme_void()
p
ggsave(
  "donut_ecological_interactions.tiff",
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



