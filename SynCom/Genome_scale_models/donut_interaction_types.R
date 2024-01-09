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




