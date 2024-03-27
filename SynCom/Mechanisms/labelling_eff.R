#plot bacterial efficiency
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/plot_bac_eff.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dat

# Load required library
library(ggplot2)

# Data
data <- data.frame(
  Media = c('32S', '32S', '32S', '32S', '32S', '32S', '34S', '34S', '34S', '34S', '34S', '34S'),
  values = c(0.957753678, 0.959085991, 0.959984285, 0.042246322, 0.040914009, 0.040015715, 
             0.017671246, 0.015830312, 0.015983565, 0.982328754, 0.984169688, 0.984016435),
  Isotope = c('32S', '32S', '32S', '34S', '34S', '34S', '32S', '32S', '32S', '34S', '34S', '34S')
)

# Calculate mean and standard error
summary <- aggregate(values ~ Media + Isotope, data = data, FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))
summary<-read_excel("/Users/arijitmukherjee/Downloads/plot_bac_eff.xlsx",sheet = "Sheet2",col_names = T,skip = 0)
# Plot
# merge dataframes and compute limits for sd
pdata<-summary%>%
  group_by(Molecule, Media) %>%            # group data for limit calculation
  mutate(upper = mean(Efficiency) + sd(Efficiency), # upper limit for error bar
         lower = mean(Efficiency) - sd(Efficiency)) # lower limit for error bar

pdata
cols<-c("")
# plot


p<-ggplot(pdata, aes(x = Molecule, y = Efficiency, fill = Media )) +
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9))+
  theme_classic()+
  xlab("")+
  ylab("34-S incorporation efficiency")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0)))

p
ggsave(
  "labelling_eff.tiff",
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















