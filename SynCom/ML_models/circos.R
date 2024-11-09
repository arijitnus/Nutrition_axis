#install.packages("circlize")
library(circlize)

# Define sectors
sectors <- data.frame(
  sector = c("A", "B", "C", "D"),
  start = c(0, 0, 0, 0),
  end = c(1, 1, 1, 1)
)

# Define links (relationships between sectors)
links <- data.frame(
  from = c("A", "A", "B", "C"),
  to = c("B", "C", "D", "A"),
  value = c(0.3, 0.5, 0.4, 0.7)  # Controls ribbon width or strength
)


circos.clear()
circos.par(start.degree = 90, gap.degree = 5)

circos.initialize(factors = sectors$sector, xlim = c(0, 1))
circos.trackPlotRegion(factors = sectors$sector, ylim = c(0, 1), 
                       track.height = 0.1, panel.fun = function(x, y) {
                         sector.name <- get.cell.meta.data("sector.index")
                         circos.text(CELL_META$xcenter, CELL_META$ylim[2] + mm_y(5),
                                     sector.name, facing = "inside", niceFacing = TRUE)
                       })

for (i in 1:nrow(links)) {
  circos.link(sector.index1 = links$from[i], point1 = c(0, links$value[i]),
              sector.index2 = links$to[i], point2 = c(0, links$value[i]),
              col = "skyblue", border = "black")
}
