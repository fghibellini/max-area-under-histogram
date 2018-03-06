
library(ggplot2)

dir.create("plots", showWarnings = FALSE)

printGraph <- function(name, plot) {
  png(paste0("plots/", name, ".png"))
  print(plot)
  dev.off()
}

df0 <- read.csv("measurements.csv", header = FALSE)

p <- ggplot(df0, aes(V2, V3, group = V1, color = V1)) + geom_point() + geom_line()
printGraph("all", p)
