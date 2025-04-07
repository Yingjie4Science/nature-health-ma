
library(ggplot2) 
library(grid)
library(RColorBrewer)

make_gradient_bg <- function(deg = 45, n = 100, cols = blues9, col_rev = T, alpha = 0.5) {
  cols <- colorRampPalette(cols)(n + 1)
  cols <- scales::alpha(cols, alpha)
  
  if (col_rev == T) {
    cols <- rev(cols)
  }
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat, 
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}


## test ----------------------------------------------------------------------------------
# g <- make_gradient(deg = 180, n = 500, cols = brewer.pal(9, "RdBu"))
# 
# p1 + annotation_custom(grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 


##' ref ----------------------------------------------------------------------------------
##' https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient

