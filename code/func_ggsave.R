
library(ggplot2)

func_ggsave <- function(fname, w = 7, h = 4, dpi = 300, save_png = save_plot) {
  if (save_png == T) {
    ggsave(filename = fname, plot = last_plot(), width = w, height = h, units = "in", dpi = dpi)
  } else {
    print('The plot will not be saved.')
  }
}