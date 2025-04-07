
library(ggalluvial)
library(ggrepel)

#' @param indicator_n_min the minimum number of flow frequency to be included in the plot
#' @param labele_small    the minimum number to have a label inside the stacked bars
#' @param first_column    to add y-axis ticks and labels at each break point for the first column

func_alluvial <- function(data, 
                          first_column = NA, 
                          sorted = NA,
                          indicator_n_min = 5, ## 
                          width_my = 1/2.5, 
                          w_p = 6,
                          labele_small = 15,
                          add_flow_label = F, 
                          n_ctr = '',
                          show_y_ticks = T, 
                          save_png = T, 
                          filename.prefix = 'mini-review_',
                          filename.postfix = '') {
  p <- data %>%
    dplyr::filter(freq >= indicator_n_min) %>%
    ggplot(
      .,
      aes(
        x = dimension,
        y = freq,
        alluvium = id_within_layers,
        stratum = layers,
        fill = layers
      )
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    # scale_fill_manual(values = getPalette(colourCount)) +
    # geom_flow(width = width_my) +
    geom_flow(
      width = width_my,
      # aes.bind = TRUE,
      decreasing = sorted
    ) +
    geom_stratum(
      alpha = .6, width = width_my,
      decreasing = sorted
    ) +
    stat_stratum(
      geom = "text",
      aes(label = ifelse(as.numeric(total) >= labele_small, layers, NA)),
      decreasing = sorted
    ) +
    ggrepel::geom_text_repel(
      decreasing = sorted,
      aes(label = ifelse(as.numeric(total) < labele_small, as.character(layers), NA)),
      # segment.square  = F,
      # segment.inflect = T,
      segment.size = 0.3,
      segment.alpha = .7,
      segment.curvature = 0.2, ## negative for left-hand and positive for right-hand curves, 0 for straight lines
      # segment.curvature = -1e-20,
      segment.ncp = 3,
      segment.angle = 20, ## values greater than 90 would skew toward the end
      
      nudge_x = .4,
      nudge_y = .5,
      force_pull = 1,
      direction = "both",
      stat = "stratum",
      size = 3
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0, "in"),
      panel.spacing = unit(0, "in"),
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        size = rel(1.4),
        vjust = 10,
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
    ) +
    ylab('Number of studies')
  
  
  ## Calculate cumulative values for the first column (stratum)
  ## if you'd like to add y-axis ticks and labels at each break point for the first column 
  if(!is.na(first_column)){
    breakpoints <- data %>%
      dplyr::filter(dimension == first_column) %>%         # Filter for the first column's year
      dplyr::filter(freq >= indicator_n_min) %>%
      dplyr::distinct(layers, .keep_all = T) %>%
      arrange(desc(layers)) %>%
      pull(total) %>%                    # Extract the `value` column
      cumsum() %>%                       # Compute cumulative sums
      c(0, .)                            # Add 0 as the first breakpoint
  }
  
  if (show_y_ticks == F) {
    p <- p + 
      theme(axis.ticks.y = element_blank(), axis.text.y  = element_blank())
  } else if (show_y_ticks == T & is.na(first_column)) {
    p <- p + 
      theme(axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))
  } else {
    p <- p + 
      scale_y_continuous(
        breaks = seq(0, sum(data$freq), by = 10),  # Custom y-axis ticks
        
        ## if you'd like to add y-axis ticks and labels at each break point for the first column 
        # breaks = breakpoints,                       # Use calculated breakpoints
        # labels = breakpoints,                       # Display breakpoints as labels
        # expand = c(0, 0)
      ) +
      theme(axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))
  }
  
  
  ##' add flow labels
  ##' https://github.com/corybrunson/ggalluvial/issues/132
  
  if (add_flow_label == T){
    # p <- p +
    #   geom_text(
    #     stat = "flow",
    #     aes(
    #       # label = ifelse(x == 2, after_stat(n), NA),
    #       label = ifelse(x == 2 &
    #                        # stratum == 'Anxiety' &
    #                        after_stat(flow) == "from" &
    #                        after_stat(n) > 0,
    #                      # scales::percent(after_stat(prop), accuracy = 0.1), ## use %
    #                      after_stat(n), ## use number count
    #                      NA),
    #       hjust = (after_stat(flow) == "to")
    #     )
    #   )


    # p <- p +
    #   stat_flow(
    #     geom = "text",
    #     aes(label = ifelse(as.numeric(total) >= labele_small, n_ind_byregion, NA)),
    #     decreasing = sorted
    #   )
    
    p <- p +
      geom_text(
        stat = "flow",
        aes(
          label = ifelse(x == 2, after_stat(n), NA), ##' `x =2` denote the 2nd column 
          hjust = (after_stat(flow) == "to")
        )
      )


  }
  
  
  
  if (save_png == T) {
    f <- paste0(filename.prefix, filename.postfix, '_min', indicator_n_min, '_ctr', n_ctr, '_', today, '.png')
    fname <- paste0(dir.fig, f); print(fname)
    ggsave(filename = fname, plot = p, width = w_p, height = 8, units = 'in', dpi = 300)
  } else {
    print('Figure is not saved.')
  }
  
  return(p)
}
