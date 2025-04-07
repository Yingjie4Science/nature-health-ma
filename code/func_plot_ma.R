# Install and load necessary packages
# install.packages(c("ggplot2", "metafor"))
library(ggplot2)
library(ggpubr)
source('./code/func_color_bygroup.R')
source('./code/func_ggsave.R')
source('./code/func_make_gradient_bg.R')



## colors
color_mh_positive <- "#175E54"  
color_mh_negative <- "#8C1515"

color_mh_positive <- "#2166ac" 
color_mh_negative <- "#b2182b" 

# Create a forest plot-style ggplot

plot_effect_size_overall <- function(
    data, 
    color_var = NULL, ## e.g.,  color_var = "tool"
    legend_title = '',
    xlab_name = "Effect Size", 
    subgroup = NULL, 
    dodge_value = 0.9,
    facet_bygroup = F,
    facet_ncol = 4,
    facet_scales = 'free',
    add_gradient_bg = T,
    add_vline_es = F,       ## if add vline for 0.2, 0.5, 0.8
    reorder_y_subgroup = F, 
    text_size = 12,
    show_legend = F) {
  
  theme_set(
    theme_minimal() + 
      theme(text = element_text(size = text_size), 
            axis.text = element_text(size = text_size),
            strip.text = element_text(size = text_size)
            )) # Applies a base size
  
  # x_limit_max <- max(abs(data$es.lower), abs(data$es.upper), na.rm = T) * 1.1
  
  gradient_bg <- make_gradient_bg(deg = 180, n = 500, cols = brewer.pal(9, "RdBu"))
  
  data <- data %>%
    dplyr::mutate(
      n_lab = paste0('n = ', n_study),
      # I2_lab= paste0("I^{2} ==", I2*100, '~', "\"%\""),
      I2_lab = sprintf("italic(I)^2==%.2f", I2), 
      # I2_lab= paste0("I^{2} == ", I2)
      ind_sub = as.factor(ind_sub),
      positive = ifelse(es.mean > 0, 'Positive MH', 'Negative MH')
      )
  
  
  ## 1. if to include subgroup analysis --------------------------------------------------
  
  ## 1.1 - no subgroup 
  if (is.null(subgroup)) { 
    
    if (!is.null(color_var)) {
      p <- data %>%
        ggplot(., 
               aes(x = es.mean, 
                   # y = ind_sub, 
                   # y = reorder(ind_sub, es.mean),          # the largest effect on the top
                   y = reorder(ind_sub, desc(es.mean)),      # the largest effect on the top
                   group = .data[[color_var]],
                   shape = .data[[color_var]],       ## use point shape to distinguish tools
                   # color = .data[[color_var]]      ## use point shape to distinguish tools
                   color = positive,
                   ))
    } else {
      p <- data %>%
        ggplot(., 
               aes(x = es.mean, 
                   # y = ind_sub,                       ## default order
                   y = reorder(ind_sub, desc(es.mean)), ## reorder by y
               ))
    }
    
    
  ## 1.2 - with subgroups  
  } else {
    
    ##' To keep the order of each Category unchanged in the legend while sorting the y-axis values within each facet based on x values, 
    ##' you can use a separate factor for the sorting within the facets and maintain the original factor levels for the legend. 
    ##' This approach involves creating two different variables:
    ##'     Original Category Variable: Used for the legend and keeps the original order of categories.
    ##'     Sorted Category Variable: Used for plotting within the facets, where the order is determined based on the x values within each facet.
    ##'     
    # Original order for the legend
    data$subgroup_original <- factor(data$subgroup, levels = unique(data$subgroup))
    
    # Step 1: Determine ordering within each facet based on mean Value
    ordering_info <- data %>%
      group_by(ind_sub) %>%
      arrange(es.mean) %>%
      dplyr::mutate(subgroup_sorted = factor(subgroup, levels = unique(subgroup))) %>%
      ungroup()
    
    # Step 2: Merge sorted category information back into original data frame
    data <- data %>%
      left_join(ordering_info %>% select(tool, ind_sub, subgroup, subgroup_sorted), by = c("tool", "ind_sub", "subgroup")) 
    
    if(reorder_y_subgroup == T){
      p <- data %>%
        ggplot(., 
               aes(x = es.mean, 
                   y = subgroup_sorted,    ## reorder -- the largest effect on the top
                   color = !!sym(subgroup),  ## use color to distinguish tools
               ))
    } else (
      p <- data %>%
        dplyr::select(-subgroup_sorted) %>%
        dplyr::mutate(subgroup_sorted = factor(subgroup, levels = group_list)) %>%
        ggplot(.,  
               aes(x = es.mean, 
                   y = subgroup_sorted, ## default order
                   color = !!sym(subgroup)))
    )
    
    
  }
  
  
  
  ## 2. if to add gradient background color ----------------------------------------------
  if(add_gradient_bg == T){
    p <- p + 
      annotation_custom(grob = gradient_bg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    vline_0_color = "gray70"
    vline_width = 0.6
  } else {
    p <- p
    vline_0_color = "grey50"
    vline_width = 0.4
  }
  

  
  ## the main plot function 
  p <- p + 
    geom_vline(xintercept = 0, linewidth = vline_width, color = vline_0_color, alpha = 0.5) + # linetype = "dashed", 
    
    ## 
    # scale_x_continuous(limits = c(-x_limit_max, x_limit_max)) + 
  
    geom_point(aes(x = es.mean), size = 2.5, position = position_dodge(dodge_value), show.legend = show_legend) +
    geom_errorbarh(aes(xmin = es.lower, xmax = es.upper), 
                   height = 0.15, position=position_dodge(width = dodge_value), show.legend = F) +
    # geom_text(aes(x = es.mean, label = paste0(round(es.mean, digits = 2), ' ', p.star)), 
    #           vjust = 1.7, size = 2.5, position=position_dodge(dodge_value), show.legend = F) +
    
    ###' text labels for `p value` 
    geom_text(aes(x = es.mean, label = p.star), vjust = -0.3, size = text_size/4, position=position_dodge(dodge_value), show.legend = F) +
    labs(title = "", x = xlab_name, y = "") 
  
  
  
  
  ## 3.1 for non-subgroup analysis -------------------------------------------------------
  if (is.null(subgroup) & add_vline_es == T) { 
    p <- p +
      ### SMD effect threshold values: small - moderate - large
      # geom_vline(xintercept = -0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") + ## , linewidth = 0.5
      geom_vline(xintercept = -0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept = -0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      # geom_vline(xintercept =  0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept =  0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept =  0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") 
  } else if (is.null(subgroup) & add_vline_es == F) {
    p <- p +
      ###' text labels for `es`, `n`, `I2`
      geom_text(aes(x = es.mean, label = round(es.mean, digits = 2)), 
                vjust = 1.7, size = text_size/4, 
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = n_lab), 
                vjust = 0,  hjust = -0.2, 
                size = text_size/4, color='gray30', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = I2_lab), 
                vjust = 1, hjust = -0.15, 
                size = text_size/4, color='gray30', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F, parse = T)
    
    
    ## add annotated arrows and text
    y_arrow <- 0    ## figure before 3/3/2025
    y_arrow <- 4.5  ## figure 
    x_arrow <- 2.0  ## figure, 1.5 before 3/3/2025
    p <- p + 
      annotate("segment", x = 0, xend =  x_arrow, 
               y = y_arrow, yend = y_arrow, 
               colour = color_mh_positive, linewidth = .8, 
               arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
      annotate("segment", x = 0, xend = -x_arrow, 
               y = y_arrow, yend = y_arrow, 
               colour = color_mh_negative, linewidth = .8, 
               arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
      annotate("text", x = x_arrow-0.5,  y = y_arrow-0.0, 
               label = "Increase in positive MH", colour = color_mh_positive, angle = 0, vjust = -1) +
      annotate("text", x = -x_arrow+0.8, y = y_arrow-0.6, 
               label = "Reduction in negative MH",colour = color_mh_negative, angle = 0, vjust = -1) +
      #' Adjust the limits and aspect of the plot as needed
      #' clip off can ensure the full arrows can be shown on the axis
      coord_cartesian(clip = "off", ylim = c(NA, NA)) +
      
      ## (1) when using color for tools
      # labs(color = legend_title) +
      # scale_color_brewer(type = 'qual', palette = 'Dark2') +  ## Colorblind Friendly
      # guides(color = guide_legend(reverse=F)) +
      
      ## (2) when using shape for tools and color for positive or negative aspects
      labs(shape = legend_title) +
      scale_color_manual(values = c(color_mh_negative, color_mh_positive)) + 
      guides(color = F)  ## do not show color in legend
    
      # ## Mirror y-axis Labels Based on x Position
      # theme(axis.text.y = element_blank()) +# Hide default y-axis text
      # geom_text(data = data[data$es.mean < 0, ],  aes(label = ind_sub, x = min(data$es.mean) - 1), hjust = 1, size = 3, color = '#8C1515') +
      # geom_text(data = data[data$es.mean >= 0, ], aes(label = ind_sub, x = max(data$es.mean) + 1), hjust = 0, size = 3, color = '#175E54') +
      # scale_x_continuous(expand = expansion(mult = c(0.2, 0.2)))  ## add more space on the right for y-axis Labels
    
    
    ## 3.2 - for subgroup plots ---------------------------------------------------------- 
    ## remove sample size for plot of subgroup results
  } else {
    p <- p +
      
      ###' text labels for `es`, `n`, `I2`
      # geom_text(aes(#x = es.mean/abs(es.mean)*(-2),
      #               # x = es.mean*1.1,
      #               x = es.mean,
      #               label = round(es.mean, digits = 1)),
      #           vjust = 1.7,
      #           size = text_size/5,
      #           position=position_dodge(dodge_value), show.legend = F) +

      ##' add `n` label
      # geom_text(aes(x = ifelse(es.mean<0, 
      #                          es.lower,
      #                          es.upper
      #                          # ifelse((es.upper-es.lower)>3, 2.5*es.mean/abs(es.mean), es.lower), 
      #                          # ifelse((es.upper-es.lower)>3, 2.5*es.mean/abs(es.mean), es.upper)
      #                          ), 
      #               hjust = ifelse((es.upper-es.lower)>2, 
      #                              ifelse(es.mean<0, -(es.mean+es.lower)/20, -(es.mean)/30), 
      #                              ifelse(abs(es.lower-es.mean)<0.5, -0.8*es.mean/abs(es.mean), -0.3*es.mean/abs(es.mean))),
      #               label = n_lab),  
      #           hjust = -0.2,
      #           vjust = -0.5, size = text_size/6, position=position_dodge(dodge_value), show.legend = F) +
      
      geom_text(aes(x = text_position,
                    # hjust = ifelse(es.mean < group_mean, 1.1, -0.2),
                    hjust = hjust_position,
                    label = n_lab),  
                # hjust = hjust_position,
                color = 'gray30', fontface = "italic",
                vjust = -0.5, size = text_size/7, position=position_dodge(dodge_value), show.legend = F) +
      
      ##' add `I2` label
      # geom_text(aes(x = ifelse(es.mean<0,
      #                          es.lower,
      #                          es.upper
      #                          # ifelse((es.upper-es.lower)>3, 2.5*es.mean/abs(es.mean), es.lower),
      #                          # ifelse(es.upper>3, 2.5*es.mean/abs(es.mean), es.upper)
      #                          ),
      #               hjust = ifelse((es.upper-es.lower)>2,
      #                              ifelse(es.mean<0, -(es.mean+es.lower)/20, -(es.mean)/30),
      #                              ifelse(abs(es.lower-es.mean)<0.5, -0.8*es.mean/abs(es.mean), -0.3*es.mean/abs(es.mean))),
      #               label = ifelse(n_study>1, I2_lab, NA)),
      #           vjust = 1, size = text_size/6, position=position_dodge(dodge_value), show.legend = F, parse = T) +
      geom_text(aes(x = text_position,
                    hjust = hjust_position,
                    label = ifelse(n_study>1, I2_lab, NA)),
                # hjust = hjust_position,
                color = 'gray30', 
                vjust = 1, size = text_size/7, position=position_dodge(dodge_value), show.legend = F, parse = T) +
      
      scale_x_continuous(expand = expansion(mult = c(0.5, 0.5))) 
  }
  
  
  
  ## Color parameter
  if (!is.null(color_var)) {
    p <- p + 
      ## (1) when using color for tools
      # scale_color_brewer(palette = "Dark2", direction = 1) ## Colorblind Friendly
      ## (2) when using shape for tools and color for positive or negative aspects
      scale_color_manual(values = c(color_mh_negative, color_mh_positive)) +  
      guides(color = 'none')  ## do not show color in legend
      
  } else {
    p <- p
  }
  
  
  ## Overall theme 
  p <- p +
    # theme_minimal() +
    theme(axis.text = element_text(color = "black"),
          # panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  
  ## when the number of subgroup is too large, it is better to facet the plot
  if (facet_bygroup == T) {
    p <- p +
      facet_wrap(~ind_sub, scales = facet_scales, ncol = facet_ncol) +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            # plot.margin = margin(t = 5, r = 5, b = 5, l = 5, "points"),
            legend.spacing.y = unit(0, 'cm'),
            legend.spacing.x = unit(0, 'cm'),
            legend.key.size = unit(0.1, 'cm'),
            )
  }
  

  ## save plot ---------------------------------------------------------------------------
  #' how many sub-indicators -> decide plot height
  # height_tbd <- 1+3/7*length(unique(data$ind_sub))
  # p
  # fname <- paste0(dir.fig, 'plot_es_', unique(data$tool) ,'.png'); fname
  # func_ggsave(fname, w = 6, h = height_tbd, save_png = T)
  
  
  ## return plot -------------------------------------------------------------------------
  return(p)
  
}


## test
# plot_ma(data = ma_result)
# p1 <- plot_ma(data = ma_result)


