

##' subgroup analysis
##'   as a source code script prepared for the main script
##'   

source('./code/func_plot_ma.R')

# Custom function to format axis labels
custom_x_axis_format <- function(x) {
  ifelse(x == round(x), as.character(x), sprintf("%.1f", x))
}


p_all <- data %>%
  dplyr::filter(group_name == subgroup_select) %>%
  dplyr::filter(ind_sub %in% ind_sub_levels) %>%
  dplyr::group_by(tool, ind_sub) %>%
  dplyr::mutate(ind_sub = factor(ind_sub, levels = ind_sub_levels)) %>%
  dplyr::mutate(tool_ind = paste0(tool, '-', ind_sub)) %>%
  dplyr::mutate(group_mean = mean(es.mean, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::mutate(
    text_position = case_when(
      es.mean < 0  & es.mean <  group_mean ~ es.upper,
      es.mean < 0  & es.mean >= group_mean ~ es.lower,
      es.mean >= 0 & es.mean <= group_mean ~ es.upper,
      es.mean >= 0 & es.mean >  group_mean ~ es.lower,
      T ~ es.mean),
    hjust_position = case_when(
      es.mean < 0  & es.mean <  group_mean ~ -0.2,
      es.mean < 0  & es.mean >= group_mean ~ 1.1,
      es.mean >= 0 & es.mean <= group_mean ~ -0.2,
      es.mean >= 0 & es.mean >  group_mean ~ 1.1,
      T ~ 0)
    
    ) %>%
  # dplyr::filter(ind_sub %in% ind_for_presentation) %>%
  # dplyr::filter(!subgroup %in% nature_type_remove_for_presentation) %>%
  as.data.frame()


##' This ensures that all facets contain all categories in `subgroup`, avoiding missing legends or inconsistent coloring.
##' Define all possible combinations of `ind` and `subgroup`
all_combinations <- expand.grid(tool_ind = unique(p_all$tool_ind), subgroup = unique(p_all$subgroup)) %>%
  as.data.frame() %>%
  tidyr::separate(tool_ind, c('tool', 'ind_sub'), sep = '-') %>% 
  as.data.frame()

# Merge with original data and fill missing values with NA or 0
p_all_complete <- full_join(all_combinations, p_all, by = c("tool", "ind_sub", "subgroup")) %>%
  dplyr::mutate(ind_sub = factor(ind_sub, levels = ind_sub_levels)) %>%
  dplyr::mutate(subgroup = factor(subgroup, levels = group_list)) %>%
  as.data.frame()


# colors_group <- func_color_bygroup(df = p_all_complete, column_name = "subgroup", color_pal = color_bygroup)
# color_bygroup <- colors_group
unique(p_all_complete$subgroup)

##' to double check if there are more than one entries in the subgroup column 
more_than_one_matches <- stringr::str_detect(pattern = ";|from window", string = unique(p_all_complete$subgroup));
# Print the matching elements
matching_elements <- unique(p_all_complete$subgroup)[more_than_one_matches] %>% as.character()
if( length(matching_elements) > 0) {
  warning(paste0("There are more than one entries in the subgroup ------ "), 
          subgroup_select, ' ------ \n\t',
          paste(matching_elements, collapse = '\n\t'))
}




## plot ----------------------------------------------------------------------------------
p_all_complete %>%
  ## remove this TBD more-than-one entries in the subgroup
  dplyr::filter(!subgroup %in% matching_elements) %>% 
  plot_effect_size_overall(data = .,
                           subgroup = 'subgroup', 
                           facet_bygroup = T,  
                           facet_ncol = facet_ncol,
                           facet_scales = 'free',
                           text_size = 11,
                           add_gradient_bg = F,
                           reorder_y_subgroup = F,
                           show_legend = T) +
  scale_colour_manual(values = color_bygroup) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.25)), n.breaks = 4, labels = custom_x_axis_format) +
  # scale_x_continuous(expand = expansion(add = c(0.25, 0.25))) +
  # this will allow the text outside of the plot panel
  coord_cartesian(clip = 'off', xlim = c(NA, NA), expand = TRUE) +
  theme(legend.position = c(0.7, 0.15),
        legend.spacing.y = unit(0, 'cm'),
        legend.spacing.x = unit(0, 'cm'),
        legend.key.size = unit(0.01, 'cm'), 
        legend.key.width = unit(0.01, 'cm'),
        legend.justification = "left", 
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(t = 0, r = 0.1, b = 0, l = 0.1, "in"),
        # panel.background = element_rect(colour = "gray50", linewidth=0.1, fill=NA), 
        panel.border = element_rect(linewidth=0.2), 
        strip.background = element_rect(linewidth=0.2), 
        legend.title=element_text(size=7.5, face = 'bold'),
        legend.text=element_text(size=7),
        panel.grid = element_blank(),
  ) +
  guides(color = guide_legend(title=group_title, reverse = T))


fname <- paste0(dir.fig, 'es_comb_subgroup_', subgroup_select, '_', today, vv, '.png'); print(fname)
func_ggsave(fname, w = 6.5, h = 6, save_png = T)



## test ------------------
# data = p_all_complete
# subgroup = 'subgroup'
# facet_bygroup = T
# facet_ncol = 5
# facet_scales = 'free_x'
# text_size = 11
# add_gradient_bg = F
# show_legend = T