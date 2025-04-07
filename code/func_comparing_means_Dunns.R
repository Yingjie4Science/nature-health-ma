
## ------------------------------------------------------------------------------------- #
library(dplyr)
library(rstatix) # dunn_test(); wilcox_test(); add_xy_position()
library(rlang)
library(ggpubr)


## ------------------------------------------------------------------------------------- #
func_test_dif_dunn <- function(
    df, 
    value = 'value', 
    group = 'group',
    facet_by = 'ind_sub') {
  
  # Step 1: Perform Dunn's test for each group (facet)
  df <- df %>%
    dplyr::rename('Value' = value,
                  'Group' = group)
  
  # Step 1: Perform Dunn's test for each facet
  dunn_results <- df %>%
    group_by(ind_sub) %>%
    dunn_test(Value ~ Group, p.adjust.method = "bonferroni")
  
  # Step 2: Ensure `xmin` and `xmax` are correctly assigned
  dunn_results <- dunn_results %>%
    group_by(ind_sub) %>%
    add_xy_position(x = "group")  %>% # Add positioning for plotting
    ungroup() %>%
    dplyr::mutate(xmin = group1, xmax = group2)  # Manually assign x positions
  
  
  # Step 3: Create a faceted boxplot with Dunn’s test results
  p <- ggboxplot(df, x = "Group", y = "Value", fill = "Group") +
    stat_pvalue_manual(dunn_results, hide.ns = TRUE) +  # Add p-values
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +  # Add 8% space at the top
    facet_wrap(~ ~ get(facet_by), scales = "free_y", ncol = 5) +  # Facet by group
    theme_bw() +
    labs(x = "", y = "Means") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(legend.position = "none")
  return(p)
}



## ------------------------------------------------------------------------------------- #

## https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
##' The above function is the same as the below, while the latter can `add_xy_position` by
##'   each MH indicator, and the y.position can better represent the value of each indicator
##'   gtoup 

func_test_dif_dunn2 <- function(df, 
                                value = 'value', 
                                group = 'group',
                                which_test = 'dunn', ## or "wilcoxon"
                                facet_by = 'ind_sub') {
  # df <- ma_result_each_i
  unique(df$ind_sub)
  
  
  df <- df %>%
    dplyr::rename('Value' = value, 
                  'Group' = group) %>%
    dplyr::mutate(ind_sub = factor(ind_sub, levels = ind_sub_levels))
  
  
  # data_comb <- data.frame()
  dunn_results <- data.frame()
  
  for (ind in unique(df$ind_sub)) {
    
    data <- df %>%
      dplyr::filter(ind_sub == ind)
    
    ##' Determine the number of groups (num_groups):
    ##' If 2 groups → Wilcoxon test (wilcox.test()).
    ##' If 3+ groups → Kruskal-Wallis test (kruskal.test()).
    ##' If Kruskal-Wallis is significant (p < 0.05), perform Post-hoc Pairwise Tests (if necessary):
    ##'   -> Use Wilcoxon for pairwise post-hoc comparisons.
    ##'   -> Use Dunn’s test as an alternative post-hoc.

    ##' Select which test function data to use 
    if (which_test == 'wilcoxon') {
      
      ##' 1. Pairwise comparisons using Wilcoxon’s test
      pwc <- data %>% 
        group_by(Group) %>%
        dplyr::filter(n() > 1) %>%
        ungroup() %>%
        wilcox_test(Value ~ Group, p.adjust.method = "bonferroni")

    } else if (which_test == 'dunn') {
      
      ##' 2. Pairwise comparisons using Dunn’s test
      ##'   Compared to the Wilcoxon’s test, the Dunn’s test takes into account the rankings 
      ##'   used by the Kruskal-Wallis test. It also does ties adjustments.
      pwc <- data %>%
        dunn_test(Value ~ Group, p.adjust.method = "bonferroni")
      
    } else {
      stop("Please specify a test approach!")
    }
    
    pwc$ind_sub = ind
    pwc <- pwc %>% 
      rstatix::add_xy_position(x = "group", fun = 'median_iqr') %>%
      mutate(xmin = group1, xmax = group2)  # Manually assign x positions
    
    # res.kruskal <- data %>% 
    #   kruskal_test(Value ~ Group) %>%
    #   mutate(ind_sub = ind)
    
    # data_comb <- rbind(data_comb, data)
    dunn_results <- rbind(dunn_results, pwc)
  }
  
  
  dunn_results <- dunn_results %>%
    dplyr::mutate(ind_sub = factor(ind_sub, levels = ind_sub_levels))
    
  # Create the plot with ggplot2
  # Visualization: box plots with p-values
  
  if (!is.null(facet_by)) {

    p <- ggboxplot(df, x = "Group", y = "Value", fill = "Group", add = "jitter", add.params = list(shape=1, size=0.5, alpha=0.5), size = 0.2) +
      stat_pvalue_manual(dunn_results, step.increase = 0.06, color = 'gray50', hide.ns = TRUE) +  # Add p-values
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +  # Add 8% space at the top
      scale_fill_manual(values = color_bygroup) +
      facet_wrap(~ get(facet_by), scales = "free_y", ncol = 4) +  # Facet by group
      theme_bw() +
      labs(x = group_title, y = "Mean effect sizes") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(legend.position = "none")
    
  } else {
    p <- 
      ggboxplot(df, x = "Group", y = "Value", fill = 'Group') +
      stat_pvalue_manual(test_comb, color = 'gray50', hide.ns = TRUE) +
      scale_fill_manual(values = color_bygroup) +
      theme_bw() +
      theme(legend.position="none")
  }
  
  return(p)
}


