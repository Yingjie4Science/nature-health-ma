

library(dmetar) # InfluenceAnalysis()

set.seed(123)

# ###################################################################################### #
# Function to perform random subset sensitivity analysis 
# ###################################################################################### #

n10_analysis <- function(meta_model, 
                         data, 
                         iterations = 100, 
                         random_remove_n = 5) {
  results <- numeric(iterations)
  
  for (i in 1:iterations) {
    sample_indices <- sample(1:nrow(data), size = (nrow(data) - random_remove_n), replace = FALSE)
    temp_data <- data[sample_indices, ]
    # temp_res <- rma(yi, vi, data = temp_data, method = "REML")
    temp_res <- meta::metacont(
      data = temp_data,
      n.e = e_n,
      n.c = c_n,
      mean.e = e_mean,
      mean.c = c_mean,
      sd.e = e_sd_r,
      sd.c = c_sd_r,
      studlab = study_label, #study_label,
      
      sm = "SMD", 
      method.smd = "Hedges",
      fixed = FALSE,
      random = TRUE,
      method.tau = "REML",
      hakn = TRUE, 
      subgroup = NULL,  ## Grouping results by a variable, default = NULL
      title = paste('mh_tool', collapse = '; '))
    
    results[i] <- temp_res$TE.random
  }
  
  return(results)
}







func_sens_n1_smd_subgroup <- function(
    
  data,
  
  ##' input data from the loop
  mh_tool, 
  sub_ind_i, 
  subgroup_select
  
) {
  
  
  ### 2. Run Influence Analysis
  ma_smd.inf <- dmetar::InfluenceAnalysis(ma_smd, random = T)
  
  # Convert influence analysis results to a dataframe
  n1_res_df <- data.frame(
    
    ## basic data
    tool = paste(mh_tool, collapse = '; '), 
    ind_sub = sub_ind_i, 
    group_name = subgroup_select,
    subgroup = subgroup_elements_i,
    
    ## new data
    study       = ma_smd$studlab,  # Study labels
    es.n1       = ma_smd.inf$Data$effect,  # Effect sizes when each study is omitted
    es.n1.lower = ma_smd.inf$Data$lower,  # Lower CI bound
    es.n1.upper = ma_smd.inf$Data$upper   # Upper CI bound
  )
  
  return(n1_res_df)
  
}




func_sens_n10_smd_subgroup <- function(
    meta_model, 
    data,
    ##' for n-10 Sensitivity Analysis
    iterations = 100,
    random_remove_n = 5,
    
    ##' input data from the loop
    mh_tool, 
    sub_ind_i, 
    subgroup_select
    
) {
  
  # Run n-10 analysis 100 times
  n10_res <- n10_analysis(ma_smd, data, 
                          iterations = iterations, 
                          random_remove_n = random_remove_n)
  
  # Convert results to a dataframe for ggplot2
  n10_res_df <- data.frame(
    ## basic data
    tool = paste(mh_tool, collapse = '; '), 
    ind_sub = sub_ind_i, 
    group_name = subgroup_select,
    subgroup = subgroup_elements_i,
    
    ## new data
    es.n10 = n10_res)
  
  return(n10_res_df)
}


