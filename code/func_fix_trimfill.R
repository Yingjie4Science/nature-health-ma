
library(meta)
library(dplyr)

# Function to test and fix trimfill() errors related to level.hetstat
func_fix_trimfill <- function(ma_smd) {
  # Attempt trimfill() and catch errors
  result <- tryCatch({
    meta::trimfill(ma_smd)
  }, error = function(e) {
    message("Initial trimfill() error: ", e$message)
    
    # Check if error is about level.hetstat
    if (grepl("level.hetstat", e$message, ignore.case = TRUE)) {
      message("\n--- Investigating level.hetstat ---")
      
      # Check if level.hetstat exists in meta object
      if (!"level.hetstat" %in% names(ma_smd)) {
        message("level.hetstat NOT found in meta object. Forcefully adding it.")
        
        # Add level.hetstat with default 0.95 (or your preferred value)
        ma_smd$level.hetstat <- 0.95  # Force override
        warning("Manually added level.hetstat to meta object. Proceed with caution!")
        
        # Retry trimfill after override
        message("Retrying trimfill()...")
        return(meta::trimfill(ma_smd))
      } else {
        message("level.hetstat EXISTS in meta object but caused an error.")
        message("Current value: ", ma_smd$level.hetstat)
        stop("Cannot resolve - check meta object structure")
      }
    } else {
      stop("Error unrelated to level.hetstat: ", e$message)
    }
  })
  
  return(result)
}

# --------------------------------------------------
# # Usage example:
# # Assuming you have an existing meta-analysis object `ma_smd`
# trimfill_result <- func_fix_trimfill(ma_smd)