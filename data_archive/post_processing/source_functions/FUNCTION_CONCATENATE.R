# Function for concatenating median
format_median <- function(median_val) {
  # Is the value of the cell larger than 9999? then, scientific notation.
  median_format <- ifelse(abs(median_val) > 9999,
                        sprintf("%.2e", median_val),
                        sprintf("%.2f", median_val))
  

  
  # Return the formatted string
  return(paste0(median_format))
}

# Function for concatenating median and SD value for the cross-lagged parameters.
format_mean <- function(mean_val, sd_val) {
  # Is the value of the cell larger than 9999? then, scientific notation.
  mean_format <- ifelse(abs(mean_val) > 9999,
                        sprintf("%.2e", mean_val),
                        sprintf("%.2f", mean_val))
  
  sd_format <- ifelse(abs(sd_val) > 9999,
                      sprintf("%.2e", sd_val),
                      sprintf("%.2f", sd_val))
  
  
  # Return the formatted string
  return(paste0(mean_format, " (", sd_format, ")"))
}

# Function for concatenating median and SD value for the cross-lagged parameters with 3 decimals of the mean
format_mean_3f <- function(mean_val, sd_val) {
  # Is the value of the cell larger than 9999? then, scientific notation.
  mean_format <- ifelse(abs(mean_val) > 9999,
                        sprintf("%.2e", mean_val),
                        sprintf("%.3f", mean_val))
  
  sd_format <- ifelse(abs(sd_val) > 9999,
                      sprintf("%.2e", sd_val),
                      sprintf("%.2f", sd_val))
  
  
  # Return the formatted string
  return(paste0(mean_format, " (", sd_format, ")"))
}




# Function for concatenating 5th and 95th percentile columns into one (cell-wise).
format_minmax <- function(p5th, p95th) {
  
  # Is the value of the cell larger than 9999? then, scientific notation.
  p5th_format <- ifelse(abs(p5th) > 9999,
                        sprintf("%.2e", p5th),
                        sprintf("%.2f", p5th))
  
  p95th_format <- ifelse(abs(p95th) > 9999,
                         sprintf("%.2e", p95th),
                         sprintf("%.2f", p95th))
  
  # Return the formatted string
  return(paste0("(", p5th_format, ", ", p95th_format, ")"))
}

# Scientific notation if larger than 9999, otherwise 2 decimal  mean values with min-max ranges
format_mean_range <- function(mean_val, min_val, max_val){
  
  mean_format <- ifelse(abs(mean_val) > 9999,
                        sprintf("%.2e", mean_val),
                        sprintf("%.2f", mean_val))
  
  min_val_format <- ifelse(abs(min_val) > 9999,
                        sprintf("%.0e", min_val),
                        sprintf("%.0f", min_val))
  
  max_val_format <- ifelse(abs(max_val) > 9999,
                         sprintf("%.0e", max_val),
                         sprintf("%.0f", max_val))
  
  return(paste0(mean_format, " [", min_val_format, ", ", max_val_format, "]"))
}

# 2 decimals
format_n_perc_2d <- function(n_val, perc_val){
  
  return(paste0(n_val, " (", sprintf("%.2f", perc_val), "%)"))
}

# 1 decimal 
format_n_perc <- function(n_val, perc_val){
  
  return(paste0(n_val, " (", perc_val, "%)"))
}
