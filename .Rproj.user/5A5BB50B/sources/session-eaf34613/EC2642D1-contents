rules_bench <- read_csv2("./data_archive/meta_files/meta_benchmark_rules.csv")

add_bound_support <- function(final_data, rules, bound_object) {
  
  df <- final_data
  
  # Loop through each rule
  for (i in seq_len(nrow(rules))) {
    
    condition_name <- rules$from[i]
    set_nr <- rules$set_nr[i]
    ratio1 <- rules$ratio_of_interest[i]
    ratio2 <- rules$ratio_of_interest_2[i]
    
    # Rows where the condition_id matches any of the specified rule values
    matching_ids <- c(rules$from[i], rules$to_1[i], rules$to_2[i], rules$to_3)
    
    if (set_nr == "set1") {
      
      # Identify the correct element in bound_object using condition_name
      ind <- grep(pattern = paste0("^", condition_name, "(?!\\d)"), 
                  x = names(bound_object), 
                  perl = TRUE)
      if (length(ind) == 0) next  # Skip if no matching bound found
      
      # Extract p5 and p95 from the set1 benchmarks
      p5 <- bound_object[[ind]]$set1$benchmarks_ratio_ll_weights[[1]][2]
      p95 <- bound_object[[ind]]$set1$benchmarks_ratio_ll_weights[[1]][6]
      # p5 <- .8
      # p95 <- 1.25
      
      # Update using dplyr; for matching rows, check if ratio1 and ratio2 are between p5 and p95
      df <- df %>%
        mutate(
          bound_supp_set1 = if_else(
            condition_id %in% matching_ids,
            if_else(!!sym(ratio1) >= p5 & !!sym(ratio1) <= p95, 1L, 0L),
            bound_supp_set1
          ),
          bound_supp_set2 = if_else(
            condition_id %in% matching_ids,
            if_else(!!sym(ratio2) >= p5 & !!sym(ratio2) <= p95, 1L, 0L),
            bound_supp_set2
          )
        )
      
    } else if (set_nr == "set3") {
      
      ind <- grep(pattern = paste0("^", condition_name, "(?!\\d)"), 
                  x = names(bound_object), 
                  perl = TRUE)
      if (length(ind) == 0) next  # skip if no match
      
      
      # For set3, assume ratio1 uses set3 benchmarks and ratio2 uses set4 benchmarks
      p5_set3 <- bound_object[[ind]]$set3$benchmarks_ratio_ll_weights[[1]][2]
      p95_set3 <- bound_object[[ind]]$set3$benchmarks_ratio_ll_weights[[1]][6]
      p5_set4 <- bound_object[[ind]]$set4$benchmarks_ratio_ll_weights[[1]][2]
      p95_set4 <- bound_object[[ind]]$set4$benchmarks_ratio_ll_weights[[1]][6]
      # p5_set3 <- exp(-1)
      # p95_set3 <- exp(1)
      # p5_set4 <- exp(-1)
      # p95_set4 <- exp(1)
      
      df <- df %>%
        mutate(
          bound_supp_set3 = if_else(
            condition_id %in% matching_ids,
            if_else(!!sym(ratio1) >= p5_set3 & !!sym(ratio1) <= p95_set3, 1L, 0L),
            bound_supp_set3
          ),
          bound_supp_set4 = if_else(
            condition_id %in% matching_ids,
            if_else(!!sym(ratio2) >= p5_set4 & !!sym(ratio2) <= p95_set4, 1L, 0L),
            bound_supp_set4
          )
        )
    }
  }
  
  return(df)
}