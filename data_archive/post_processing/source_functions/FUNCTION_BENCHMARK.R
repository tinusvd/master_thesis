benchmark_run <- function(object, info_object, pop_est = c(0, 0)){
  # Load required library for string extraction
  library(stringr)
  
  # Extract the names of all result entries (condition_X_sim_Y)
  result_names <- names(object)
  
  # Create a list to store all benchmark results
  benchmark_results <- list()
  
  # Process each entry in the object
  for(i in 1:length(object)){
    # Get the current result name
    current_name <- result_names[i]
    
    # Extract the current object
    current_obj <- object[[i]]
    
    # Create a list for this specific result
    output_list <- list()
    
    # Store the n value from the current object
    # We added this in the previous function
    output_list$n <- current_obj$n
    
    # Run benchmark for each set
    # Note: set1 and set2 are the same hypotheses, but set 2 has Heq = TRUE. 
    # This causes the benchmark function to crash.
    
    if (all(pop_est == c(0.15, 0.15))) {
      
      bench_set1 <- benchmark(object = current_obj$set1, pop_est = pop_est, iter = 5000)
      output_list$set1 <- bench_set1
      output_list$set1_perc <- bench_set1$benchmarks_ratio_ll_weights
      
      benchmark_results[[current_name]] <- output_list
      
    } else if (all(pop_est == c(0.20, 0.15))) {
      
      bench_set3 <- benchmark(object = current_obj$set3, pop_est = pop_est, iter = 5000)
      output_list$set3 <- bench_set3
      output_list$set3_perc <- bench_set3$benchmarks_ratio_ll_weights
      
      bench_set4 <- benchmark(object = current_obj$set4, pop_est = pop_est, iter = 5000)
      output_list$set4 <- bench_set4
      output_list$set4_perc <- bench_set4$benchmarks_ratio_ll_weights
      
      benchmark_results[[current_name]] <- output_list
      
    } else {
      stop("No valid population values are provided. Function stopped.")
    }
    
    
    }
  
  # Create a dynamic name for the results
  info_name <- deparse(substitute(info_object))
  benchmark_name <- paste0(info_name, "_benchmark")
  
  # Assign to global environment
  assign(benchmark_name, benchmark_results, envir = .GlobalEnv)
  
  # Return a summary
  summary <- paste("Created", benchmark_name, "containing benchmark results for", 
                   length(benchmark_results), "simulation results.")
  
  return(summary)
}

