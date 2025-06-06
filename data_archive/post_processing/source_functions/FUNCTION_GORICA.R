# Initialization for GORICA function
gorica_files <- "./data_archive/simulations/conditions_data/output"

# Function to scan the best candidates for the benchmark
scan_bound_candidate <- function(data, pop_val = c(0, 0)){
  
  df <- data
  con_grp <- rules
  
  if (all(pop_val == c(0.15, 0.15))) {
    cmg <- con_grp$condition_p1515
    popval1 <- 0.15
    popval2 <- 0.15
  } else if (all(pop_val == c(0.20, 0.15))) {
    cmg <- con_grp$condition_p215
    popval1 <- 0.20
    popval2 <- 0.15
  } else if (all(pop_val == c(0.20, 0.175))) {
    cmg <- con_grp$condition_p2175
    popval1 <- 0.20
    popval2 <- 0.15
  } else {
    stop(message = "No valid population values are supplied.")

  }
  
  df %>% 
    filter(condition_id %in% cmg) %>% 
    dplyr::select(condition_id, data_set_id, b_y12, b_y21) %>%
    # Calculate the distance of each b value from 0.15
    mutate(
      dist_b_y12 = abs(b_y12 - popval1),
      dist_b_y21 = abs(b_y21 - popval2)
    ) %>%
    # Group by condition and find the minimum combined distance
    group_by(condition_id) %>%
    slice_min(order_by = dist_b_y12 + dist_b_y21, n = 1) %>%
    # Keep original values and individual distances (no rounding)
    dplyr::select(condition_id, data_set_id, b_y12, b_y21, dist_b_y12, dist_b_y21) %>% 
    mutate(n = case_when(
      condition_id %in% con_grp$condition_n50 ~ 50,
      condition_id %in% con_grp$condition_n75 ~ 75,
      condition_id %in% con_grp$condition_n100 ~ 100,
      condition_id %in% con_grp$condition_n150 ~ 150,
      TRUE ~ NA_integer_
    ))
  
}


# Function to run GORICA for all sets of hypotheses
load_data_gorica <- function(bound, gorica_files) {
  # Get the name of the bound object
  library(restriktor)
  bound_name <- deparse(substitute(bound))
  
  # Create the results list with a dynamic name
  results_name <- paste0(bound_name, "_goric_results")
  results_list <- list()
  
  processed_files <- 0
  skipped_files <- 0
  
  cat("Starting with bound object:", bound_name, "\n")
  cat("Number of rows in bound object:", nrow(bound), "\n")
  
  # Check if gorica_files is a directory or a vector of paths
  if(length(gorica_files) == 1 && dir.exists(gorica_files)) {
    all_condition_dirs <- list.dirs(gorica_files, full.names = TRUE, recursive = FALSE)
    cat("Found", length(all_condition_dirs), "subdirectories in base directory\n")
    for(dir in all_condition_dirs) {
      cat("  - ", dir, "\n")
    }
  } else {
    all_condition_dirs <- gorica_files
    cat("Using provided gorica_files as is\n")
  }
  
  # Define the hypothesis sets
  set1 <- list("abs(B_Y12) > abs(B_Y21)")
  set2 <- list("abs(B_Y12) > abs(B_Y21)")
  set3 <- list("-0.05 < abs(B_Y12) - abs(B_Y21) < 0.05")
  set4 <- list("-0.01 < abs(B_Y12) - abs(B_Y21) < 0.01")
  
  for(i in 1:nrow(bound)) {
    sim <- bound$data_set_id[i]
    condition <- gsub("condition_", "", bound$condition_id[i])
    
    # Get the n value for this row
    n_value <- bound$n[i]
    cat("\nProcessing row", i, ": data_set_id =", sim, ", condition_id =", bound$condition_id[i], ", n =", n_value, "\n")
    
    # Find the matching condition folder
    folder_pattern <- paste0("condition_", condition, "$")
    folder_loc <- grep(pattern = folder_pattern, x = all_condition_dirs, value = TRUE)
    
    cat("  Looking for folder pattern:", folder_pattern, "\n")
    cat("  Matched folders:", if(length(folder_loc) > 0) folder_loc else "NONE", "\n")
    
    if(length(folder_loc) > 0) {
      # Build the filename
      target_filename <- paste0("gorica_output_con_", condition, "_", sim, ".Rdata")
      file_path <- file.path(folder_loc, target_filename)
      
      cat("  Looking for file:", file_path, "\n")
      cat("  File exists:", file.exists(file_path), "\n")
      
      if(file.exists(file_path)) {
        # Load the file into a temporary environment
        temp_env <- new.env()
        load(file_path, envir = temp_env)
        
        # Get the loaded object name
        obj_names <- ls(temp_env)
        cat("  Loaded objects:", if(length(obj_names) > 0) paste(obj_names, collapse=", ") else "NONE", "\n")
        
        if(length(obj_names) > 0) {
          # Get the first loaded object
          gorica_obj <- get(obj_names[1], envir = temp_env)
          
          # Create a unique ID for this result - FIXED: using just the sim ID
          result_id <- paste0("condition_", condition, "_", sim)
          
          # Extract the components needed for goric function
          est <- tryCatch({
            gorica_obj[["model_std"]][["est"]]
          }, error = function(e) {
            cat("  ERROR: Unable to extract 'est' from gorica object:", e$message, "\n")
            return(NULL)
          })
          
          cov <- tryCatch({
            gorica_obj[["model_std"]][["cov"]]
          }, error = function(e) {
            cat("  ERROR: Unable to extract 'cov' from gorica object:", e$message, "\n")
            return(NULL)
          })
          
          if(!is.null(est) && !is.null(cov)) {
            # Create a container for this object's results
            file_results <- list()
            
            # Add the n value to the results
            file_results$n <- n_value
            
            # Process each hypothesis set
            tryCatch({
              set1_result <- goric(object = est, VCOV = cov, hypotheses = set1)
              file_results$set1 <- set1_result
              cat("  Set1 analysis complete\n")
              
              set3_result <- goric(object = est, VCOV = cov, hypotheses = set3)
              file_results$set3 <- set3_result
              cat("  Set3 analysis complete\n")
              
              set4_result <- goric(object = est, VCOV = cov, hypotheses = set4)
              file_results$set4 <- set4_result
              cat("  Set4 analysis complete\n")
              
              # Add to results list
              results_list[[result_id]] <- file_results
              processed_files <- processed_files + 1
              
            }, error = function(e) {
              cat("  ERROR processing goric for", result_id, ":", e$message, "\n")
              skipped_files <- skipped_files + 1
            })
          } else {
            cat("  Skipping due to missing est or cov components\n")
            skipped_files <- skipped_files + 1
          }
        } else {
          cat("  No objects found in loaded RData file\n")
          skipped_files <- skipped_files + 1
        }
      } else {
        cat("  File not found, skipping\n")
        skipped_files <- skipped_files + 1
      }
    } else {
      cat("  No matching folder found, skipping\n")
      skipped_files <- skipped_files + 1
    }
  }
  
  # Assign the results to global environment with dynamic name
  assign(results_name, results_list, envir = .GlobalEnv)
  
  # Return information about what was created
  summary <- paste("Created", results_name, "containing", length(results_list), 
                   "simulation results. Successfully processed", processed_files, 
                   "files and skipped", skipped_files, "files.")
  
  cat("\n", summary, "\n")
  return(summary)
}

# Run the function and create different lists with output
bound1515 <- scan_bound_candidate(data = final_conditions, pop_val = c(0.15, 0.15))
bound215 <- scan_bound_candidate(data = final_conditions, pop_val = c(0.20, 0.15))


# Run the GORICA functions
load_data_gorica(bound1515, gorica_files)
load_data_gorica(bound215, gorica_files)


