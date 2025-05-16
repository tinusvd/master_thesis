
meta_conditions <- read_csv2("./data_archive/meta_files/meta_conditions.csv")

simulate_condition <- function(
    condition = 0,
    location = "",
    head_seed = 0,
    n_datasets = 0,
    np = 0,  # Number of subjects
    tt = 0,  # Number of time points
    by12 = 0,
    by21 = 0,
    ny = 2,   # Number of indicators
    ne = 2,   # Number of latent states
    nr = 6,   # Number of random parameters
    parmu = c(.4, by12, by21, .4, 3, 2), # Fixed effects for Phi and random means
    parcov = matrix(c(
      .01, 0, 0, 0, 0, 0,
      0, .01, 0, 0, 0, 0,
      0, 0, .01, 0, 0, 0,
      0, 0, 0, .01, 0, 0,
      0, 0, 0, 0, .25, 0,
      0, 0, 0, 0, 0, .25
    ), 6, 6, byrow = TRUE),
    Q = matrix(c(1, 0, 0, 1), ne, ne, byrow = TRUE), # Innovation covariance matrix
    max_stationary_attempts = 150  # Maximum attempts to draw a stationary parameter set
) {
  # Helper function to check stationarity of a 2x2 matrix
  is_stationary <- function(mat) {
    eigenvalues <- eigen(mat)$values
    return(max(abs(eigenvalues)) < 1)
  }
  
  # Ensure the directory exists
  if (!dir.exists(location)) {
    stop("The directory specified in 'location' does not exist.")
  }
  
  # Create log files for seeds and stationarity checks
  seed_log_file <- paste0(seed_location, "condition_", condition, "_seeds.log")
  stationarity_log_file <- paste0(seed_location, "condition_", condition, "_stationarity.log")
  
  set.seed(head_seed)
  seeds <- sample(1:1e6, n_datasets) # Generate unique seeds for each dataset
  
  # Log initial seeds
  write(paste("Initial seeds for condition", condition, ":", 
              paste(seeds, collapse = ", ")), 
        file = seed_log_file)
  
  successful_datasets <- 0
  dataset_number <- 1
  
  while (successful_datasets < n_datasets) {
    tryCatch({
      set.seed(seeds[dataset_number])
      
      # Storage matrices for random effects and autoregressive matrices
      d <- matrix(0, np, ny)
      H <- matrix(0, ne * np, ne)
      
      # Storage for stationarity attempts for each subject
      stationarity_attempts <- numeric(np)
      
      # Generate random effects for each subject with stationarity check
      for (j in 1:np) {
        stationary <- FALSE
        attempts <- 0
        while (!stationary && attempts < max_stationary_attempts) {
          muphi <- mvrnorm(1, mu = parmu, Sigma = parcov)
          # Create the 2x2 autoregressive matrix H_j from the first 4 parameters
          H_j <- matrix(c(
            muphi[1], muphi[2],
            muphi[3], muphi[4]
          ), ne, ne, byrow = TRUE)
          
          stationary <- is_stationary(H_j)
          attempts <- attempts + 1
        }
        
        # Record the number of attempts for subject j
        stationarity_attempts[j] <- attempts
        
        if (!stationary) {
          stop(paste("Failed to generate stationary parameters for subject", j,
                     "after", max_stationary_attempts, "attempts"))
        }
        
        H[(j * ne - 1):(j * ne), 1:ne] <- H_j
        d[j, 1:ny] <- muphi[5:6]
      }
      
      # Log stationarity attempts for the current dataset
      write(paste("Dataset", successful_datasets + 1, "- Stationarity attempts by subject:",
                  paste(stationarity_attempts, collapse = ", "),
                  "- Average attempts:", mean(stationarity_attempts)),
            file = stationarity_log_file, append = TRUE)
      
      # Initialize covariance matrices using the kronecker formulation
      Id <- diag(ne^2)
      covmat <- matrix(0, ne * np, ne)
      
      for (j in 1:np) {
        cvvec <- solve(Id - kronecker(H[(j * ne - 1):(j * ne), 1:ne], 
                                      H[(j * ne - 1):(j * ne), 1:ne])) %*% c(Q)
        covmat[(j * ne - 1):(j * ne), 1:ne] <- matrix(cvvec, ne, ne)
      }
      
      # Simulate data
      ydata <- matrix(0, tt * np, ny)
      zdata <- matrix(0, tt * np, ne)
      ppn <- rep(1:np, each = tt)
      nt <- rep(1:tt, np)
      
      # Simulate for time t = 1
      for (j in 1:np) {
        ydata[(j - 1) * tt + 1, 1:ny] <- mvrnorm(1, d[j, 1:ny], 
                                                 covmat[(j * ne - 1):(j * ne), 1:ne])
        zdata[(j - 1) * tt + 1, 1:ne] <- ydata[(j - 1) * tt + 1, 1:ny] - d[j, 1:ny]
      }
      
      # Simulate for remaining time points
      for (j in 1:np) {
        for (t in 2:tt) {
          inno <- mvrnorm(1, mu = rep(0, ne), Sigma = Q)
          zdata[(j - 1) * tt + t, 1:ne] <- H[(j * ne - 1):(j * ne), 1:ne] %*% 
            zdata[(j - 1) * tt + t - 1, 1:ne] + inno
          ydata[(j - 1) * tt + t, 1:ny] <- zdata[(j - 1) * tt + t, 1:ne] + d[j, 1:ny]
        }
      }
      
      # Combine data into a single matrix with specified column names
      data <- cbind(ppn, nt, ydata)
      colnames(data) <- c("ppn", "nt", "Y1", "Y2")
      
      # Save the dataset as a .dat file with the current dataset number
      file_name <- paste0(location, "/condition_", condition, "-", successful_datasets + 1, ".dat")
      write.table(data, file = file_name, row.names = FALSE, col.names = TRUE, sep = " ")
      
      # Log the successful dataset and seed used
      write(paste("Dataset", successful_datasets + 1, "generated using seed:", seeds[dataset_number]), 
            file = seed_log_file, append = TRUE)
      
      successful_datasets <- successful_datasets + 1
      message(sprintf("Successfully generated dataset %d for condition %d using seed %d", 
                      successful_datasets, condition, seeds[dataset_number]))
      
    }, error = function(e) {
      message(sprintf("Error in condition %d, attempt %d with seed %d: %s\nContinuing with next attempt...", 
                      condition, dataset_number, seeds[dataset_number], e$message))
      # Log the error and the seed that caused it
      write(paste("Error in attempt", dataset_number, "with seed:", seeds[dataset_number], 
                  "- Error message:", e$message), 
            file = seed_log_file, append = TRUE)
    })
    
    dataset_number <- dataset_number + 1
    
    # Guard against infinite loops by adding additional seeds if necessary
    if (dataset_number > length(seeds)) {
      new_seeds <- sample(1:1e6, n_datasets)
      warning(sprintf("Ran out of seeds after %d successful datasets. Adding more seeds.", successful_datasets))
      write(paste("\nAdditional seeds added:", paste(new_seeds, collapse = ", ")), 
            file = seed_log_file, append = TRUE)
      seeds <- c(seeds, new_seeds)
    }
  }
  
  cat("All datasets successfully saved in:", location, "\n")
  cat("Seed log saved to:", seed_log_file, "\n")
  cat("Stationarity log saved to:", stationarity_log_file, "\n")
}

seed_location <-  paste0(base_path, "logs/")

# Main loop for conditions
for(i in c(1:48, 61:68)) {
  message(sprintf("\nStarting condition %d", meta_conditions$condition[i]))
  simulate_condition(
    condition = meta_conditions$condition[i],
    location = paste0(load_directory, "/condition_", meta_conditions$condition[i]),
    head_seed = meta_conditions$data_generation_seed[i],
    n_datasets = meta_conditions$nsim[i],
    np = meta_conditions$n[i],
    tt = meta_conditions$t[i],
    by12 = as.numeric(meta_conditions$b_y12[i]),
    by21 = as.numeric(meta_conditions$b_y21[i])
  )
}
