

# THIS IS A VERY IMPORTANT STEP AS IT DETERMINES HOW MANY CORES YOU WILL UTILIZE FOR THE PARALLEL PROCESS.
# DETERMINE YOURSELF HOW MANY CORES YOU WANT TO USE BY INCREASING OR DECREASING THE VALUE AFTER THE FUNCTION.
ncores <- detectCores() - leave_n_cores_free


cl <- makeCluster(ncores) 
registerDoParallel(cl)



# Process each condition sequentially
for (condition_idx in 1:nrow(meta_conditions2)) {
  
  # Extract condition-specific details
  data_path <- meta_conditions2$load_data_path[condition_idx]
  u_seed <- meta_conditions2$u.seed[condition_idx]
  save_dir <- meta_conditions2$save_gorica_path[condition_idx]
  nsim <- meta_conditions2$nsim[condition_idx]
  jags_file <- meta_conditions2$jags_file_path[condition_idx]
  
  # List files for the current condition
  files <- list.files(data_path, full.names = TRUE, pattern = ".*\\.dat")
  files <- files[grepl(".*-(\\d+)\\.dat$", basename(files))]
  file_order <- as.numeric(sub(".*-(\\d+)\\.dat$", "\\1", basename(files)))
  files <- files[order(file_order)]
  
  if (length(files) < nsim) {
    stop(paste("Not enough datasets for condition", condition_idx))
  }
  
  foreach(dataset_idx = 1:nsim, 
          .packages = c("rjags", "coda", "restriktor", "tidyverse")) %dopar% {
            
            seed <- (u_seed + condition_idx - 1) * 1000000 + dataset_idx * 1000
            set.seed(seed)
            
            file_path <- files[dataset_idx]
            temp_data <- read.table(file_path, header = TRUE)
            
            if (!"ppn" %in% colnames(temp_data)) {
              stop("Column 'ppn' is missing.")
            }
            
            jagsdata <- as.matrix(temp_data[, 3:4])
            ppn <- unique(temp_data[, "ppn"])
            nts <- sapply(ppn, function(p) sum(temp_data[, "ppn"] == p))
            startseries <- cumsum(c(1, nts[-length(nts)]))
            endseries <- cumsum(nts)
            np <- length(ppn)
            
            jags_data <- list(
              y = jagsdata,
              np = np,
              startseries = startseries,
              endseries = endseries
            )
            
            
            rm(temp_data, jagsdata)
            
            model <- jags.model(
              file = jags_file,
              data = jags_data,
              inits = NULL,
              n.chains = 2,
              n.adapt = 4000
            )
            
            update(model, n.iter = 6000)
            samples <- coda.samples(model,
                                    variable.names = c("bmu", "bvar", "b", "Sigma"),
                                    n.iter = 10000, thin = 10)
            
            ##### Step 2: Extracting VAR and standardizing results #########################
            
            posterior_matrix <- as.matrix(samples)
            
            rm(samples)
            gc()
            
            
            ###################################################################
            ns <- 2000 # number of simulations
            H<-array(NA,dim=c(2,2,ns,np)) # matrix for storing Phi, aka regression parameters
            Q<-array(NA,dim=c(4,ns))  #vector for storing Sigma, aka innovation matrix
            
            for (pp in 1:np){
              
              for (sample in 1:ns) {
                H[1,1,sample,pp] = posterior_matrix[sample,paste("b[",pp,",5]", sep = "")]#autoregressive y1
                H[1,2,sample,pp] = posterior_matrix[sample,paste("b[",pp,",3]", sep = "")]#cross-lagged effect of y2t-1 on y1t, so y1t regressed on y2t-1, Y1 = Y, Y2 = X
                H[2,1,sample,pp] = posterior_matrix[sample,paste("b[",pp,",4]", sep = "")]#cross-lagged y1t-1 on y2t Y2 = Y, Y1 = X
                H[2,2,sample,pp] = posterior_matrix[sample,paste("b[",pp,",6]", sep = "")] #autoregressive y2
                
              }  ###read out the relevant coefficients and put into matrix.
            }
            
            for (sample in 1:ns) {
              Q[1,sample] = posterior_matrix[sample,"Sigma[1,1]"]
              Q[2,sample] = posterior_matrix[sample,"Sigma[1,2]"] 
              Q[3,sample] = posterior_matrix[sample,"Sigma[2,1]"] 
              Q[4,sample] = posterior_matrix[sample,"Sigma[2,2]"] 
            }
            
            ################################ functions######################################
            Id = matrix(c(1,0,0,0, ##identity matrix
                          0,1,0,0,
                          0,0,1,0,
                          0,0,0,1),4,4)
            
            
            ##function to obtain person-specific covariance matrices for each iteration of the mcmc sampler
            VAR <- function(ID, Phi,Res){ 
              VECSIGMA = solve(ID-(Phi%x%Phi)) %*% Res
              SIGMA = matrix(VECSIGMA,2,2)
              return(SIGMA)
            }
            
            ##function to obtain person-specific variances for each iteration of the mcmc sampler
            GETS1S2 <- function(ID, Phi,Res){
              VECSIGMA = solve(ID-(Phi%x%Phi)) %*% Res
              S1S2 =c(VECSIGMA[1],VECSIGMA[4])
              return(S1S2)
            }
            
            ##function to obtain person-specific standardized coefficients for each iteration of the mcmc sampler
            STAN <- function(b, Sy,Sx){
              beta = b*Sx/Sy
              names(beta)="standardized coefficient"
              return(beta)
            }
            
            ####################################################################
            ##########calculate within person variances and standard deviations#####
            SIGMAS <-array(0, dim=c(2, 2, ns, np)) ###storage array for all the covariance matrices
            
            for(pp in 1:np){ ##calculate person-specific covariance matrices
              for (sample in 1:ns){
                SIGMAS[, ,sample, pp] = VAR(Id,H[ , ,sample, pp],Q[ ,sample])
              }
            }
            
            vars <-array(0, dim = c(ns, 2, np)) ##storage array for the variances
            colnames(vars)=c("S1","S2") 
            
            for(pp in 1:np){ ##calculate person-specific variances
              for (sample in 1:ns){
                vars[sample,1:2,pp] = GETS1S2(Id,H[,,sample,pp],Q[,sample])
              }
            }
            
            # Given sigs is a 3D 
            sigs <- sqrt(vars)  # Assuming this is how sigs was created
            
            # Create a logical array indicating NaN values
            is_nan <- is.nan(sigs)
            
            # Find which indices have NaN values
            nan_indices <- which(is_nan, arr.ind = TRUE)
            
            # Check if any NaN values exist
            if(nrow(nan_indices) > 0) {
              
              # Get unique positions in the 3rd dimension that have NaNs
              unique_third_dim <- unique(nan_indices[,3])
              cat("NaN values found in these third dimension indices:", unique_third_dim, "\n")
              
              # For each problematic third dimension, print the specific locations
              for(dim3 in unique_third_dim) {
                cat("\nFor third dimension index", dim3, ":\n")
                dim3_indices <- nan_indices[nan_indices[,3] == dim3, , drop=FALSE]
                print(dim3_indices)
                
              }
            } else {
              cat("No NaN values found in sigs.\n")
            }
            
            ##storage for the within-person standardized coefficients
            Sphi <- array(0,dim =c(ns ,2, np)) 
            colnames(Sphi) <- c("B_Y12","B_Y21")
            
            ########### FILTER OUT SAMPLES FROM THE POSTERIOR THAT CAUSE NUMERICAL INSTABILITY ################
            
            # Create containers for cleaned objects
            cleaned_H     <- vector("list", np)
            cleaned_SIGMAS<- vector("list", np)
            cleaned_vars  <- vector("list", np)
            cleaned_sigs  <- vector("list", np)
            cleaned_Sphi  <- vector("list", np)
            
            # Loop over persons to filter out iterations with negative variances
            for(pp in 1:np){
              # Get indices of iterations with both variances >= 0
              good_idx <- which(vars[,1,pp] >= 0 & vars[,2,pp] >= 0)
              cat("Person", pp, ": kept", length(good_idx), "of", ns, "iterations\n")
              
              # Subset the arrays for these "good" iterations
              cleaned_H[[pp]]      <- H[,, good_idx, pp]
              cleaned_SIGMAS[[pp]] <- SIGMAS[,, good_idx, pp]
              cleaned_vars[[pp]]   <- vars[good_idx, , pp]
              cleaned_sigs[[pp]]   <- sqrt(cleaned_vars[[pp]])
              
              # Compute the standardized coefficients using the cleaned objects
              n_good <- length(good_idx)
              Sphi_person <- matrix(NA, nrow = n_good, ncol = 2)
              colnames(Sphi_person) <- c("B_Y12", "B_Y21")
              
              for(i in seq_along(good_idx)){
                Sphi_person[i,1] <- STAN(b = cleaned_H[[pp]][1,2,i],
                                         Sy = cleaned_sigs[[pp]][i,1],
                                         Sx = cleaned_sigs[[pp]][i,2])
                
                Sphi_person[i,2] <- STAN(b = cleaned_H[[pp]][2,1,i],
                                         Sy = cleaned_sigs[[pp]][i,2],
                                         Sx = cleaned_sigs[[pp]][i,1])
              }
              cleaned_Sphi[[pp]] <- Sphi_person
            }
            
            # Now use the cleaned_Sphi objects for further person-specific analysis:
            cov_matrices_clean <- vector("list", np)
            person_means_clean <- matrix(NA, nrow = np, ncol = 2)
            colnames(person_means_clean) <- c("B_Y12", "B_Y21")
            
            for(pp in 1:np) {
              cov_matrices_clean[[pp]] <- cov(cleaned_Sphi[[pp]])
              person_means_clean[pp,] <- colMeans(cleaned_Sphi[[pp]], na.rm = TRUE)
            }
            
            # And then overall model summaries across persons:
            overall_mean_clean <- colMeans(person_means_clean, na.rm = TRUE)
            overall_cov_clean  <- cov(person_means_clean)
            
            # Finally, build final result lists:
            model_std <- list(
              cov = overall_cov_clean,
              est = overall_mean_clean
            )
            
            # For person-specific standardized estimates, you could create:
            person_std <- vector("list", np)
            for(pp in 1:np) {
              person_std[[pp]] <- list(
                cov = cov_matrices_clean[[pp]],
                est = person_means_clean[pp,]
              )
            }
            
            
            ####################### Step 3: Process GORICA results  ########################
            
            seed <- (u_seed + condition_idx - 1) * 1000000 + dataset_idx * 1000
            set.seed(seed)
            
            gorica_m_set1 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("abs(B_Y12) > abs(B_Y21)"), Heq = FALSE)
            gorica_m_set1 <- matrix(
              c(
                gorica_m_set1$result[1, 2], gorica_m_set1$result[1, 3], gorica_m_set1$result[1, 4], gorica_m_set1$result[1, 7],
                gorica_m_set1$result[2, 2], gorica_m_set1$result[2, 3], gorica_m_set1$result[2, 4], gorica_m_set1$result[2, 7]
              ),
              nrow = 1, byrow = TRUE
            )
            colnames(gorica_m_set1) <- c("h1_log", "h1_pen", "h1_gor", "h1_wgt", "hc_log", "hc_pen", "hc_gor", "hc_wgt")
            
            gorica_m_set2 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("abs(B_Y12) > abs(B_Y21)"), Heq = TRUE)
            gorica_m_set2 <- matrix(
              c(
                gorica_m_set2$result[1, 2], gorica_m_set2$result[1, 3], gorica_m_set2$result[1, 4], gorica_m_set2$result[1, 7],
                gorica_m_set2$result[2, 2], gorica_m_set2$result[2, 3], gorica_m_set2$result[2, 4], gorica_m_set2$result[2, 7],
                gorica_m_set2$result[3, 2], gorica_m_set2$result[3, 3], gorica_m_set2$result[3, 4], gorica_m_set2$result[3, 7]
              ),
              nrow = 1, byrow = TRUE
            )
            colnames(gorica_m_set2) <- c("h0_log", "h0_pen", "h0_gor", "h0_wgt", "h1_log", "h1_pen", "h1_gor", "h1_wgt", "hc_log", "hc_pen", "hc_gor", "hc_wgt")
            
            gorica_m_set3 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("-0.05 < abs(B_Y12) - abs(B_Y21) < 0.05"), Heq = FALSE)
            gorica_m_set3 <- matrix(
              c(
                gorica_m_set3$result[1, 2], gorica_m_set3$result[1, 3], gorica_m_set3$result[1, 4], gorica_m_set3$result[1, 7],
                gorica_m_set3$result[2, 2], gorica_m_set3$result[2, 3], gorica_m_set3$result[2, 4], gorica_m_set3$result[2, 7]
              ),
              nrow = 1, byrow = TRUE
            )
            colnames(gorica_m_set3) <- c("ha1_log", "ha1_pen", "ha1_gor", "ha1_wgt", "ha1c_log", "ha1c_pen", "ha1c_gor", "ha1c_wgt")
            
            gorica_m_set4 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("-0.01 < abs(B_Y12) - abs(B_Y21) < 0.01"), Heq = FALSE)
            gorica_m_set4 <- matrix(
              c(
                gorica_m_set4$result[1, 2], gorica_m_set4$result[1, 3], gorica_m_set4$result[1, 4], gorica_m_set4$result[1, 7],
                gorica_m_set4$result[2, 2], gorica_m_set4$result[2, 3], gorica_m_set4$result[2, 4], gorica_m_set4$result[2, 7]
              ),
              nrow = 1, byrow = TRUE
            )
            colnames(gorica_m_set4) <- c("ha2_log", "ha2_pen", "ha2_gor", "ha2_wgt", "ha2c_log", "ha2c_pen", "ha2c_gor", "ha2c_wgt")
            
            gorica_results_list <- vector("list", length = length(np))
            
            for (person in 1:length(person_std)) {
              # Set unique seed for person-level GORICA
              seed <- (u_seed + condition_idx - 1) * 1000000 + dataset_idx * 1000 + person
              set.seed(seed)
              
              # First goric function
              gorica_p_set1 <- tryCatch({
                result <- goric(object = person_std[[person]]$est, VCOV = person_std[[person]]$cov, hypotheses = list("abs(B_Y12) > abs(B_Y21)"), Heq = FALSE)
                matrix(
                  c(
                    result$result[1, 2], result$result[1, 3], result$result[1, 4], result$result[1, 7],
                    result$result[2, 2], result$result[2, 3], result$result[2, 4], result$result[2, 7]
                  ),
                  nrow = 1, byrow = TRUE,
                  dimnames = list(NULL, c("h1_log", "h1_pen", "h1_gor", "h1_wgt", "hc_log", "hc_pen", "hc_gor", "hc_wgt"))
                )
              }, error = function(e) {
                NULL
              })
              
              # Second goric function
              gorica_p_set2 <- tryCatch({
                result <- goric(object = person_std[[person]]$est, VCOV = person_std[[person]]$cov, hypotheses = list("abs(B_Y12) > abs(B_Y21)"), Heq = TRUE)
                matrix(
                  c(
                    result$result[1, 2], result$result[1, 3], result$result[1, 4], result$result[1, 7],
                    result$result[2, 2], result$result[2, 3], result$result[2, 4], result$result[2, 7],
                    result$result[3, 2], result$result[3, 3], result$result[3, 4], result$result[3, 7]
                  ),
                  nrow = 1, byrow = TRUE,
                  dimnames = list(NULL, c("h0_log", "h0_pen", "h0_gor", "h0_wgt", "h1_log", "h1_pen", "h1_gor", "h1_wgt", "hc_log", "hc_pen", "hc_gor", "hc_wgt"))
                )
              }, error = function(e) {
                NULL
              })
              
              # Third goric function
              gorica_p_set3 <- tryCatch({
                result <- goric(object = person_std[[person]]$est, VCOV = person_std[[person]]$cov, hypotheses = list("-0.05 < abs(B_Y12) - abs(B_Y21) < 0.05"), Heq = FALSE)
                matrix(
                  c(
                    result$result[1, 2], result$result[1, 3], result$result[1, 4], result$result[1, 7],
                    result$result[2, 2], result$result[2, 3], result$result[2, 4], result$result[2, 7]
                  ),
                  nrow = 1, byrow = TRUE,
                  dimnames = list(NULL, c("ha1_log", "ha1_pen", "ha1_gor", "ha1_wgt", "ha1c_log", "ha1c_pen", "ha1c_gor", "ha1c_wgt"))
                )
              }, error = function(e) {
                NULL
              })
              
              # Fourth goric function
              gorica_p_set4 <- tryCatch({
                result <- goric(object = person_std[[person]]$est, VCOV = person_std[[person]]$cov, hypotheses = list("-0.01 < abs(B_Y12) - abs(B_Y21) < 0.01"), Heq = FALSE)
                matrix(
                  c(
                    result$result[1, 2], result$result[1, 3], result$result[1, 4], result$result[1, 7],
                    result$result[2, 2], result$result[2, 3], result$result[2, 4], result$result[2, 7]
                  ),
                  nrow = 1, byrow = TRUE,
                  dimnames = list(NULL, c("ha2_log", "ha2_pen", "ha2_gor", "ha2_wgt", "ha2c_log", "ha2c_pen", "ha2c_gor", "ha2c_wgt"))
                )
              }, error = function(e) {
                NULL
              })
              
              gorica_results_list[[person]] <- list(
                set1 = gorica_p_set1,
                set2 = gorica_p_set2,
                set3 = gorica_p_set3,
                set4 = gorica_p_set4
              )
            }
            
            gc()
            
            
            ############## Step 4: Save the dynamically named object #######################        
            
            final_output_name <- paste0("gorica_output_con_", meta_conditions2$condition[condition_idx], "_sim_", dataset_idx)
            assign(final_output_name, list(
              model_gorica = list(set1 = gorica_m_set1, set2 = gorica_m_set2, set3 = gorica_m_set3, set4 = gorica_m_set4),
              person_gorica = gorica_results_list,
              model_std = model_std,
              person_std = lapply(person_std, function(x) list(cov = x$cov, est = x$est))
            ))
            
            
            gorica_file_path <- file.path(
              meta_conditions2$save_gorica_path[condition_idx],
              paste0("gorica_output_con_", meta_conditions2$condition[condition_idx], "_sim_", dataset_idx, ".Rdata")
            )
            save(list = final_output_name, file = gorica_file_path)
            
            rm(list = final_output_name)
            rm(gorica_results_list, model_std, person_std, gorica_m_set1, gorica_m_set2, gorica_m_set3, gorica_m_set4,
               gorica_p_set1, gorica_p_set2, gorica_p_set3, gorica_p_set4)
            gc()
          }
}
stopCluster(cl)
gc()


