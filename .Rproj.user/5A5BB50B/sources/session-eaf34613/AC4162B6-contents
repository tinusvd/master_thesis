
library(rjags)
library(coda)
library(restriktor)
library(tidyverse)


temp_data <- read.table("./empirical_example/data/condition_9998-1.dat", header = T)


u_seed <- 9998
set.seed(u_seed)

jagsdata <- as.matrix(temp_data[, 4:5])
ppn <- unique(temp_data[, "ppn"])
nts <- sapply(ppn, function(p) sum(temp_data[, "ppn"] == p))
startseries <- cumsum(c(1, nts[-length(nts)]))
endseries <- cumsum(nts)
np <- length(ppn)

# Extract one X value per subject
subject_x <- numeric(np)
for (i in 1:np) {
  # Get the X value for this subject (should be constant for each subject)
  subject_x[i] <- unique(temp_data[temp_data$ppn == ppn[i], "X"])
}



jags_data <- list(
  y = jagsdata,
  x = subject_x,
  np = np,
  startseries = startseries,
  endseries = endseries
)

model <- jags.model(
  file = "./empirical_example/bivar_var1_covariate.jags",
  data = jags_data,
  inits = NULL,
  n.chains = 2,
  n.adapt = 4000
)

update(model, n.iter = 6000)

vars_to_sample <- c(
  # residual‐level
  "Ipre", "Sigma",
  
  # between‐person variances for each b[j,k]
  "bvar1","bvar2","bvar3","bvar4","bvar5","bvar6",
  
  # population‐level regressions
  "intb1","intb2","intb3","intb4","intb5","intb6",
  "c1_x","c2_x","c3_x","c4_x","c5_x","c6_x",
  
  # all subject‐level effects
  "b"
)

samples <- coda.samples(
  model,
  variable.names = vars_to_sample,
  n.iter = 25000,
  thin   = 10
)



######################### DIAGNOSTICS ##########################################
posterior_matrix <- samples

bvar <- grep(varnames(samples), pattern = "bvar")
intb <- grep(varnames(samples), pattern = "intb")
c_x <- grep(varnames(samples), pattern = "_x")
c_x5 <- grep(varnames(samples), pattern = "c5_x")
c_x6 <- grep(varnames(samples), pattern = "c6_x")

pars <- c(bmu, bvar, intb, c_x)
summary_pars <- c(bmu, intb, c_x)

diagnostic_pars <- c(intb, c_x5, c_x6, bvar)


traceplot(samples[, diagnostic_pars])

summary(samples[,pars])

pdf("traceplots.pdf", width = 8, height = 6)

dev.off()
###################### STANDARDIZATION #########################################

posterior_matrix <- as.matrix(samples)

###################################################################
# STEP 1: Extract parameters from posterior samples
###################################################################
ns <- 2000 # number of simulations
H <- array(NA, dim=c(2, 2, ns, np)) # matrix for storing Phi, aka regression parameters
Q <- array(NA, dim=c(4, ns))  # vector for storing Sigma, aka innovation matrix

# Read in the regression parameters
for (pp in 1:np) {
  for (sample in 1:ns) {
    H[1,1,sample,pp] = posterior_matrix[sample,paste("b[",pp,",3]", sep = "")]  # autoregressive y1
    H[1,2,sample,pp] = posterior_matrix[sample,paste("b[",pp,",5]", sep = "")]  # cross-lagged effect of y2t-1 on y1t 
    H[2,1,sample,pp] = posterior_matrix[sample,paste("b[",pp,",6]", sep = "")]  # cross-lagged effect of y1t-1 on y2t
    H[2,2,sample,pp] = posterior_matrix[sample,paste("b[",pp,",4]", sep = "")]  # autoregressive y2
  }
}

# Read in the innovation matrix parameters
for (sample in 1:ns) {
  Q[1,sample] = posterior_matrix[sample,"Sigma[1,1]"]
  Q[2,sample] = posterior_matrix[sample,"Sigma[1,2]"] 
  Q[3,sample] = posterior_matrix[sample,"Sigma[2,1]"] 
  Q[4,sample] = posterior_matrix[sample,"Sigma[2,2]"] 
}

# Define helper functions
Id = matrix(c(1,0,0,0,  # identity matrix
              0,1,0,0,
              0,0,1,0,
              0,0,0,1), 4, 4)

# Function to obtain person-specific covariance matrices for each iteration
VAR <- function(ID, Phi, Res){ 
  VECSIGMA = solve(ID-(Phi%x%Phi)) %*% Res
  SIGMA = matrix(VECSIGMA, 2, 2)
  return(SIGMA)
}

# Function to obtain person-specific variances for each iteration
GETS1S2 <- function(ID, Phi, Res){
  VECSIGMA = solve(ID-(Phi%x%Phi)) %*% Res
  S1S2 = c(VECSIGMA[1], VECSIGMA[4])
  return(S1S2)
}

# Function to obtain person-specific standardized coefficients for each iteration
STAN <- function(b, Sy, Sx){
  beta = b*Sx/Sy
  names(beta) = "standardized coefficient"
  return(beta)
}

###################################################################
# STEP 2: Calculate within-person covariance matrices and check validity
###################################################################
SIGMAS <- array(0, dim=c(2, 2, ns, np))  # storage array for all the covariance matrices

# Calculate person-specific covariance matrices for each iteration
for(pp in 1:np) {
  for (sample in 1:ns){
    SIGMAS[, , sample, pp] = VAR(Id, H[, , sample, pp], Q[, sample])
  }
}

# Calculate person-specific variances for each iteration
vars <- array(0, dim=c(ns, 2, np))
colnames(vars) = c("S1", "S2") 

for(pp in 1:np) {
  for (sample in 1:ns){
    vars[sample, 1:2, pp] = GETS1S2(Id, H[, , sample, pp], Q[, sample])
  }
}

# Calculate standard deviations
sigs <- sqrt(vars)

# Check for NaN values
is_nan <- is.nan(sigs)
nan_indices <- which(is_nan, arr.ind=TRUE)
if(nrow(nan_indices) > 0) {
  unique_third_dim <- unique(nan_indices[,3])
  cat("NaN values found in these third dimension indices:", unique_third_dim, "\n")
  for(dim3 in unique_third_dim) {
    cat("\nFor third dimension index", dim3, ":\n")
    dim3_indices <- nan_indices[nan_indices[,3] == dim3, , drop=FALSE]
    print(dim3_indices)
  }
} else {
  cat("No NaN values found in sigs.\n")
}

###################################################################
# STEP 3: Filter valid iterations and standardize cross-lagged parameters
###################################################################
# Identify valid iterations for each person (positive variances)
valid_iter_by_person <- vector("list", np)
for(pp in 1:np) {
  valid_iter_by_person[[pp]] <- which(vars[,1,pp] >= 0 & vars[,2,pp] >= 0)
  cat("Person", pp, ": kept", length(valid_iter_by_person[[pp]]), "of", ns, "iterations\n")
}

# We'll use all iterations and handle missing values with na.rm=TRUE
# But keep track of how many valid observations per iteration for information
iter_valid_count <- rep(0, ns)
for(pp in 1:np) {
  iter_valid_count[valid_iter_by_person[[pp]]] <- iter_valid_count[valid_iter_by_person[[pp]]] + 1
}
cat("Number of iterations with data from all", np, "persons:", sum(iter_valid_count == np), "\n")
cat("Number of iterations with data from at least one person:", sum(iter_valid_count > 0), "\n")

# Standardize cross-lagged parameters for each person at each valid iteration
std_params <- array(NA, dim=c(ns, 2, np))  # [iteration, parameter, person]
colnames(std_params) <- c("B_Y12", "B_Y21")

for(pp in 1:np) {
  for(i in valid_iter_by_person[[pp]]) {
    # B_Y12: Cross-lagged effect of Y2 on Y1
    std_params[i, 1, pp] <- STAN(
      b = H[1, 2, i, pp],  # Unstandardized parameter
      Sy = sigs[i, 1, pp],  # SD of Y1
      Sx = sigs[i, 2, pp]   # SD of Y2
    )
    
    # B_Y21: Cross-lagged effect of Y1 on Y2
    std_params[i, 2, pp] <- STAN(
      b = H[2, 1, i, pp],  # Unstandardized parameter  
      Sy = sigs[i, 2, pp],  # SD of Y2
      Sx = sigs[i, 1, pp]   # SD of Y1
    )
  }
}

###################################################################
# STEP 4: Calculate iteration-level means across persons
###################################################################

idx1 <- which(subject_x == 1)  # persons with X = 1
idx0 <- which(subject_x == 0)  # persons with X = 0

# For all iterations (even those with some missing data), calculate mean standardized parameter across all persons
iteration_means <- matrix(NA, nrow = ns, ncol = 4,
                          dimnames = list(NULL,
                                          c("B_Y12X1","B_Y21X1","B_Y12X0","B_Y21X0")))

for (i in 1:ns) {
  # only bother if at least one person in either group contributed valid data
  if (iter_valid_count[i] == 0) next
  
  # extract all per‐person standardized parameters for this iteration
  B12_all <- std_params[i, 1, ]   # Y2→Y1
  B21_all <- std_params[i, 2, ]   # Y1→Y2
  
  # subgroup means
  iteration_means[i, "B_Y12X1"] <- mean(B12_all[idx1], na.rm = TRUE)
  iteration_means[i, "B_Y21X1"] <- mean(B21_all[idx1], na.rm = TRUE)
  
  iteration_means[i, "B_Y12X0"] <- mean(B12_all[idx0], na.rm = TRUE)
  iteration_means[i, "B_Y21X0"] <- mean(B21_all[idx0], na.rm = TRUE)
}

###################################################################
# STEP 5: Calculate person-specific means
###################################################################
person_means <- matrix(NA, nrow=np, ncol=2)
colnames(person_means) <- c("B_Y12", "B_Y21")

for(pp in 1:np) {
  # Get valid iterations for this person
  valid_idx <- valid_iter_by_person[[pp]]
  
  # Calculate mean of standardized parameters across valid iterations
  person_means[pp, 1] <- mean(std_params[valid_idx, 1, pp], na.rm=TRUE)
  person_means[pp, 2] <- mean(std_params[valid_idx, 2, pp], na.rm=TRUE)
}

###################################################################
# STEP 6: Calculate model-level estimates
###################################################################
# Model estimate: Average of iteration-level means (skipping iterations with no valid data)
valid_iteration_indices <- which(!is.na(iteration_means[,1]) & !is.na(iteration_means[,2]) & !is.na(iteration_means[,3]) & !is.na(iteration_means[,4]))
model_estimate <- colMeans(iteration_means[valid_iteration_indices,], na.rm=TRUE)
names(model_estimate) <- c("B_Y12X1", "B_Y21X1", "B_Y12X0", "B_Y21X0")

###################################################################
# STEP 7: Calculate covariance matrices
###################################################################
# Person-specific covariance matrices of standardized parameters
person_cov_matrices <- vector("list", np)

for(pp in 1:np) {
  valid_idx <- valid_iter_by_person[[pp]]
  person_data <- matrix(NA, nrow=length(valid_idx), ncol=2)
  
  for(i in seq_along(valid_idx)) {
    iter <- valid_idx[i]
    person_data[i, 1] <- std_params[iter, 1, pp]
    person_data[i, 2] <- std_params[iter, 2, pp]
  }
  
  person_cov_matrices[[pp]] <- cov(person_data)
}

# Model-level covariance matrix of iteration-level means (using only iterations with valid data)
model_cov_matrix <- cov(iteration_means[valid_iteration_indices,], use="complete.obs")

###################################################################
# STEP 8: Create and save final results
###################################################################
person_std <- vector("list", np)
for (pp in 1:np) {
  person_std[[pp]] <- list(
    x   = subject_x[pp],           # add the covariate here
    cov = person_cov_matrices[[pp]],
    est = person_means[pp, ]
  )
}

# Create model-level results
model_std <- list(
  cov = model_cov_matrix,
  est = model_estimate
)

################################ GORICA ########################################

# Between-subject (model) level
gorica_m_set1 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("abs(B_Y12X1) > abs(B_Y21X1), abs(B_Y12X0) < abs(B_Y21X0)"), Heq = FALSE)
gorica_m_set1 <- matrix(
  c(
    gorica_m_set1$result[1, 2], gorica_m_set1$result[1, 3], gorica_m_set1$result[1, 4], gorica_m_set1$result[1, 7],
    gorica_m_set1$result[2, 2], gorica_m_set1$result[2, 3], gorica_m_set1$result[2, 4], gorica_m_set1$result[2, 7]
  ),
  nrow = 1, byrow = TRUE
)
colnames(gorica_m_set1) <- c("h1_log", "h1_pen", "h1_gor", "h1_wgt", "hc_log", "hc_pen", "hc_gor", "hc_wgt")

gorica_m_set2 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("abs(B_Y12X1) > abs(B_Y21X1), abs(B_Y12X0) < abs(B_Y21X0)"), Heq = TRUE)
gorica_m_set2 <- matrix(
  c(
    gorica_m_set2$result[1, 2], gorica_m_set2$result[1, 3], gorica_m_set2$result[1, 4], gorica_m_set2$result[1, 7],
    gorica_m_set2$result[2, 2], gorica_m_set2$result[2, 3], gorica_m_set2$result[2, 4], gorica_m_set2$result[2, 7],
    gorica_m_set2$result[3, 2], gorica_m_set2$result[3, 3], gorica_m_set2$result[3, 4], gorica_m_set2$result[3, 7]
  ),
  nrow = 1, byrow = TRUE
)
colnames(gorica_m_set2) <- c("h0_log", "h0_pen", "h0_gor", "h0_wgt", "h1_log", "h1_pen", "h1_gor", "h1_wgt", "hc_log", "hc_pen", "hc_gor", "hc_wgt")

gorica_m_set3 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("-0.05 < abs(B_Y12X1) - abs(B_Y21X1) < 0.05, -0.05 < abs(B_Y12X0) - abs(B_Y21X0) < 0.05"), Heq = FALSE)
gorica_m_set3 <- matrix(
  c(
    gorica_m_set3$result[1, 2], gorica_m_set3$result[1, 3], gorica_m_set3$result[1, 4], gorica_m_set3$result[1, 7],
    gorica_m_set3$result[2, 2], gorica_m_set3$result[2, 3], gorica_m_set3$result[2, 4], gorica_m_set3$result[2, 7]
  ),
  nrow = 1, byrow = TRUE
)
colnames(gorica_m_set3) <- c("ha1_log", "ha1_pen", "ha1_gor", "ha1_wgt", "ha1c_log", "ha1c_pen", "ha1c_gor", "ha1c_wgt")

gorica_m_set4 <- goric(object = model_std$est, VCOV = model_std$cov, hypotheses = list("-0.01 < abs(B_Y12X1) - abs(B_Y21X1) < 0.01, -0.01 < abs(B_Y12X0) - abs(B_Y21X0) < 0.01"), Heq = FALSE)
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
  seed <- (u_seed) * 10000 + u_seed * 1000 + person
  set.seed(seed)
  
  X1 <- "abs(B_Y12) > abs(B_Y21)"
  X0 <- "abs(B_Y12) < abs(B_Y21)"
  
  if(person_std[[person]][["x"]] == 1){
    hypo <- X1
  } else if (person_std[[person]][["x"]] == 0){
    hypo <- X0
  }
  
    # First goric function
    gorica_p_set1 <- tryCatch({
      result <- goric(object = person_std[[person]]$est, VCOV = person_std[[person]]$cov, hypotheses = list(hypo), Heq = FALSE)
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
      result <- goric(object = person_std[[person]]$est, VCOV = person_std[[person]]$cov, hypotheses = list(hypo), Heq = TRUE)
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



final_output <- "final_output"
assign(final_output, list(
  model_gorica = list(set1 = gorica_m_set1, set2 = gorica_m_set2, set3 = gorica_m_set3, set4 = gorica_m_set4),
  person_gorica = gorica_results_list,
  model_std = model_std,
  person_std = lapply(person_std, function(x) list(cov = x$cov, est = x$est))
))


################## CREATING DATA FRAME FOR THE OVERVIEW ########################
simulation_df <- data.frame(
  person_id = integer(),
  x = integer(),
  b_y12 = numeric(),
  b_y21 = numeric(),
  set1_h1_wgt_gor = numeric(),
  set1_hc_wgt_gor = numeric(),
  supp_set1_h1 = integer(),
  supp_set1_hc = integer(),
  set2_h0_wgt_gor = numeric(),
  set2_h1_wgt_gor = numeric(),
  set2_hc_wgt_gor = numeric(),
  supp_set2_h1 = integer(),
  supp_set2_h0 = integer(),
  supp_set2_hc = integer(),
  set3_ha1_wgt_gor = numeric(),
  set3_ha1c_wgt_gor = numeric(),
  supp_set3_ha1 = integer(),
  supp_set3_ha1c = integer(),
  set4_ha2_wgt_gor = numeric(),
  set4_ha2c_wgt_gor = numeric(),
  supp_set4_ha2 = integer(),
  supp_set4_ha2c = integer(),
  set1_ratio_h1hc = numeric(),
  set1_ratio_hch1 = numeric(),
  set2_ratio_h1h0 = numeric(),
  set2_ratio_h0h1 = numeric(),
  set2_ratio_h1hc = numeric(),
  set2_ratio_hch1 = numeric(),
  set3_ratio_ha1ha1c = numeric(),
  set3_ratio_ha1cha1 = numeric(),
  set4_ratio_ha2ha2c = numeric(),
  set4_ratio_ha2cha2 = numeric()
)

# Load the file and extract the required data
for (j in seq_along(final_output[["person_gorica"]])) {
  person <- final_output[["person_gorica"]][[j]]
  params <- final_output[["person_std"]][[j]][["est"]]
  
  # Get the X value for this person (fix: assign the actual X value)
  x_value <- person_std[[j]]$x
  
  # Extract the required values - using numeric indices to match original code
  set1 <- c(h1_wgt_gor = person[["set1"]][4], hc_wgt_gor = person[["set1"]][8])
  set2 <- c(
    h0_wgt_gor = person[["set2"]][4],
    h1_wgt_gor = person[["set2"]][8],
    hc_wgt_gor = person[["set2"]][12]
  )
  set3 <- c(ha1_wgt_gor = person[["set3"]][4], ha1c_wgt_gor = person[["set3"]][8])
  set4 <- c(ha2_wgt_gor = person[["set4"]][4], ha2c_wgt_gor = person[["set4"]][8])
  
  b_y12 <- params[[1]]
  b_y21 <- params[[2]]
  
  # Calculate ratios
  set1_ratio_h1hc <- set1["h1_wgt_gor"] / set1["hc_wgt_gor"]
  set1_ratio_hch1 <- set1["hc_wgt_gor"] / set1["h1_wgt_gor"]
  set2_ratio_h1h0 <- set2["h1_wgt_gor"] / set2["h0_wgt_gor"]
  set2_ratio_h0h1 <- set2["h0_wgt_gor"] / set2["h1_wgt_gor"]
  set2_ratio_h1hc <- set2["h1_wgt_gor"] / set2["hc_wgt_gor"]
  set2_ratio_hch1 <- set1["hc_wgt_gor"] / set1["h1_wgt_gor"]
  set3_ratio_ha1ha1c <- set3["ha1_wgt_gor"] / set3["ha1c_wgt_gor"]
  set3_ratio_ha1cha1 <- set3["ha1c_wgt_gor"] / set3["ha1_wgt_gor"]
  set4_ratio_ha2ha2c <- set4["ha2_wgt_gor"] / set4["ha2c_wgt_gor"]
  set4_ratio_ha2cha2 <- set4["ha2c_wgt_gor"] / set4["ha2_wgt_gor"]
  
  # Check if conditions hold and assign 1 or 0
  supp_set1_h1 <- as.integer(set1["h1_wgt_gor"] > set1["hc_wgt_gor"])
  supp_set1_hc <- as.integer(set1["hc_wgt_gor"] > set1["h1_wgt_gor"])
  
  supp_set2_h0 <- as.integer((set2["h0_wgt_gor"] >= set2["h1_wgt_gor"]) & (set2["h0_wgt_gor"] >= set2["hc_wgt_gor"]))
  supp_set2_h1 <- as.integer((set2["h1_wgt_gor"] > set2["h0_wgt_gor"]) & (set2["h1_wgt_gor"] > set2["hc_wgt_gor"]))
  supp_set2_hc <- as.integer((set2["hc_wgt_gor"] > set2["h0_wgt_gor"]) & (set2["hc_wgt_gor"] > set2["h1_wgt_gor"]))
  
  supp_set3_ha1c <- as.integer(set3["ha1c_wgt_gor"] > set3["ha1_wgt_gor"])
  supp_set3_ha1 <- as.integer(set3["ha1_wgt_gor"] > set3["ha1c_wgt_gor"])
  
  supp_set4_ha2c <- as.integer(set4["ha2c_wgt_gor"] > set4["ha2_wgt_gor"])
  supp_set4_ha2 <- as.integer(set4["ha2_wgt_gor"] > set4["ha2c_wgt_gor"])
  
  # Create a data frame row for this person
  person_row <- data.frame(
    person_id = j,
    x = x_value, 
    b_y12 = b_y12,
    b_y21 = b_y21,
    set1_h1_wgt_gor = set1["h1_wgt_gor"],
    set1_hc_wgt_gor = set1["hc_wgt_gor"],
    supp_set1_h1 = supp_set1_h1,
    supp_set1_hc = supp_set1_hc,
    set2_h0_wgt_gor = set2["h0_wgt_gor"],
    set2_h1_wgt_gor = set2["h1_wgt_gor"],
    set2_hc_wgt_gor = set2["hc_wgt_gor"],
    supp_set2_h1 = supp_set2_h1,
    supp_set2_h0 = supp_set2_h0,
    supp_set2_hc = supp_set2_hc,
    set3_ha1_wgt_gor = set3["ha1_wgt_gor"],
    set3_ha1c_wgt_gor = set3["ha1c_wgt_gor"],
    supp_set3_ha1 = supp_set3_ha1,
    supp_set3_ha1c = supp_set3_ha1c,
    set4_ha2_wgt_gor = set4["ha2_wgt_gor"],
    set4_ha2c_wgt_gor = set4["ha2c_wgt_gor"],
    supp_set4_ha2 = supp_set4_ha2,
    supp_set4_ha2c = supp_set4_ha2c,
    set1_ratio_h1hc = set1_ratio_h1hc,
    set1_ratio_hch1 = set1_ratio_hch1,
    set2_ratio_h1h0 = set2_ratio_h1h0,
    set2_ratio_h0h1 = set2_ratio_h0h1,
    set2_ratio_h1hc = set2_ratio_h1hc,
    set2_ratio_hch1 = set2_ratio_hch1,
    set3_ratio_ha1ha1c = set3_ratio_ha1ha1c,
    set3_ratio_ha1cha1 = set3_ratio_ha1cha1,
    set4_ratio_ha2ha2c = set4_ratio_ha2ha2c,
    set4_ratio_ha2cha2 = set4_ratio_ha2cha2
  )
  
  # Add this person's data to the main data frame
  simulation_df <- rbind(simulation_df, person_row)
}

simulation_df %>% filter(supp_set1_h1 == 1) %>% count() # N subjects with support for true hypothesis

save(simulation_df, posterior_matrix, samples, person_std, gorica_results_list, file = "/Users/mdam21/Desktop/new_thesis/example/example_files.RData")



# TABLE FOR WITHIN-SUBJECT RESULTS
# Calculate the support variable counts per dataset
support_counts <- simulation_df %>%
  select(person_id, supp_set1_h1) %>%
  # Count people with 0 and 1 values
  summarize(
    value_0_count = sum(supp_set1_h1 == 0, na.rm = TRUE),
    value_1_count = sum(supp_set1_h1 == 1, na.rm = TRUE),
    .groups = "drop"
  )


support_counts_x1 <- simulation_df %>%
  filter(x == 1) %>% 
  select(person_id, supp_set1_h1) %>%
  # Count people with 0 and 1 values
  summarize(
    value_0_count = sum(supp_set1_h1 == 0, na.rm = TRUE),
    value_1_count = sum(supp_set1_h1 == 1, na.rm = TRUE),
    .groups = "drop"
  )


# Calculate other statistical metrics
summary_stats_x1 <- simulation_df %>%
  filter(x == 1) %>% 
  group_by(supp_set1_h1) %>%
  summarise(
    # For parameters like b_y12 and b_y21 that are the same within a data_set
    n_size = n(),
    across(
      c(b_y12, b_y21), 
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        p5th = ~quantile(.x, probs = 0.05, na.rm = TRUE),
        p95th = ~quantile(.x, probs = 0.95, na.rm = TRUE)
      )
    ),
    
    # For other numeric variables
    across(
      where(is.numeric) & 
        !matches("supp") & 
        !matches("bound") &
        !matches("^n$") & 
        !matches("b_y12|b_y21") &
        !matches("person_id") &
        !matches("N_size") &
        !matches("x"),
      list(
        median = ~median(.x, na.rm = TRUE), 
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        p5th = ~quantile(.x, probs = 0.05, na.rm = TRUE),
        p95th = ~quantile(.x, probs = 0.95, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  ) 

# Calculate the support variable counts per dataset
support_counts_x0 <- simulation_df %>%
  filter(x == 0) %>% 
  select(person_id, supp_set1_h1) %>%
  # Count people with 0 and 1 values
  summarize(
    value_0_count = sum(supp_set1_h1 == 0, na.rm = TRUE),
    value_1_count = sum(supp_set1_h1 == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate other statistical metrics
summary_stats_x0 <- simulation_df %>%
  filter(x == 0) %>% 
  group_by(supp_set1_h1) %>%
  summarise(
    # For parameters like b_y12 and b_y21 that are the same within a data_set
    n_size = n(),
    across(
      c(b_y12, b_y21), 
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        p5th = ~quantile(.x, probs = 0.05, na.rm = TRUE),
        p95th = ~quantile(.x, probs = 0.95, na.rm = TRUE)
      )
    ),
    
    # For other numeric variables
    across(
      where(is.numeric) & 
        !matches("supp") & 
        !matches("bound") &
        !matches("^n$") & 
        !matches("b_y12|b_y21") &
        !matches("person_id") &
        !matches("N_size") &
        !matches("x"),
      list(
        median = ~median(.x, na.rm = TRUE), 
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        p5th = ~quantile(.x, probs = 0.05, na.rm = TRUE),
        p95th = ~quantile(.x, probs = 0.95, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  ) 


table_1 <- summary_stats_x1 %>% 
  mutate(
    support_value = supp_set1_h1,
    supp = case_when(
      supp_set1_h1 == 0 ~ "no",
      supp_set1_h1 == 1 ~ "yes",
      TRUE ~ "unknown"),
    b_y12 = format_mean_3f(b_y12_mean, b_y12_sd),
    b_y12_p = format_minmax(b_y12_p5th, b_y12_p95th),
    b_y21 = format_mean_3f(b_y21_mean, b_y21_sd),
    b_y21_p = format_minmax(b_y21_p5th, b_y21_p95th),
    gor_wgt1 = format_median(set1_h1_wgt_gor_median),
    gor_wgt1_p = format_minmax(set1_h1_wgt_gor_min, set1_h1_wgt_gor_max),
    gor_wgt2 = format_median(set1_hc_wgt_gor_median),
    gor_wgt2_p = format_minmax(set1_hc_wgt_gor_min, set1_hc_wgt_gor_max),
    gor_ratio = case_when(
      support_value == 0 ~ format_median(set1_ratio_hch1_median),
      support_value == 1 ~ format_median(set1_ratio_h1hc_median),
      TRUE ~ format_median(set1_ratio_h1hc_median)
    ),
    # Calculate different gor_ratio minmax based on support value
    # For "no" support, invert the minmax (1/p95 becomes the 5th percentile, 1/p5 becomes the 95th percentile)
    gor_ratio_p = case_when(
      support_value == 0 ~ format_minmax(set1_hc_wgt_gor_p5th, set1_hc_wgt_gor_p95th),
      support_value == 1 ~ format_minmax(set1_h1_wgt_gor_p5th, set1_h1_wgt_gor_p95th),
      TRUE ~ format_minmax(0, 0)
    )
  ) %>%
  dplyr::select(supp, n_size, b_y12, b_y21, gor_wgt1, gor_wgt1_p, gor_wgt2, gor_wgt2_p, gor_ratio, gor_ratio_p)

table_0 <- summary_stats_x0 %>% 
  mutate(
    support_value = supp_set1_h1,
    supp = case_when(
      supp_set1_h1 == 0 ~ "no",
      supp_set1_h1 == 1 ~ "yes",
      TRUE ~ "unknown"),
    b_y12 = format_mean_3f(b_y12_mean, b_y12_sd),
    b_y12_p = format_minmax(b_y12_p5th, b_y12_p95th),
    b_y21 = format_mean_3f(b_y21_mean, b_y21_sd),
    b_y21_p = format_minmax(b_y21_p5th, b_y21_p95th),
    gor_wgt1 = format_median(set1_h1_wgt_gor_median),
    gor_wgt1_p = format_minmax(set1_h1_wgt_gor_min, set1_h1_wgt_gor_max),
    gor_wgt2 = format_median(set1_hc_wgt_gor_median),
    gor_wgt2_p = format_minmax(set1_hc_wgt_gor_min, set1_hc_wgt_gor_max),
    gor_ratio = case_when(
      support_value == 0 ~ format_median(set1_ratio_hch1_median),
      support_value == 1 ~ format_median(set1_ratio_h1hc_median),
      TRUE ~ format_median(set1_ratio_h1hc_median)
    ),
    # Calculate different gor_ratio minmax based on support value
    # For "no" support, invert the minmax (1/p95 becomes the 5th percentile, 1/p5 becomes the 95th percentile)
    gor_ratio_p = case_when(
      support_value == 0 ~ format_minmax(set1_hc_wgt_gor_p5th, set1_hc_wgt_gor_p95th),
      support_value == 1 ~ format_minmax(set1_h1_wgt_gor_p5th, set1_h1_wgt_gor_p95th),
      TRUE ~ format_minmax(0, 0)
    )
  ) %>%
  dplyr::select(supp, n_size, b_y12, b_y21, gor_wgt1, gor_wgt1_p, gor_wgt2, gor_wgt2_p, gor_ratio, gor_ratio_p)


table <- rbind(table_0, table_1)

header_supp <- "$Support $H_{1}$"
header_wgt1 <- "$GORICA_{wH_1}$"
header_wgt2 <- "$GORICA_{wH_c}$"

headers <- c(" " = 2,
             "\\phi_{12}$" = 2, 
             "\\phi_{21}$" = 2)

wgt1_header <- setNames(2, header_wgt1)
wgt2_header <- setNames(2, header_wgt2)

headers <- c(
  " " = 2,                 
  "$\\phi_{12}$" = 1,
  "$\\phi_{21}$" = 1,
  setNames(2, header_wgt1),                     
  setNames(2, header_wgt2),   
  "ratio GORICA$_{w}$" = 2
)

kbl_table <- table %>% 
  kbl(
    col.names = c(
      "Support",
      "Number of subjects",     # ← insert this
      "M (SD)",  # For phi_12
      "M (SD)",  # For phi_21
      "Median", "(min, max)",  # For GORICA W1
      "Median", "(min, max)",  # For GORICA W2
      "Median", "(min, max)"
    ),
    escape = FALSE, format = "latex"
  ) %>% 
  kable_styling(full_width = TRUE) %>% 
  add_header_above(headers)


##### BETWEEN-PERSON GORICA TABLES #######

# pull out your 4 model‐level matrices
mg <- final_output$model_gorica

make_df <- function(mat, set_name) {
  # figure out the hypothesis prefixes ("h1","hc", etc)
  hyps <- unique(sub("_(log|pen|gor|wgt)$", "", colnames(mat)))

  df <- data.frame(
    hypothesis = hyps,
    logl    = as.numeric(mat[ , paste0(hyps, "_log")]),
    penalty = as.numeric(mat[ , paste0(hyps, "_pen")]),
    gorica  = as.numeric(mat[ , paste0(hyps, "_gor")]),
    weights = as.numeric(mat[ , paste0(hyps, "_wgt")]),
    set     = set_name,
    stringsAsFactors = FALSE
  )
  df
}

# 3) apply to each set and row‐bind
between_h1 <- do.call(
  rbind,
  mapply(
    FUN      = make_df,
    mat      = mg,
    set_name = names(mg),
    SIMPLIFY = FALSE
  )
)

between_h1 <- subset(between_h1, set %in% c("set1","set3","set4"))

# drop the set column
between_h1$set <- NULL
rownames(between_h1) <- NULL



#### TABLE #####

headers <- c(
  "Hypothesis" ,
  "Log-Likelihood",
  "Penalty Term",
  "GORICA value",
  "GORICA weight"
  
)

between_h1 %>% kbl(col.names = headers, format = "latex") %>% 
  kable_styling(full_width = TRUE)



##################### BETWEEN-PERSON ESTIMATES TABLE ###########################

means_between <- summary(samples[, summary_pars])[[1]][,1]
sd_between <- summary(samples[, summary_pars])[[1]][,2]
p2.5_between <- summary(samples[, summary_pars])[[2]][,1]
p97.5_between <- summary(samples[, summary_pars])[[2]][,5]


df_between <- tibble(
  Parameter = summary_pars,
  Mean       = means_between,
  SD         = sd_between,
  `2.5%`     = p2.5_between,
  `97.5%`    = p97.5_between
)

df_between %>%
  kable(
    format       = "latex",      # use "html" if you prefer
    digits       = 3,
  ) %>%
  kable_styling(
    full_width   = FALSE,
    latex_options = c("hold_position","scale_down")
  )



# DIAGNOSTICS PLOT #

vn      <- varnames(samples)
bmu_idx <- grep("bmu",    vn)   # between-person mean
intb_idx<- grep("intb",   vn)   # autocorrelation
c0_idx  <- grep("c5_x",   vn)   # cross-lag at x = 0
c1_idx  <- grep("c6_x",   vn)   # cross-lag at x = 1
bvar_idx<- grep("bvar",   vn)   # between-person variance

# 2. combine them in the exact order you want
order_idx <- c(
  bmu_idx,           # 1–2: bp mean NA, bp mean RUM
  intb_idx,          # 3–4: bp mean autocorr NA, RUM
  c0_idx,            # 5–6: bp M cross-lag RUM→NA, NA→RUM @ x=0
  c1_idx,            # 7–8: delta bp M cross-lag RUM→NA, NA→RUM @ x=1
  bvar_idx           # 9–14: bp var mean NA/RUM, var autocorr NA/RUM, var cross-lag RUM→NA & NA→RUM
)

titles <- c(
  "fixed effect NA",
  "fixed effect RUM",
  "fixed effect autocorrelation NA",
  "fixed effect autocorrelation RUM",
  "fixed  cross-lagged effect RUM->NA|x = 0",
  "fixed  cross-lagged effect NA->RUM|x = 0",
  "change in fixed cross-lagged RUM->NA|x = 1",
  "change in fixed cross-lagged NA->RUM|x = 1",
  "random variance NA",
  "random variance RUM",
  "random variance autocorrelation NA",
  "random variance autocorrelation RUM",
  "random variance cross-lagged RUM-> NA",
  "random variance cross-lagged NA-> RUM"
)

# 3. open your graphics device (optional—here as PDF)
pdf("all_traceplots.pdf", width = 8.27, height = 11.69)  

# 4. set up 7×2 panels and room for a global title
par(mfrow = c(7, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 2, 1))

# 5. loop over each parameter
for(i in seq_along(order_idx)) {
  idx <- order_idx[i]
  traceplot(samples[, idx],
            main = titles[i],
            xlab = "Iteration",
            ylab = "Value")
}

# 6. add one big title
title("MCMC Traceplots for All Diagnostic Parameters", 
      outer = TRUE, cex.main = 1.5)

# 7. close device
dev.off()

