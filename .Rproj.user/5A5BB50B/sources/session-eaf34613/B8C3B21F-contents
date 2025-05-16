library(tidyverse)
# Define the main folder
output_dir <- "./data_archive/post_processing/intermediate_output/between_level"
output_combined <- "./data_archive/post_processing/intermediate_output/between_level/combined"
gorica_files <- "./data_archive/simulations/conditions_data/output"

# Load important file for clustering
rules <- read_csv2("./data_archive/meta_files/meta_conditions_groups.csv")


# Identify all condition folders in the main directory
output_folders <- list.dirs(path = gorica_files,
                            full.names = TRUE,
                            recursive = FALSE)

output_folders <- output_folders[grepl("^condition_.*$", basename(output_folders))]

# Process each condition separately
for (condition in output_folders) {
  condition_name <- basename(condition)
  
  # Modified pattern to match only gorica output files
  files_to_read <- list.files(path = condition,
                              pattern = "^gorica_output_con.*\\.Rdata$",
                              full.names = TRUE)
  
  # Print number of files found for verification
  cat(sprintf("Found %d gorica output files in %s\n", 
              length(files_to_read), condition_name))
  
  # Create an output file for the condition
  output_file <- file.path(output_dir, paste0("intermediate_sample_summary_", condition_name, ".csv"))
  
  
  # Write column names to the output file once
  initial_df <- data.frame(
    condition_id = character(),
    data_set_id = character(),
    b_y12 = double(),
    b_y21 = double(),
    set1_h1_wgt_gor = double(),
    set1_hc_wgt_gor = double(),
    supp_set1_h1 = integer(),
    supp_set1_hc = integer(),
    set2_h0_wgt_gor = double(),
    set2_h1_wgt_gor = double(),
    set2_hc_wgt_gor = double(),
    supp_set2_h1 = integer(),
    supp_set2_h0 = integer(),
    supp_set2_hc = integer(),
    set3_ha1_wgt_gor = double(),
    set3_ha1c_wgt_gor = double(),
    supp_set3_ha1 = integer(),
    supp_set3_ha1c = integer(),
    set4_ha2_wgt_gor = double(),
    set4_ha2c_wgt_gor = double(),
    supp_set4_ha2 = integer(),
    supp_set4_ha2c = integer(),
    set1_ratio_h1hc = double(),
    set1_ratio_hch1 = double(),
    set2_ratio_h1h0 = double(),
    set2_ratio_h0h1 = double(),
    set2_ratio_h1hc = double(),
    set2_ratio_hch1 = double(),
    set3_ratio_ha1ha1c = double(),
    set3_ratio_ha1cha1 = double(),
    set4_ratio_ha2ha2c = double(),
    set4_ratio_ha2cha2 = double(),
    set1_h1_log = double(),
    set1_hc_log = double(),
    set2_h0_log = double(),
    set2_h1_log = double(),
    set2_hc_log = double(),
    set3_ha1_log = double(),
    set3_ha1c_log = double(),
    set4_ha2_log = double(),
    set4_ha2c_log = double(),
    set1_IC_weight_h1 = double(),
    set1_IC_weight_hc = double(),
    set2_IC_weight_h0 = double(),
    set2_IC_weight_h1 = double(),
    set2_IC_weight_hc = double(),
    set3_IC_weight_ha1 = double(),
    set3_IC_weight_ha1c = double(),
    set4_IC_weight_ha2 = double(),
    set4_IC_weight_ha2c = double(),
    set1_ratio_IC_wgt_h1_vs_hc = double(),
    set1_ratio_IC_wgt_hc_vs_h1 = double(),
    set2_ratio_IC_wgt_h1_vs_hc = double(),
    set2_ratio_IC_wgt_hc_vs_h1 = double(),
    set3_ratio_IC_wgt_ha1_vs_ha1c = double(),
    set3_ratio_IC_wgt_ha1c_vs_ha1 = double(),
    set4_ratio_IC_wgt_ha2_vs_ha2c = double(),
    set4_ratio_IC_wgt_ha2c_vs_ha2 = double(),
    bound_supp_set1 = numeric(),
    bound_supp_set2 = numeric(),
    bound_supp_set3 = numeric(),
    bound_supp_set4 = numeric()
  )
  
  write.table(
    initial_df,
    file = output_file,
    sep = ",",
    col.names = TRUE,
    row.names = FALSE
  )
  
  # Loop through all simulations in the condition
  for (i in seq_along(files_to_read)) {
    file <- files_to_read[i]
    
    # Print which file is being processed for debugging
    cat(sprintf("Processing file %d/%d: %s\n", 
                i, length(files_to_read), basename(file)))
    
    data_set_id <- sub(".*(sim_.*)$", "\\1", tools::file_path_sans_ext(basename(file)))
    
    # Load the file and extract the required data
    load(file)
    object_name <- tools::file_path_sans_ext(basename(file))
    loaded_object <- get(object_name)
    
    model <- loaded_object[["model_gorica"]]
    params <- loaded_object[["model_std"]][["est"]]
      
      # Extract the required values
      set1 <- c(h1_wgt_gor = model[["set1"]][4], hc_wgt_gor = model[["set1"]][8])
      set2 <- c(
        h0_wgt_gor = model[["set2"]][4],
        h1_wgt_gor = model[["set2"]][8],
        hc_wgt_gor = model[["set2"]][12]
      )
      set3 <- c(ha1_wgt_gor = model[["set3"]][4], ha1c_wgt_gor = model[["set3"]][8])
      set4 <- c(ha2_wgt_gor = model[["set4"]][4], ha2c_wgt_gor = model[["set4"]][8])
      
      b_y12 <- params[1]
      b_y21 <- params[2]
      
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
      
      # Obtain the logs and boundary
      set1_h1_log <- model[["set1"]][1]
      set1_hc_log <- model[["set1"]][5]
      
      set2_h0_log <- model[["set2"]][1]
      set2_h1_log <- model[["set2"]][5]
      set2_hc_log <- model[["set2"]][9]
      
      set3_ha1_log <- model[["set3"]][1]
      set3_ha1c_log <- model[["set3"]][5]
      
      set4_ha2_log <- model[["set4"]][1]
      set4_ha2c_log <- model[["set4"]][5]

      # Obtain IC weights
      set1_IC_weight_h1 <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set1"]][1], model[["set1"]][5]))$IC_weights[1])
      set1_IC_weight_hc <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set1"]][1], model[["set1"]][5]))$IC_weights[2])
      
      set2_IC_weight_h0 <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set2"]][1], model[["set2"]][5], model[["set2"]][9]))$IC_weights[1])
      set2_IC_weight_h1 <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set2"]][1], model[["set2"]][5], model[["set2"]][9]))$IC_weights[2])
      set2_IC_weight_hc <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set2"]][1], model[["set2"]][5], model[["set2"]][9]))$IC_weights[3])
      
      set3_IC_weight_ha1 <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set3"]][1], model[["set3"]][5]))$IC_weights[1])
      set3_IC_weight_ha1c <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set3"]][1], model[["set3"]][5]))$IC_weights[2])
      
      set4_IC_weight_ha2 <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set4"]][1], model[["set4"]][5]))$IC_weights[1])
      set4_IC_weight_ha2c <- as.numeric(restriktor::calc_ICweights(-2*c(model[["set4"]][1], model[["set4"]][5]))$IC_weights[2])
      
      # Obtain the ratio of the IC weights
      set1_ratio_IC_wgt_h1_vs_hc <- set1_IC_weight_h1 / set1_IC_weight_hc
      set1_ratio_IC_wgt_hc_vs_h1 <- set1_IC_weight_hc / set1_IC_weight_h1
      set2_ratio_IC_wgt_h1_vs_hc <- set2_IC_weight_hc / set2_IC_weight_h1
      set2_ratio_IC_wgt_hc_vs_h1 <- set2_IC_weight_h1 / set2_IC_weight_hc
      set3_ratio_IC_wgt_ha1_vs_ha1c <- set3_IC_weight_ha1 / set3_IC_weight_ha1c
      set3_ratio_IC_wgt_ha1c_vs_ha1 <- set3_IC_weight_ha1c / set3_IC_weight_ha1
      set4_ratio_IC_wgt_ha2_vs_ha2c <- set4_IC_weight_ha2 / set4_IC_weight_ha2c
      set4_ratio_IC_wgt_ha2c_vs_ha2 <- set4_IC_weight_ha2c / set4_IC_weight_ha2
      
      bound_supp_set1 <- NA_integer_
      bound_supp_set2 <- NA_integer_
      bound_supp_set3 <- NA_integer_
      bound_supp_set4 <- NA_integer_
      
      
      # Combine all sets, support columns, and ratios into a single row
      simulation_row <- c(
        condition_id = condition_name,
        data_set_id = data_set_id,
        b_y12,
        b_y21,
        set1_h1_wgt_gor = set1["h1_wgt_gor"],
        set1_hc_wgt_gor = set1["hc_wgt_gor"],
        supp_set1_h1,
        supp_set1_hc,
        set2_h0_wgt_gor = set2["h0_wgt_gor"],
        set2_h1_wgt_gor = set2["h1_wgt_gor"],
        set2_hc_wgt_gor = set2["hc_wgt_gor"],
        supp_set2_h1,
        supp_set2_h0,
        supp_set2_hc,
        set3_ha1_wgt_gor = set3["ha1_wgt_gor"],
        set3_ha1c_wgt_gor = set3["ha1c_wgt_gor"],
        supp_set3_ha1,
        supp_set3_ha1c,
        set4_ha2_wgt_gor = set4["ha2_wgt_gor"],
        set4_ha2c_wgt_gor = set4["ha2c_wgt_gor"],
        supp_set4_ha2,
        supp_set4_ha2c,
        set1_ratio_h1hc,
        set1_ratio_hch1,
        set2_ratio_h1h0,
        set2_ratio_h0h1,
        set2_ratio_h1hc,
        set2_ratio_hch1,
        set3_ratio_ha1ha1c,
        set3_ratio_ha1cha1,
        set4_ratio_ha2ha2c,
        set4_ratio_ha2cha2,
        set1_h1_log,
        set1_hc_log,
        set2_h0_log,
        set2_h1_log,
        set2_hc_log,
        set3_ha1_log,
        set3_ha1c_log,
        set4_ha2_log,
        set4_ha2c_log,
        set1_IC_weight_h1,
        set1_IC_weight_hc,
        set2_IC_weight_h0,
        set2_IC_weight_h1,
        set2_IC_weight_hc,
        set3_IC_weight_ha1,
        set3_IC_weight_ha1c,
        set4_IC_weight_ha2,
        set4_IC_weight_ha2c,
        set1_ratio_IC_wgt_h1_vs_hc,
        set1_ratio_IC_wgt_hc_vs_h1,
        set2_ratio_IC_wgt_h1_vs_hc,
        set2_ratio_IC_wgt_hc_vs_h1,
        set3_ratio_IC_wgt_ha1_vs_ha1c,
        set3_ratio_IC_wgt_ha1c_vs_ha1,
        set4_ratio_IC_wgt_ha2_vs_ha2c,
        set4_ratio_IC_wgt_ha2c_vs_ha2,
        bound_supp_set1,
        bound_supp_set2,
        bound_supp_set3,
        bound_supp_set4
      )
      
      
      # Append the row to the file without column names
      write.table(
        t(simulation_row),
        file = output_file,
        sep = ",",
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
      )

    
    # Remove the loaded object to save memory
    rm(list = object_name)
    gc()
  }
}

# Create empty data frame for final results
final_conditions <- data.frame()

# List all intermediate files
files_listed <- list.files(path = output_dir, recursive = F, full.names = T, pattern = "*\\.csv$")

for (file in files_listed){
  # Read the file 
  temp <- read_csv(file)
  
  # Remove completely empty rows
  temp <- temp %>% filter(rowSums(is.na(.)) != ncol(.))
  
  # If any specific column should never be NA, filter on that too
  # For example, if condition_id should never be NA:
  temp <- temp %>% filter(!is.na(condition_id))
  
  # Add to final results
  final_conditions <- bind_rows(final_conditions, temp)
}


final_conditions <- final_conditions %>% 
  mutate(n = case_when(
    condition_id %in% rules$condition_n50 ~ 50,
    condition_id %in% rules$condition_n75 ~ 75,
    condition_id %in% rules$condition_n100 ~ 100,
    condition_id %in% rules$condition_n150 ~ 150,
    TRUE ~ NA_integer_),
    t = case_when(
      condition_id %in% rules$condition_t25 ~ 25,
      condition_id %in% rules$condition_t50 ~ 50,
      condition_id %in% rules$condition_t75 ~ 75,
      condition_id %in% rules$condition_t100 ~ 100,
      TRUE ~ NA_integer_
    ),
    # Extract numeric parts for proper sorting
    condition_num = as.numeric(str_extract(condition_id, "\\d+")),
    sim_num = as.numeric(str_extract(data_set_id, "\\d+"))
  ) %>% 
  arrange(condition_num, sim_num) %>%
  dplyr::select(-condition_num, -sim_num)  # Remove the temporary columns

# You can also write the cleaned version back
write.csv(final_conditions, file = paste0(output_combined, "/combined_results.csv"), row.names = FALSE)


