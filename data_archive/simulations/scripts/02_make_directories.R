
# Define the base path
base_path <- "./data_archive/simulations/conditions_data/"

# Loop through and create each condition folder
for (i in c(1:48, 61:68)) {
  folder_name <- paste0("condition_", i)
  data_folder_path <- file.path(base_path, "data", folder_name)
  output_folder_path <- file.path(base_path, "output", folder_name)
  
  if (!dir.exists(data_folder_path)) {
    dir.create(data_folder_path, recursive = TRUE)
    print(paste("Created folder:", normalizePath(data_folder_path, winslash = "\\")))
  } else {
    print(paste("Folder already exists:", normalizePath(data_folder_path, winslash = "\\")))
  }
  
  if (!dir.exists(output_folder_path)) {
    dir.create(output_folder_path, recursive = TRUE)
    print(paste("Created folder:", normalizePath(output_folder_path, winslash = "\\")))
  } else {
    print(paste("Folder already exists:", normalizePath(output_folder_path, winslash = "\\")))
  }
}


# Initialize load and save directories 
load_directory <- paste0(base_path, "data")
save_directory <- paste0(base_path, "output")

# Initialize the jags file
jags_file <- "./data_archive/simulations/files/bivar_var1_nocovariate.jags"


meta_conditions2 <- tibble(
  condition = c(1:48, 61:68),
  u.seed = 1000 + c(1:48, 61:68),
  nsim = rep(500, 56),
  load_data_path = normalizePath(paste0(load_directory, "/condition_", sprintf("%01d", condition)), winslash = "\\"),
  save_gorica_path = normalizePath(paste0(save_directory, "/condition_", sprintf("%01d", condition)), winslash = "\\"),
  gorica_save_name = paste0("gorica_condition_", sprintf("%01d", condition)),
  jags_file_path = normalizePath(jags_file, winslash = "\\")
)
