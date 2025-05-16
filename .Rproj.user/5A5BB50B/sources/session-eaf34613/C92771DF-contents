

# Please load the following line to install and load the necessary packages.
source("./data_archive/simulations/scripts/01_initialization.R")


# Run the following line to create necessary directories.
source("./data_archive/simulations/scripts/02_make_directories.R")

# Run the following line to generate the data.
source("./data_archive/simulations/scripts/03_create_data.R")

# Parallel proces

# IMPORTANT: Before running the proces, determine how many cores you want 
# to utilize for the parallelization. Leave at least 2 cores free on your
# machine to avoid overload.
message(paste("You have", detectCores(), "cores"))


leave_n_cores_free <- 4 # CHANGE THIS VALUE IF NECESSARY. 

# Run the following lines to start the parallel process. 
# WARNING: The run time is heavily dependent on the specifications of your machine.
# We recommend running the process on a machine with at least 64 cores. Even
# with that many cores, the process will probably take at least 20 days 
# to complete.
source("./data_archive/simulations/scripts/04_parallel_process.R")
