
#################### Between-Subject Post-Processing ###########################

#                    Run the following lines separately for the                #
#                    post-processing of the between-subject level              #
#                    results.                                                  #

# Uncomment and run the following line if you already have the object for the
# between-level and want to analyze the results.
#final_conditions <- read_csv("./data_archive/post_processing/intermediate_output/between_level/combined/combined_results.csv")

# Run the following line to create and save a large object that holds the 
# between-subject results for every data set in every condtion.
source("./data_archive/post_processing/scripts_between_level/01_INTERMEDIATE_SUMM.R")


# Run the following line. The output is necessary for the benchmark function.
source("./data_archive/post_processing/source_functions/FUNCTION_GORICA.R")

# Run the following lines to generate the GORICA objects.
bound1515 <- scan_bound_candidate(data = final_conditions, 
                                  pop_val = c(0.15, 0.15))

bound215 <- scan_bound_candidate(data = final_conditions, 
                                 pop_val = c(0.20, 0.15))

# Run the GORICA functions
load_data_gorica(bound1515, gorica_files)
load_data_gorica(bound215, gorica_files)


# Run the following line to initialize the benchmark function.
source("./data_archive/post_processing/source_functions/FUNCTION_BENCHMARK.R")

# Run the following lines to generate benchmark results. 
# The processes probably take more than 10 minutes.
benchmark_run(bound1515_goric_results, 
              info_object = bound1515, 
              pop_est = c(0.15, 0.15))

benchmark_run(bound215_goric_results, 
              info_object = bound215, 
              pop_est = c(0.20, 0.15))

bound_list <- c(bound1515_benchmark, bound215_benchmark)

# Run the following line to initialize the apply-benchmark function.
source("./data_archive/post_processing/source_functions/FUNCTION_APPLY_BENCH.R")


# Run the following line to apply the benchmark and add it to the final object.
source("./data_archive/post_processing/scripts_between_level/02_APPLY_BENCH.R")

# Store the new object as a .csv.
write_csv(final_conditions, 
          file = paste0(output_combined, "/combined_results.csv"), 
          row.names = FALSE)


# Run this line to generate summaries per hypothesis condition
source("./data_archive/post_processing/scripts_between_level/03_CONDITIONAL_SUMM.R")


# Run this line to generate tables
source("./data_archive/post_processing/scripts_between_level/04_TABLES.R")


# Run this line to generate plots in general, and stack the plots to create
# the figures that are found in the paper.
source("./data_archive/post_processing/scripts_between_level/05_PLOTS.R")


# Run this line to generate plots for the supplementary material - between level.
source("./data_archive/post_processing/scripts_between_level/06_PLOTS_SUPPLEMENTARY.R")

