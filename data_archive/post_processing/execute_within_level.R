
#################### Within-Subject Post-Processing ###########################

#                    Run the following lines separately for the                #
#                    post-processing of the witihn-subject level               #
#                    results.                                                  #


# WARNING! PLEASE MAKE SURE YOU HAVE AT LEAST 3.5 GB OF MEMORY (RAM) AVAILABLE
# WHEN EXECUTING THIS SCRIPT.

# Run the following line to create and save a large object that holds the 
# between-subject results for every data set in every condtion.
source("./data_archive/post_processing/scripts_within_level/01_WL_INTERMEDIATE_SUMM.R")


# Run the following line. The output is necessary for the benchmark function.
source("./data_archive/post_processing/source_functions/FUNCTION_GORICA.R")

# Run the following lines to generate the GORICA objects.
bound1515_wl <- scan_bound_candidate(data = final_conditions_wl, 
                                  pop_val = c(0.15, 0.15))
bound215_wl <- scan_bound_candidate(data = final_conditions_wl, 
                                 pop_val = c(0.20, 0.15))

# Run the GORICA functions
load_data_gorica(bound1515, gorica_files)
load_data_gorica(bound215, gorica_files)


# Run the following line to initialize the benchmark function.
source("./data_archive/post_processing/source_functions/FUNCTION_BENCHMARK.R")

# Run the following lines to generate benchmark results. 
# The processes probably take more than 10 minutes.
benchmark_run(bound1515_wl_goric_results, 
              info_object = bound1515_wl, 
              pop_est = c(0.15, 0.15))

benchmark_run(bound215_wl_goric_results, 
              info_object = bound215_wl, 
              pop_est = c(0.20, 0.15))


bound_list_wl <- c(bound1515_wl_benchmark, bound215_wl_benchmark)

# Run the following line to initialize the apply-benchmark function.
source("./data_archive/post_processing/source_functions/FUNCTION_APPLY_BENCH.R")


# Run the following line to apply the benchmark and add it to the final object.
source("./data_archive/post_processing/scripts_within_level/02_WL_APPLY_BENCH.R")

# Store the new object as a .csv.
data.table::fwrite(final_conditions_wl, 
          file = paste0(output_combined, "/combined_results_wl.csv"), 
          row.names = FALSE)


# Run this line to generate summaries per hypothesis condition
source("./data_archive/post_processing/scripts_within_level/03_WL_CONDITIONAL_SUMM.R")


# Run this line to generate tables
source("./data_archive/post_processing/scripts_within_level/04_WL_KABLE_TABLE.R")


# Run this line to generate plots in general, and stack the plots to create
# the figures that are found in the paper.
source("./data_archive/post_processing/scripts_within_level/05_WL_PLOTS.R")


# Run this line to generate plots for the supplementary material - between level.
source("./data_archive/post_processing/scripts_within_level/06_WL_PLOTS_SUPPLEMENTARY.R")
