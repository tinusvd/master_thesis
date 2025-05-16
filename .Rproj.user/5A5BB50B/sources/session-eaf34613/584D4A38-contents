library(tidyverse)

if (!exists("rules", envir = .GlobalEnv)) {
  rules <- read_csv2("./data_archive/meta_files/meta_conditions_groups.csv")
} else {
  message("The necessary 'rules' object exists.")
}


if (!exists("dictionary", envir = .GlobalEnv)) {
  dictionary <- read_csv2("./data_archive/meta_files/meta_dictionary.csv")
} else {
  message("The necessary 'dictionary' object exists.")
}


summs_to_create <- c("supp_set1_h1", "bound_supp_set1", "supp_set1_h1_adj",
                     "supp_set2_h1", "supp_set2_h0", 
                     "bound_supp_set2", "supp_set2_h1_adj",
                     "supp_set3_ha1", "supp_set3_ha1c", 
                     "bound_supp_set3", "supp_set3_ha1_adj_2015",
                     "supp_set4_ha2", "supp_set4_ha2c", "bound_supp_set4")


final_conditions_wl <- final_conditions_wl %>%
 mutate(across(4:60, ~if(is.character(.x)) as.numeric(.x) else .x))
final_conditions_wl <- final_conditions_wl %>%
  mutate(across(where(is.numeric), as.numeric))

final_conditions_wl <- as.data.frame(final_conditions_wl)

# Updated create_summary_stats function with proper ordering
create_summary_stats <- function(data, variable_name) {
  # Convert the variable name to a symbol for proper evaluation
  var_sym <- rlang::sym(variable_name)
  
  # First create global unique IDs
  data_with_global_ids <- data %>%
    mutate(person_id = paste(data_set_id, person_id, sep = "_"))
  
  # Calculate the support variable counts per dataset
  support_counts <- data_with_global_ids %>%
    dplyr::select(condition_id, data_set_id, person_id, !!var_sym) %>%
    group_by(condition_id, data_set_id) %>%
    # Count people with 0 and 1 values
    summarize(
      value_0_count = sum(!!var_sym == 0, na.rm = TRUE),
      value_1_count = sum(!!var_sym == 1, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate support statistics by condition and support value
  support_stats_0 <- support_counts %>%
    group_by(condition_id) %>%
    summarize(
      min_people_with_0 = min(value_0_count),
      max_people_with_0 = max(value_0_count),
      mean_people_with_0 = round(mean(value_0_count), 2),
      .groups = "drop"
    ) %>%
    # Add the support value column
    mutate(!!var_sym := 0)
  
  support_stats_1 <- support_counts %>%
    dplyr::group_by(condition_id) %>%
    summarize(
      min_people_with_1 = min(value_1_count),
      max_people_with_1 = max(value_1_count),
      mean_people_with_1 = round(mean(value_1_count), 2),
      .groups = "drop"
    ) %>%
    # Add the support value column
    mutate(!!var_sym := 1)
  
  # Calculate total counts by condition for percentage calculation
  condition_totals <- data_with_global_ids %>%
    dplyr::group_by(condition_id) %>%
    summarise(total_condition_size = n(), .groups = "drop")
  
  # Calculate other statistical metrics
  summary_stats <- data_with_global_ids %>%
    group_by(condition_id, !!var_sym) %>%
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
          !matches("N_size"),
        list(
          median = ~median(.x, na.rm = TRUE), 
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          p5th = ~quantile(.x, probs = 0.05, na.rm = TRUE),
          p95th = ~quantile(.x, probs = 0.95, na.rm = TRUE)
        )
      ),
      .groups = "drop"
    ) %>%
    # Join with condition totals to calculate percentage
    left_join(condition_totals, by = "condition_id") %>%
    # Calculate percentage with 2 decimal places
    mutate(percent_of_condition = round(n_size / total_condition_size * 100, 2)) %>%
    # Remove the total column as it's no longer needed
    dplyr::select(-total_condition_size)
  
  # Join the support statistics with the summary statistics
  # For value 0
  result_0 <- left_join(
    summary_stats %>% filter(!!var_sym == 0),
    support_stats_0,
    by = c("condition_id", variable_name)
  )
  
  # For value 1
  result_1 <- left_join(
    summary_stats %>% filter(!!var_sym == 1),
    support_stats_1,
    by = c("condition_id", variable_name)
  )
  
  # Combine the results in the desired order
  # First all conditions with value 0, then all conditions with value 1
  final_result <- bind_rows(
    # All conditions with value 0, ordered by condition_id
    result_0 %>% arrange(condition_id),
    # All conditions with value 1, ordered by condition_id
    result_1 %>% arrange(condition_id)
  )
  
  final_result <- final_result %>% 
    arrange(as.numeric(str_extract(condition_id, "\\d+"))) %>% 
    left_join(dictionary %>% dplyr::select(condition, info), 
              by = c("condition_id" = "condition")) %>%
    # Rename the info column to condition_ind
    rename(condition_ind = info) %>%
    # Extract n and t values from the condition_ind string
    mutate(
      n = as.numeric(str_extract(condition_ind, "(?<=N = )\\d+")),
      t = as.numeric(str_extract(condition_ind, "(?<=T = )\\d+"))
    ) %>%
    arrange(as.numeric(str_extract(condition_id, "\\d+")))
  
  return(final_result)
}

###### PLEASE MAKE SURE YOU HAVE AT LEAST 3GB OF RAM FREE FOR THE NEXT FOR LOOP! ########


# Make a summary of data grouped by condition_id and support variables
for (var_name in summs_to_create) {
  # Extract the part after "supp_" (e.g., "set3_ha1c")
  suffix <- sub("supp_", "", var_name)
  
  # Split by underscore to separate set number and hypothesis code
  parts <- strsplit(suffix, "_")[[1]]
  set_part <- parts[1]  # e.g., "set3"
  hypothesis_part <- paste(parts[-1], collapse = "")  # e.g., "ha1c"
  
  # Create the final object name
  obj_name <- paste0("summ_", set_part, hypothesis_part, "_wl")
  
  # Create summary
  summary_df <- create_summary_stats(final_conditions_wl, var_name)
  assign(obj_name, summary_df)
  
  # Optional: Print confirmation message
  cat("Created", obj_name, "for variable", var_name, "\n")
}

################################################################################

set1h1_true_wl <- summ_set1h1_wl %>% 
  filter(condition_id %in% rules$set1h1_true)

set1h1_false_wl <- summ_set1h1_wl %>% 
  filter(condition_id %in% rules$set1bound_true)

set1bound_true_wl <- summ_boundset1_wl %>% 
  filter(condition_id %in% rules$set1bound_true)

set1bound_false_wl <- summ_boundset1_wl %>% 
  filter(condition_id %in% rules$set1h1_true)

set2h1_true_wl <- summ_set2h1_wl %>% 
  filter(condition_id %in% rules$set2h1_true)

set2h1_false_wl <- summ_set2h1_wl %>% 
  filter(condition_id %in% rules$set2h0_true)

set2h0_true_wl <- summ_set2h0_wl %>% 
  filter(condition_id %in% rules$set2h0_true)

set2bound_true_wl <- summ_boundset2_wl %>% 
  filter(condition_id %in% rules$set2h0_true)

set2bound_false_wl <- summ_boundset2_wl %>% 
  filter(condition_id %in% rules$set2h1_true)

set2h0_false_wl <- summ_set2h0_wl %>% 
  filter(condition_id %in% rules$set2h1_true)

set3ha1_true_wl <- summ_set3ha1_wl %>% 
  filter(condition_id %in% rules$set3ha1_true)

set3bound_true_wl <- summ_boundset3_wl %>% 
  filter(condition_id %in% rules$set3bound_true)

set3bound_false_wl <- summ_boundset3_wl %>% 
  filter(condition_id %in% rules$set3bound_false)

set3ha1_false_wl <- summ_set3ha1_wl %>% 
  filter(condition_id %in% rules$set3ha1c_true)

set3_ha1_false_wl_p210 <- summ_set3ha1_wl %>% 
  filter(condition_id %in% rules$set3ha1c_true & condition_id %in% rules$condition_p210 & condition_id %in% rules$condition_n100)

set3_ha1c_true_wl_p210 <- summ_set3ha1c_wl %>% 
  filter(condition_id %in% rules$set3ha1c_true & condition_id %in% rules$condition_p210 & condition_id %in% rules$condition_n100)

set3ha1c_true_wl <- summ_set3ha1c_wl %>% 
  filter(condition_id %in% rules$set3ha1c_true)

set3ha1c_false_wl <- summ_set3ha1c_wl %>% 
  filter(condition_id %in% rules$set3ha1_true)

set4ha2_true_wl <- summ_set4ha2_wl %>% 
  filter(condition_id %in% rules$set4ha2_true)

set4ha2_false_wl <- summ_set4ha2_wl %>% 
  filter(condition_id %in% rules$set4ha2c_true)

set4ha2c_true_wl <- summ_set4ha2c_wl %>% 
  filter(condition_id %in% rules$set4ha2c_true)

set4ha2c_false_wl <- summ_set4ha2c_wl %>% 
  filter(condition_id %in% rules$set4ha2_true)

set4bound_false_wl <- summ_set4ha2_wl %>% 
  filter(condition_id %in% rules$set4bound_false)

