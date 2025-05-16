
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


final_conditions <- final_conditions %>% 
  mutate(across(3:59, ~if(is.character(.x)) as.numeric(.x) else .x))

# How many data sets are there per condition?
ncons <- final_conditions %>% 
  group_by(condition_id) %>% 
  count(condition_id)

# Function to create summaries grouped by condition and whether there is support in a set for a hypothesis.
create_summary_stats <- function(data, variable_name) {
  # Convert the variable name to a symbol for proper evaluation
  var_sym <- rlang::sym(variable_name)
  
  # Create the summary
  summary_df <- data %>%
    group_by(condition_id, !!var_sym) %>%
    summarise(
      n_size = n(),
      # Standard summary statistics for numeric variables
      across(
        where(is.numeric) & !matches("supp") & !matches("n_size") & !matches("^t") & !matches("^n"), 
        list(
          median = ~median(.x, na.rm = TRUE), 
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          p5th = ~quantile(.x, probs = 0.05, na.rm = TRUE),
          p95th = ~quantile(.x, probs = 0.95, na.rm = TRUE)
        )
      ),
      # Mean and sd for b_y12 and b_y21
      b_y12_mean = mean(b_y12, na.rm = FALSE),
      b_y12_sd = sd(b_y12, na.rm = FALSE),
      b_y21_mean = mean(b_y21, na.rm = FALSE),
      b_y21_sd = sd(b_y21, na.rm = FALSE),
      .groups = "drop"
    ) %>% 
    # Join with the dictionary to get the condition info
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
  
  return(summary_df)
}


# Make a summary of data grouped by condition_id and whether there is support for H_x in set_x
for (var_name in summs_to_create) {
  # Extract the part after "supp_" (e.g., "set3_ha1c")
  suffix <- sub("supp_", "", var_name)
  
  parts <- strsplit(suffix, "_")[[1]]
  set_part <- parts[1] 
  hypothesis_part <- paste(parts[-1], collapse = "")
  
  # Create the final object name
  obj_name <- paste0("summ_", set_part, hypothesis_part, "_model")
  summary_df <- create_summary_stats(final_conditions, var_name)
  assign(obj_name, summary_df)
  
  # Print confirmation message
  cat("Created", obj_name, "for variable", var_name, "\n")
}


# Create conditional summaries
set1h1_true <- summ_set1h1_model %>% 
  filter(condition_id %in% rules$set1h1_true)

set1h1_false <- summ_set1h1_model %>% 
  filter(condition_id %in% rules$set1bound_true)

set1bound_true <- summ_boundset1_model %>% 
  filter(condition_id %in% rules$set1bound_true)

set1bound_false <- summ_boundset1_model %>% 
  filter(condition_id %in% rules$set1h1_true)

set2h1_true <- summ_set2h1_model %>% 
  filter(condition_id %in% rules$set2h1_true)

set2h1_false <- summ_set2h1_model %>% 
  filter(condition_id %in% rules$set2h0_true)

set2h0_true <- summ_set2h0_model %>% 
  filter(condition_id %in% rules$set2h0_true)

set2bound_true <- summ_boundset2_model %>% 
  filter(condition_id %in% rules$set2h0_true)

set2bound_false <- summ_boundset2_model %>% 
  filter(condition_id %in% rules$set2h1_true)

set2h0_false <- summ_set2h0_model %>% 
  filter(condition_id %in% rules$set2h1_true)

set3ha1_true <- summ_set3ha1_model %>% 
  filter(condition_id %in% rules$set3ha1_true)

set3bound_true <- summ_boundset3_model %>% 
  filter(condition_id %in% rules$set3bound_true)

set3bound_false <- summ_boundset3_model %>% 
  filter(condition_id %in% rules$set3bound_true)

set3ha1_false <- summ_set3ha1_model %>% 
  filter(condition_id %in% rules$set3ha1c_true)

set3ha1c_true <- summ_set3ha1c_model %>% 
  filter(condition_id %in% rules$set3ha1c_true)

set3ha1c_false <- summ_set3ha1c_model %>% 
  filter(condition_id %in% rules$set3ha1_true)

set4ha2_true <- summ_set4ha2_model %>% 
  filter(condition_id %in% rules$set4ha2_true)

set4ha2_false <- summ_set4ha2_model %>% 
  filter(condition_id %in% rules$set4ha2c_true)

set4ha2c_true <- summ_set4ha2c_model %>% 
  filter(condition_id %in% rules$set4ha2c_true)

set4ha2c_false <- summ_set4ha2c_model %>% 
  filter(condition_id %in% rules$set4ha2_true)

set4bound_false <- summ_set4ha2_model %>% 
  filter(condition_id %in% rules$set4bound_false)


