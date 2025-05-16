library(tidyverse)
library(kableExtra)

# Your existing setup
summary_rules <- read_csv2("./data_archive/meta_files/meta_summary_condition_rules.csv")


source("./data_archive/post_processing/source_functions/FUNCTION_CONCATENATE.R")

conditions_table <- c("condition_p210", "condition_p215", "condition_p1515", "condition_p2175")

# Clean table_function without debug messages
table_function <- function(format_rules, output_dir = ".", par_conditions, rules_df) {
  
  n_tables <- nrow(format_rules)
  n_conditions <- length(par_conditions)
  saved_tables <- list() # To keep track of created tables
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Loop through each table in format_rules
  for (table_idx in 1:n_tables) {
    # Get table configuration for this iteration
    table_id <- format_rules$table_id[table_idx]
    
    # Loop through each condition column
    for (cond_idx in 1:n_conditions) {
      par_con <- par_conditions[cond_idx]
      
      # Get the source table
      table_of_interest <- paste0("summ_", table_id, "_model")
      
      # Make sure the table exists in the environment
      if (!exists(table_of_interest)) {
        next
      }
      
      table_name <- get(table_of_interest)
      
      # Get the condition IDs to keep from the rules dataframe
      # First check if the column exists in rules_df
      if (!(par_con %in% colnames(rules_df))) {
        next
      }
      
      # Get the condition IDs for this condition
      condition_ids_to_keep <- rules_df[[par_con]]
      
      # Remove NAs and empty strings
      condition_ids_to_keep <- condition_ids_to_keep[!is.na(condition_ids_to_keep) & condition_ids_to_keep != ""]
      
      if (length(condition_ids_to_keep) == 0) {
        next
      }
      
      # Filter the table for the specified condition IDs
      filtered_table <- table_name %>% 
        dplyr::filter(condition_id %in% condition_ids_to_keep)
      
      # If no data after filtering, skip to next
      if (nrow(filtered_table) == 0) {
        next
      }
      
      # Use the filtered_table directly instead of trying to get a non-existent table
      table <- filtered_table %>% 
        mutate(
          # Store the original support value for conditional logic
          support_value = .data[[format_rules$support[table_idx]]],
          # Create the support label
          supp = case_when(
            .data[[format_rules$support[table_idx]]] == 0 ~ "no",
            .data[[format_rules$support[table_idx]]] == 1 ~ "yes",
            TRUE ~ "unknown"
          ),
          b_y12 = format_mean(.data[[format_rules$b_y12_m[table_idx]]], .data[[format_rules$b_y12_sd[table_idx]]]),
          b_y12_p = format_minmax(.data[[format_rules$b_y12_p5[table_idx]]], .data[[format_rules$b_y12_p95[table_idx]]]),
          b_y21 = format_mean(.data[[format_rules$b_y21_m[table_idx]]], .data[[format_rules$b_y21_sd[table_idx]]]),
          b_y21_p = format_minmax(.data[[format_rules$b_y21_p5[table_idx]]], .data[[format_rules$b_y21_p95[table_idx]]]),
          gor_wgt1 = format_median(.data[[format_rules$gor_wgt1_m[table_idx]]]),
          gor_wgt1_p = format_minmax(.data[[format_rules$gor_wgt1_p5[table_idx]]], .data[[format_rules$gor_wgt1_p95[table_idx]]]),
          gor_wgt2 = format_median(.data[[format_rules$gor_wgt2_m[table_idx]]]),
          gor_wgt2_p = format_minmax(.data[[format_rules$gor_wgt2_p5[table_idx]]], .data[[format_rules$gor_wgt2_p95[table_idx]]]),
          # Calculate different gor_ratio based on support value
          gor_ratio = case_when(
            support_value == 0 ~ format_median(.data[[format_rules$gor_ratio2_m[table_idx]]]),
            support_value == 1 ~ format_median(.data[[format_rules$gor_ratio_m[table_idx]]]),
            TRUE ~ format_median(.data[[format_rules$gor_ratio_m[table_idx]]])
          ),
          # Calculate different gor_ratio minmax based on support value
          gor_ratio_p = case_when(
            support_value == 0 ~ format_minmax(.data[[format_rules$gor_ratio2_p5[table_idx]]], .data[[format_rules$gor_ratio2_p95[table_idx]]]),
            support_value == 1 ~ format_minmax(.data[[format_rules$gor_ratio_p5[table_idx]]], .data[[format_rules$gor_ratio_p95[table_idx]]]),
            TRUE ~ format_minmax(.data[[format_rules$gor_ratio_p5[table_idx]]], .data[[format_rules$gor_ratio_p95[table_idx]]])
          )
        ) %>% 
        dplyr::select(condition_ind, supp, n_size, b_y12, b_y12_p, b_y21, b_y21_p, 
                      gor_wgt1, gor_wgt1_p, gor_wgt2, gor_wgt2_p, gor_ratio, gor_ratio_p)
      
      n_rows <- nrow(table)
      
      # Check if header columns exist and provide defaults if missing
      header_supp_n <- if("header_supp" %in% names(format_rules)) format_rules$header_supp[table_idx] else "Support"
      header_wgt1 <- if("header_wgt1" %in% names(format_rules)) format_rules$header_wgt1[table_idx] else "GORICA W1"
      header_wgt2 <- if("header_wgt2" %in% names(format_rules)) format_rules$header_wgt2[table_idx] else "GORICA W2"
      
      # Create the header vector correctly
      headers <- c(" " = 3,
                   "$\\phi_{12}$" = 2, 
                   "$\\phi_{21}$" = 2)
      N_header <- "$N_{datasets}$"
      
      # Dynamically add the wgt headers
      wgt1_header <- setNames(2, header_wgt1)
      wgt2_header <- setNames(2, header_wgt2)
      
      # Combine all headers
      headers <- c(headers, wgt1_header, wgt2_header, "ratio $ww'$" = 2)
      
      kbl_table_tex <- table %>%
        arrange(as.numeric(str_extract(condition_ind, "\\d+"))) %>%
        kbl(
          format = "latex",
          escape = FALSE,
          col.names = c(
            "Condition", format_rules$header_supp[table_idx], "$N_{pooled}$",
            "Mean (SD)", "(min, max)",  # φ12
            "Mean (SD)", "(min, max)",  # φ21
            "Median", "(min, max)",     # GOR W1
            "Median", "(min, max)",     # GOR W2
            "Median", "(min, max)"      # ratio
          )
        ) %>%
        kable_styling(full_width = TRUE) %>%
        add_header_above(headers) %>%
        row_spec(0, extra_css = "border-bottom: 1px solid #ddd !important;")
      
      # assign the LaTeX version in .GlobalEnv
      output_name <- paste0("table_", table_id, "_", gsub("condition_", "", par_con), "_model")
      assign(output_name, kbl_table_tex, envir = .GlobalEnv)
      saved_tables <- c(saved_tables, output_name)
      
      # Build HTML table
      kbl_table_html <- table %>%
        arrange(as.numeric(str_extract(condition_ind, "\\d+"))) %>%
        kbl(
          format = "html",
          escape = FALSE,
          col.names = c(
            "Condition", format_rules$header_supp[table_idx], "N<sub>pooled</sub>",
            "Mean (SD)", "(min, max)",
            "Mean (SD)", "(min, max)",
            "Median", "(min, max)",
            "Median", "(min, max)",
            "Median", "(min, max)"
          )
        ) %>%
        kable_styling(full_width = TRUE) %>%
        add_header_above(headers)
      
      # Save HTML to disk
      html_file <- file.path(output_dir, paste0(output_name, ".html"))
      save_kable(kbl_table_html, html_file)
    }
  }
  
  return(saved_tables)
}

# Example usage:
table_function(summary_rules, "./data_archive/figures_tables/tables/between_level", conditions_table, rules)
