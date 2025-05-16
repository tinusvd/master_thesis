library(patchwork)
library(latex2exp)

# Define all the support variables you want to summarize
support_vars <- c(
  "supp_set1_h1", "supp_set1_hc",
  "bound_supp_set1",
  "supp_set1_h1_adj", "supp_set1_hc_adj",
  "supp_set2_h1", "supp_set2_h0", 
  "supp_set2_hc", "bound_supp_set2",
  "supp_set2_h1_adj", "supp_set2_h0_adj",
  "supp_set2_hc_adj",
  "supp_set3_ha1", "supp_set3_ha1c",
  "bound_supp_set3", "supp_set3_ha1_adj_2015",
  "supp_set3_ha1_adj", "supp_set3_ha1c_adj",
  "supp_set4_ha2", "supp_set4_ha2c",
  "supp_set4_ha2_adj", "supp_set4_ha2c_adj",
  "bound_supp_set4"
)

thr_plot_rules <- read_csv2("./data_archive/meta_files/meta_THR-plot_rules.csv")
plot_transform_rules <- read_csv2("./data_archive/meta_files/meta_plot_information.csv")

# Create a function to generate the summaries for all variables at once
get_support_summary <- function(data, supp_vars, by_var = "condition_id") {
  
  # Initialize an empty list to store results
  result_list <- list()
  
  # Process each support variable
  for (var in supp_vars) {
    # Create summary for current variable
    temp_summary <- data %>%
      group_by(!!sym(by_var), !!sym(var)) %>%
      summarise(n = n(), .groups = "drop") %>%
      # Pivot to get wide format
      pivot_wider(
        names_from = !!sym(var),
        values_from = n,
        names_prefix = paste0(var, "_val_")
      )
    
    # Add to list
    result_list[[var]] <- temp_summary
  }
  
  # Join all results together
  final_result <- result_list[[1]]
  if (length(result_list) > 1) {
    for (i in 2:length(result_list)) {
      final_result <- full_join(final_result, result_list[[i]], by = by_var)
    }
  }
  
  return(final_result)
}

# Run the function on your data
support_summary <- get_support_summary(
  final_conditions_wl, 
  support_vars
) %>%
  arrange(as.numeric(str_extract(condition_id, "\\d+")))


# For a new column (when n doesn't exist yet)
support_summary <- support_summary %>% 
  mutate(n = dplyr::case_when(
    condition_id %in% rules$condition_n50  ~ 50,
    condition_id %in% rules$condition_n75 ~ 75,
    condition_id %in% rules$condition_n100 ~ 100,
    condition_id %in% rules$condition_n150 ~ 150,
    TRUE ~ NA_real_
  ),
  t = dplyr::case_when(
    condition_id %in% rules$condition_t25 ~ 25,
    condition_id %in% rules$condition_t50 ~ 50,
    condition_id %in% rules$condition_t75 ~ 75,
    condition_id %in% rules$condition_t100 ~ 100,
  )
  ) 



if(!("supp_set3_ha1_adj_val_1" %in% names(support_summary))){
  support_summary <- support_summary %>% 
    mutate(supp_set3_ha1_adj_val_1 = 0)
  print("Added supp_set3_ha1_adj_val_1 and filled with NA because it did not exist. Assuming that val_0 is entirely filled with 500s.")
} else{
  print("Did not add column supp_set3_ha1_adj_val_1 It already exists.")
}


### Function 
THR_sample_plots_wl <- function(rules, thr_rules, group = "t", file_name = "plots_wl", data = support_summary) {
  
  
  nplots <- nrow(thr_rules)
  plots_wl <- list()  # Single list for all plots
  
  if(group == "n"){
    gvar <- "t"
    gvar2 <- "n"
    plot_legend_ind <- "Number of Subjects"
    plot_x_axis <- "Measurement Occasions"
    scale_x_ticks <- c(25, 50, 75, 100) # Hardcoded
    x_limits <- c(25, 100)  # Add fixed limits
  } else if(group == "t"){
    gvar <- "n"
    gvar2 <- "t"
    plot_legend_ind <- "Measurement Occasions"
    plot_x_axis <- "Number of Subjects"
    scale_x_ticks <- c(50, 75, 100, 150) # Hardcoded
    x_limits <- c(50, 150)  # Add fixed limits
  }
  
  for(i in 1:nplots){
    # Get column names from thr_rules to access in rules
    rule1_col <- thr_rules$rules_1[i]
    rule_cond_col <- thr_rules$condition_rules[i]
    
    gor_supp_1 <- thr_rules$select_gor[i]
    gor_supp_0 <- thr_rules$select_gor2[i]
    
    # Get the condition IDs from rules using the column names from thr_rules
    rule1_conditions <- rules[[rule1_col]]
    rule_cond_conditions <- rules[[rule_cond_col]]
    
    p <- data %>% 
      filter(condition_id %in% rule1_conditions & condition_id %in% rule_cond_conditions) %>% 
      dplyr::select(condition_id, !!sym(gor_supp_1), !!sym(gor_supp_0), n, t) %>% 
      mutate(
        # Handle the cases where values might be NA or zero
        percent_1 = case_when(
          is.na(!!sym(gor_supp_1)) & is.na(!!sym(gor_supp_0)) ~ NA_real_,
          is.na(!!sym(gor_supp_1)) ~ 0.0,
          is.na(!!sym(gor_supp_0)) ~ 1.0,
          !!sym(gor_supp_1) + !!sym(gor_supp_0) == 0 ~ NA_real_,
          TRUE ~ round(!!sym(gor_supp_1) / (!!sym(gor_supp_1) + !!sym(gor_supp_0)), 3)
        ),
        percent_0 = case_when(
          is.na(!!sym(gor_supp_1)) & is.na(!!sym(gor_supp_0)) ~ NA_real_,
          is.na(!!sym(gor_supp_0)) ~ 0.0,
          is.na(!!sym(gor_supp_1)) ~ 1.0,
          !!sym(gor_supp_1) + !!sym(gor_supp_0) == 0 ~ NA_real_,
          TRUE ~ round(!!sym(gor_supp_0) / (!!sym(gor_supp_1) + !!sym(gor_supp_0)), 3)
        )
      ) %>% 
      # Convert percentage from 0-1 scale to 0-100 scale
      mutate(
        percent_1 = percent_1 * 100,
        percent_0 = percent_0 * 100
      ) %>%
      ggplot(aes(x = !!sym(gvar), y = percent_1, color = as.factor(!!sym(gvar2)), group = as.factor(!!sym(gvar2)))) +
      geom_line(alpha = 0.5) + 
      geom_point(size = 1.5) +
      scale_color_discrete(name = paste0(plot_legend_ind)) +
      
      scale_y_continuous(
        limits = c(0, 100), # Hardcoded!
        breaks = seq(0, 100, by = 25) #Hardcoded!
      ) +
      scale_x_continuous(
        limits = x_limits,
        breaks = scale_x_ticks
      ) +
      labs(
        x = paste(plot_x_axis), 
        y = "THR GORICA H(%)"
      ) +
      theme(
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom"
      ) +
      scale_color_viridis_d(option = "viridis", name = plot_legend_ind)
    
    # Create a name for the plot that's descriptive
    plot_name <- thr_rules$plot[i]
    
    # Add to the single named list
    plots_wl[[plot_name]] <- p
  }
  
  # Assign the plots list to the global environment
  assign(file_name, plots_wl, envir = .GlobalEnv)
  
  # Simply return the plots list
  return(plots_wl)
}

THR_sample_plots_wl(rules = rules, thr_rules = thr_plot_rules, group = "n")




# PLOT TRANSFORM
for (i in 1:nrow(plot_transform_rules)) {
  # Get the plot name from the data frame
  plot_name <- plot_transform_rules$plot[i]
  
  # Check if this plot exists in the plots list
  if (plot_name %in% names(plots_wl)) {
    # Start with base plot
    modified_plot <- plots_wl[[plot_name]]
    
    # Initialize an empty theme call
    plot_theme <- theme(
      legend.position = "bottom")
    
    # Check subtitle - if it has content, add it; if empty, blank it out
    if (!is.na(plot_transform_rules$subtitle_person[i]) && 
        plot_transform_rules$subtitle_person[i] != "" && 
        !is.null(plot_transform_rules$subtitle_person[i])) {
      modified_plot <- modified_plot + labs(subtitle = TeX(plot_transform_rules$subtitle_person[i]))
    } else {
      plot_theme <- plot_theme + theme(plot.subtitle = element_blank())
    }
    
    # Check y-axis - if it has content, add it; if empty, blank it out
    if (exists("person_y", where = plot_transform_rules) && 
        !is.na(plot_transform_rules$y_axis[i]) && 
        plot_transform_rules$y_axis[i] != "" && 
        !is.null(plot_transform_rules$y_axis[i])) {
      modified_plot <- modified_plot + labs(y = TeX(plot_transform_rules$y_axis[i]))
    } else {
      plot_theme <- plot_theme + theme(axis.title.y = element_blank())
    }
    
    if (exists("x_axis", where = plot_transform_rules) &&
        !is.na(plot_transform_rules$x_axis[i]) &&
        plot_transform_rules$x_axis[i] != "" &&
        !is.null(plot_transform_rules$x_axis[i])) {
      modified_plot <- modified_plot + labs(x = TeX(plot_transform_rules$x_axis[i]))
    } else {
      plot_theme <- plot_theme + theme(axis.title.x = element_blank())
    }
    
    # Apply the final theme
    plots_wl[[plot_name]] <- modified_plot + plot_theme
    
  } else {
    warning(paste("Plot", plot_name, "not found in plots list"))
  }
}


################################################################################

empty <- ggplot() +
  scale_x_continuous(
    breaks = c(0, 50, 75, 100, 150),
    limits = c(0, 150)
  ) +
  scale_y_continuous(
    limits = c(0, 0.1)
  ) +
  theme_classic() +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    axis.line         = element_line(color = "white"),
    axis.ticks        = element_line(color = "white"),
    axis.text.x       = element_text(color = "white"),
    axis.text.y       = element_text(color = "white"),
    axis.title.x      = element_text(color = "white"),
    axis.title.y      = element_text(color = "white"),
    panel.grid        = element_blank()
  )


################################################################################
source("./data_archive/post_processing/source_functions/FUNCTION_PLOT_ARRANGE.R")

plot_set1set4_h1_ha2_and_adj_pers <- grid_arrange_shared_legend_with_two_texts(
  plots_wl$set1h1_true_p210, plots_wl$set1h1_true_p210_adj,
  empty, empty,
  plots_wl$set2h1_true_p210, plots_wl$set2h1_true_p210_adj,
  empty, empty,
  plots_wl$set3ha1c_true_p210, plots_wl$set3ha1c_true_p210_adj,
  empty, empty,
  plots_wl$set4ha2c_true_p210, plots_wl$set4ha2c_true_p210_adj,
  empty, empty,
  ncol = 2, nrow = 8,
  cell_heights = c(1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1))

grid::grid.newpage()
vp <- grid::viewport(width = 0.95, height = 0.96, y = 0.48)  # Reduce height to 95%
grid::pushViewport(vp)
grid::grid.draw(plot_set1set4_h1_ha2_and_adj_pers)
grid::popViewport()

grid::grid.text(TeX("$\\phi_{12} = 0.20, \ \\phi_{21} = 0.10$"), 
                x = 0.53, y = 0.97, 
                gp = grid::gpar(fontsize = 11))


grid::grid.text(c("Measurement Occasions"), 
                x = 0.295, y = 0.055, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Measurement Occasions"), 
                x = 0.765, y = 0.055, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Hypothesis Set 1"),
                x = 0.01, y = 0.84, rot = 90)

grid::grid.text(c("Hypothesis Set 2"),
                x = 0.01, y = 0.608, rot = 90)

grid::grid.text(c("Hypothesis Set 3"),
                x = 0.01, y = 0.37, rot = 90)

grid::grid.text(c("Hypothesis Set 4"),
                x = 0.01, y = 0.139, rot = 90)

# Figure 2
plot_set1set2_h1_h0_and_adj <- grid_arrange_shared_legend_with_two_texts(
  plots_wl$set1h1_true_p210, plots_wl$set1h1_true_p215, 
  plots_wl$set1h1_true_p2175,
  plots_wl$set1h1_true_p210_adj, plots_wl$set1h1_true_p215_adj, 
  plots_wl$set1h1_true_p2175_adj,
  plots_wl$set2h1_true_p210, plots_wl$set2h1_true_p215, 
  plots_wl$set2h1_true_p2175,
  plots_wl$set2h1_true_p210_adj, plots_wl$set2h1_true_p215_adj, 
  plots_wl$set2h1_true_p2175_adj,
  ncol = 3, nrow = 4,
  position = "bottom",
  bottom_text = "",
  top_text = "",
  text_size = 12,
  text_angle = 90  # Vertical text (90 degrees)
)


######################### Figure 8

plot_p1515_pers <- grid_arrange_shared_legend_with_two_texts(plots_wl$set1h1_false_p1515, plots_wl$set1h1_false_p1515_adj,
                                                             empty, empty,
                                                             plots_wl$set2h0_true_p1515, plots_wl$set2h0_true_p1515_adj,
                                                             empty, empty,
                                                             plots_wl$set3ha1_true_p1515, plots_wl$set3ha1_true_p1515_adj,
                                                             empty, empty,
                                                             plots_wl$set4ha2_true_p1515, plots_wl$set4ha2_true_p1515_adj,
                                                             empty, empty,
                                                             ncol = 2, nrow = 8, cell_heights = c(1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1))

grid::grid.newpage()
vp <- grid::viewport(width = 0.95, height = 0.96, y = 0.48)  # Reduce height to 95%
grid::pushViewport(vp)
grid::grid.draw(plot_p1515_pers)
grid::popViewport()

grid::grid.text(TeX("$\\phi_{12} = 0.15, \ \\phi_{21} = 0.15$"), 
                x = 0.53, y = 0.97, 
                gp = grid::gpar(fontsize = 11))


grid::grid.text(c("Measurement Occasions"), 
                x = 0.290, y = 0.055, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Measurement Occasions"), 
                x = 0.765, y = 0.055, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Hypothesis Set 1"),
                x = 0.01, y = 0.835, rot = 90)

grid::grid.text(c("Hypothesis Set 2"),
                x = 0.01, y = 0.61, rot = 90)

grid::grid.text(c("Hypothesis Set 3"),
                x = 0.01, y = 0.375, rot = 90)

grid::grid.text(c("Hypothesis Set 4"),
                x = 0.01, y = 0.143, rot = 90)
