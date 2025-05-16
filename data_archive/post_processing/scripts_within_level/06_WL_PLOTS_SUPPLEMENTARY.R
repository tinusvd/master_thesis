
#### APPENDIX ####
# Within-Person plots

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
    if (!is.na(plot_transform_rules$appendix_person_subtitle[i]) && 
        plot_transform_rules$appendix_person_subtitle[i] != "" && 
        !is.null(plot_transform_rules$appendix_person_subtitle[i])) {
      modified_plot <- modified_plot + labs(subtitle = TeX(plot_transform_rules$appendix_person_subtitle[i]))
    } else {
      plot_theme <- plot_theme + theme(plot.subtitle = element_blank())
    }
    
    # Check y-axis - if it has content, add it; if empty, blank it out
    if (exists("y_axis", where = plot_transform_rules) && 
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


############################### P_2715 & P_215 #################################




wp_p215p2175 <- grid_arrange_shared_legend_with_two_texts(
  # Row 1: set1, person 215 & 2175
  plots_wl$set1h1_true_p215,    plots_wl$set1h1_true_p215_adj,
  plots_wl$set1h1_true_p2175,   plots_wl$set1h1_true_p2175_adj,
  # Row 2: spacer
  empty, empty, empty, empty,
  # Row 3: set2, person 215 & 2175
  plots_wl$set2h1_true_p215,    plots_wl$set2h1_true_p215_adj,
  plots_wl$set2h1_true_p2175,   plots_wl$set2h1_true_p2175_adj,
  # Row 4: spacer
  empty, empty, empty, empty,
  # Row 5: set3, person 215 & 2175
  plots_wl$set3ha1_true_p215,   plots_wl$set3ha1_true_p215_adj,
  plots_wl$set3ha1_true_p2175,  plots_wl$set3ha1_true_p2175_adj,
  # Row 6: spacer
  empty, empty, empty, empty,
  # Row 7: set4, person 215 & 2175
  plots_wl$set4ha2c_true_p215,  plots_wl$set4ha2c_true_p215_adj,
  plots_wl$set4ha2c_true_p2175, plots_wl$set4ha2c_true_p2175_adj,
  empty, empty,
  
  ncol = 4, nrow = 8,
  cell_heights = c(
    1.5,  # row 1
    0.1,  # spacer
    1.5,  # row 3
    0.1,  # spacer
    1.5,  # row 5
    0.1,  # spacer
    1.5,   # row 7
    0.1
  )
)


grid::grid.newpage()
vp <- grid::viewport(width = 0.95, height = 0.96, y = 0.48)  # Reduce height to 95%
grid::pushViewport(vp)
grid::grid.draw(wp_p215p2175)
grid::popViewport()

grid::grid.text(TeX("$\\phi_{12} = 0.20, \ \\phi_{21} = 0.15$"), 
                x = 0.29, y = 0.97, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(TeX("$\\phi_{12} = 0.20, \ \\phi_{21} = 0.175$"), 
                x = 0.77, y = 0.97, 
                gp = grid::gpar(fontsize = 11))


grid::grid.text(c("Measurement Occasions"), 
                x = 0.165, y = 0.05, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Measurement Occasions"), 
                x = 0.880, y = 0.05, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Measurement Occasions"), 
                x = 0.405, y = 0.05, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Measurement Occasions"), 
                x = 0.640, y = 0.05, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Hypothesis Set 1"),
                x = 0.01, y = 0.835, rot = 90)

grid::grid.text(c("Hypothesis Set 2"),
                x = 0.01, y = 0.61, rot = 90)

grid::grid.text(c("Hypothesis Set 3"),
                x = 0.01, y = 0.375, rot = 90)

grid::grid.text(c("Hypothesis Set 4"),
                x = 0.01, y = 0.143, rot = 90)
