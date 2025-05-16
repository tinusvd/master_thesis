
#### APPENDIX ####

############################### BETWEEN ########################################
# PLOT TRANSFORM
for (i in 1:nrow(plot_transform_rules)) {
  # Get the plot name from the data frame
  plot_name <- plot_transform_rules$plot[i]
  
  # Check if this plot exists in the plots list
  if (plot_name %in% names(plots)) {
    # Start with base plot
    modified_plot <- plots[[plot_name]]
    
    # Initialize an empty theme call
    plot_theme <- theme(
      legend.position = "bottom")
    
    # Check subtitle - if it has content, add it; if empty, blank it out
    if (!is.na(plot_transform_rules$appendix_between_subtitle[i]) && 
        plot_transform_rules$appendix_between_subtitle[i] != "" && 
        !is.null(plot_transform_rules$appendix_between_subtitle[i])) {
      modified_plot <- modified_plot + labs(subtitle = TeX(plot_transform_rules$appendix_between_subtitle[i]))
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
    plots[[plot_name]] <- modified_plot + plot_theme
    
  } else {
    warning(paste("Plot", plot_name, "not found in plots list"))
  }
}


######################### Figure APPENDIX 3 A #################################

bp_set3 <- grid_arrange_shared_legend_with_two_texts(
  plots$set3ha1_true_p2175, plots$set3ha1_true_p1515,
  empty, empty,
  plots$set3ha1_true_p2175_adj, plots$set3ha1_true_p1515_adj,
  empty, empty,
  ncol = 2, nrow = 4,
  cell_heights = c(1.5, 0.1, 1.5, 0.1)
)

grid::grid.newpage()
vp <- grid::viewport(width = 1, height = 0.96, y = 0.48)  # Reduce height to 95%
grid::pushViewport(vp)
grid::grid.draw(bp_set3)
grid::popViewport()

grid::grid.text(TeX("Hypothesis Set 3"), 
                x = 0.50, y = 0.97, 
                gp = grid::gpar(fontsize = 11))


grid::grid.text(c("Number of Subjects"), 
                x = 0.267, y = 0.08, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Number of Subjects"), 
                x = 0.767, y = 0.08, 
                gp = grid::gpar(fontsize = 11))




######################### Figure APPENDIX 3 B #################################


bp_set4 <- grid_arrange_shared_legend_with_two_texts(
  plots$set4ha2c_true_p210_adj, plots$set4ha2c_true_p215_adj,
  empty, empty, 
  plots$set4ha2c_true_p2175_adj, plots$set4ha2_true_p1515_adj,
  empty, empty,
  ncol = 2, nrow = 4,
  cell_heights = c(1.5, 0.1, 1.5, 0.1)
)


grid::grid.newpage()
vp <- grid::viewport(width = 1, height = 0.96, y = 0.48)  # Reduce height to 95%
grid::pushViewport(vp)
grid::grid.draw(bp_set4)
grid::popViewport()

grid::grid.text(TeX("Hypothesis Set 4"), 
                x = 0.50, y = 0.97, 
                gp = grid::gpar(fontsize = 11))



grid::grid.text(c("Number of Subjects"), 
                x = 0.267, y = 0.08, 
                gp = grid::gpar(fontsize = 11))

grid::grid.text(c("Number of Subjects"), 
                x = 0.767, y = 0.08, 
                gp = grid::gpar(fontsize = 11))

