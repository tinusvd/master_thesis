# Grid function from Baptiste Auguié (https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html)
library(grid)
library(gridExtra)

# Original shared‐legend helper
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
  }


# Patched version with two‐texts and adjustable cell heights
grid_arrange_shared_legend_with_two_texts <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right"),
           top_text = NULL,
           bottom_text = NULL,
           text_size = 14,
           text_angle = 90,
           align_top_text_to_first_row = FALSE,
           top_rows = 1,              # rows reserved for the top text
           bottom_rows = NULL,        # rows reserved for the bottom text
           cell_heights = rep(1, nrow) # relative heights for each row
  ) {
    
    plots <- list(...)
    position <- match.arg(position)
    
    # extract legend from first plot
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    
    # strip legends from all plots
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    # build main plot area with custom row heights
    heights_units <- unit(cell_heights, "null")
    main_grob <- do.call(arrangeGrob, c(gl, list(heights = heights_units)))
    
    # determine bottom_rows if not set
    if (is.null(bottom_rows)) {
      bottom_rows <- nrow - top_rows
    }
    
    # if text labels are requested, build a left‐side text column
    if (!is.null(top_text) || !is.null(bottom_text)) {
      top_grob <- textGrob(top_text, gp = gpar(fontsize = text_size), rot = text_angle)
      bottom_grob <- textGrob(bottom_text, gp = gpar(fontsize = text_size), rot = text_angle)
      
      # arrange the two text grobs in a single column
      text_column <- arrangeGrob(
        top_grob, bottom_grob,
        ncol = 1,
        heights = unit(c(top_rows, bottom_rows), "null")
      )
      
      # combine text column + main plots horizontally
      text_width <- unit(2, "lines")
      plots_with_text <- arrangeGrob(
        text_column, main_grob,
        ncol = 2,
        widths = unit.c(text_width, unit(1, "npc") - text_width)
      )
    } else {
      plots_with_text <- main_grob
    }
    
    # finally, tack on the shared legend
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        plots_with_text,
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        plots_with_text,
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    invisible(combined)
  }


grid_arrange_shared_legend_with_two_texts_align <-
  function(...,
           ncol = 2,  # Fixed at 2 columns
           position = c("bottom", "right"),
           top_text = NULL,
           bottom_text = NULL,
           text_size = 14,
           text_angle = 90,
           align_top_text_to_first_row = FALSE,
           top_rows = 1,              # rows reserved for the top text
           bottom_rows = NULL,        # rows reserved for the bottom text
           cell_heights = NULL        # relative heights for each row
  ) {
    
    plots <- list(...)
    position <- match.arg(position)
    num_plots <- length(plots)
    
    # Calculate number of rows needed
    full_rows <- floor(num_plots / 2)
    has_centered_plot <- (num_plots %% 2 == 1)
    total_rows <- full_rows
    if (has_centered_plot) {
      total_rows <- total_rows + 1
    }
    
    # Set up cell_heights if not provided
    if (is.null(cell_heights)) {
      cell_heights <- rep(1, total_rows)
    } else if (length(cell_heights) < total_rows) {
      cell_heights <- rep(cell_heights, length.out = total_rows)
    }
    
    # Extract legend from first plot
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend_idx <- which(sapply(g, function(x) x$name) == "guide-box")
    
    if (length(legend_idx) == 0) {
      warning("No legend found in the first plot.")
      legend <- nullGrob()
      lheight <- unit(0, "cm")
      lwidth <- unit(0, "cm")
    } else {
      legend <- g[[legend_idx]]
      lheight <- sum(legend$height)
      lwidth <- sum(legend$width)
    }
    
    # Strip legends from all plots
    plots_no_legend <- lapply(plots, function(x) x + theme(legend.position = "none"))
    
    # Handle centered bottom plot layout if needed
    if (has_centered_plot) {
      # Create an empty grob for spacing
      empty_grob <- rectGrob(gp = gpar(col = NA, fill = NA))
      
      # Create full rows (with 2 plots each)
      rows_list <- list()
      
      # Process full rows (with 2 plots each)
      for (r in 1:full_rows) {
        start_idx <- (r-1) * 2 + 1
        end_idx <- r * 2
        rows_list[[r]] <- arrangeGrob(
          plots_no_legend[[start_idx]], plots_no_legend[[end_idx]],
          ncol = 2
        )
      }
      
      # Add centered plot in last row
      bottom_plot <- plots_no_legend[[num_plots]]
      bottom_row <- arrangeGrob(
        empty_grob, bottom_plot, empty_grob,
        ncol = 3,
        widths = unit(c(0.5, 1, 0.5), "null")
      )
      
      rows_list[[total_rows]] <- bottom_row
      
      # Combine all rows
      main_grob <- arrangeGrob(
        grobs = rows_list,
        ncol = 1,
        heights = unit(cell_heights, "null")
      )
    } else {
      # Standard case - no centered plot needed
      gl <- plots_no_legend
      heights_units <- unit(cell_heights, "null")
      main_grob <- do.call(arrangeGrob, c(gl, list(ncol = ncol, nrow = total_rows, heights = heights_units)))
    }
    
    # Determine bottom_rows if not set
    if (is.null(bottom_rows)) {
      bottom_rows <- total_rows - top_rows
    }
    
    # If text labels are requested, build a left-side text column
    if (!is.null(top_text) || !is.null(bottom_text)) {
      top_grob <- textGrob(top_text, gp = gpar(fontsize = text_size), rot = text_angle)
      bottom_grob <- textGrob(bottom_text, gp = gpar(fontsize = text_size), rot = text_angle)
      
      # Arrange the two text grobs in a single column
      text_column <- arrangeGrob(
        top_grob, bottom_grob,
        ncol = 1,
        heights = unit(c(top_rows, bottom_rows), "null")
      )
      
      # Combine text column + main plots horizontally
      text_width <- unit(2, "lines")
      plots_with_text <- arrangeGrob(
        text_column, main_grob,
        ncol = 2,
        widths = unit.c(text_width, unit(1, "npc") - text_width)
      )
    } else {
      plots_with_text <- main_grob
    }
    
    # Finally, tack on the shared legend
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        plots_with_text,
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        plots_with_text,
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    invisible(combined)
  }

