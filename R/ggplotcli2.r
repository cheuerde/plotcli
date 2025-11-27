#' Convert ggplot2 to Terminal Plot (v2)
#'
#' This is the new implementation using the Canvas abstraction and geom registry.
#' It properly uses ggplot_build() to get computed data and dispatches to
#' registered geom handlers.
#'
#' @param p A ggplot2 object
#' @param width Character width of the plot (default: 60)
#' @param height Character height of the plot (default: 20)
#' @param canvas_type Type of canvas: "braille", "block", or "ascii" (default: "braille")
#' @param show_axes Whether to show axes (default: TRUE)
#' @param show_legend Whether to show legend (default: TRUE)
#' @param title Optional title override
#'
#' @return Invisibly returns the canvas object
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' ggplotcli2(p)
ggplotcli2 <- function(p,
                       width = 60,
                       height = 20,
                       canvas_type = "braille",
                       show_axes = TRUE,
                       show_legend = TRUE,
                       title = NULL) {
  
  # Build the plot to get computed data
  built <- ggplot2::ggplot_build(p)
  
  # Get plot title
  plot_title <- title
  if (is.null(plot_title) && !is.null(built$plot$labels$title)) {
    plot_title <- built$plot$labels$title
  }
  
  # Check for faceting
  layout <- built$layout
  facet_info <- get_facet_info(layout)
  
  if (facet_info$has_facets) {
    # Render faceted plot
    render_faceted_plot(built, facet_info, width, height, canvas_type, 
                        show_axes, plot_title)
  } else {
    # Render single panel plot
    render_single_panel(built, width, height, canvas_type, show_axes, plot_title)
  }
}


#' Render a single panel (non-faceted) plot
#' @keywords internal
render_single_panel <- function(built, width, height, canvas_type, show_axes, plot_title) {
  # Calculate margins for axes
  left_margin <- if (show_axes) 6 else 0
  bottom_margin <- if (show_axes) 2 else 0
  top_margin <- if (!is.null(plot_title)) 1 else 0
  
  # Calculate plot area dimensions
  plot_width <- width - left_margin
  plot_height <- height - bottom_margin - top_margin
  
  # Create canvas for plot area
  canvas <- create_canvas(plot_width, plot_height, canvas_type)
  
  # Create scales
  scales <- create_scales(built, canvas$pixel_width, canvas$pixel_height)
  
  # Process each layer
  for (i in seq_along(built$data)) {
    layer_data <- built$data[[i]]
    layer <- built$plot$layers[[i]]
    
    # Get geom class name
    geom_class <- class(layer$geom)[1]
    
    # Get handler
    handler <- get_geom_handler(geom_class)
    
    if (is.null(handler)) {
      warning(sprintf("No handler registered for geom: %s", geom_class))
      next
    }
    
    # Get layer parameters
    params <- layer$aes_params
    
    # Call handler
    tryCatch({
      handler(layer_data, canvas, scales, params)
    }, error = function(e) {
      warning(sprintf("Error rendering %s: %s", geom_class, e$message))
    })
  }
  
  # Build the final output matrix
  output <- build_plot_output(
    canvas = canvas,
    scales = scales,
    width = width,
    height = height,
    show_axes = show_axes,
    title = plot_title
  )
  
  # Print
  cat("\n")
  for (i in seq_len(nrow(output))) {
    cat(paste(output[i, ], collapse = ""), "\n")
  }
  
  invisible(canvas)
}


#' Build Plot Output with Axes and Title
#'
#' @param canvas The rendered canvas
#' @param scales The scales object
#' @param width Total width
#' @param height Total height
#' @param show_axes Whether to show axes
#' @param title Optional title
#' @return Character matrix
#' @keywords internal
build_plot_output <- function(canvas, scales, width, height, show_axes, title) {
  # Get rendered canvas
  rendered <- canvas$render()
  
  if (!show_axes) {
    return(rendered)
  }
  
  # Calculate margins
  left_margin <- 6
  top_margin <- if (!is.null(title)) 1 else 0
  
  # Create output matrix
  output <- matrix(" ", nrow = height, ncol = width)
  
  # Add title if present
  if (!is.null(title)) {
    title_chars <- strsplit(substr(title, 1, width), "")[[1]]
    start_col <- max(1, floor((width - length(title_chars)) / 2))
    for (i in seq_along(title_chars)) {
      output[1, start_col + i - 1] <- title_chars[i]
    }
  }
  
  # Copy canvas to output (offset by margins)
  for (i in seq_len(nrow(rendered))) {
    for (j in seq_len(ncol(rendered))) {
      output[i + top_margin, j + left_margin] <- rendered[i, j]
    }
  }
  
  # Draw Y axis
  y_ticks <- pretty(scales$y_range, n = 5)
  y_ticks <- y_ticks[y_ticks >= scales$y_range[1] & y_ticks <= scales$y_range[2]]
  
  for (tick in y_ticks) {
    # Calculate row position
    y_frac <- (tick - scales$y_range[1]) / (scales$y_range[2] - scales$y_range[1])
    row <- round(nrow(rendered) - y_frac * (nrow(rendered) - 1)) + top_margin
    
    if (row >= 1 && row <= height) {
      # Format tick label
      label <- format_axis_label(tick)
      label_chars <- strsplit(label, "")[[1]]
      
      # Right-align in left margin
      start_col <- max(1, left_margin - length(label_chars))
      for (i in seq_along(label_chars)) {
        if (start_col + i - 1 <= left_margin) {
          output[row, start_col + i - 1] <- label_chars[i]
        }
      }
    }
  }
  
  # Draw X axis
  x_row <- height
  x_ticks <- pretty(scales$x_range, n = 5)
  x_ticks <- x_ticks[x_ticks >= scales$x_range[1] & x_ticks <= scales$x_range[2]]
  
  for (tick in x_ticks) {
    # Calculate column position
    x_frac <- (tick - scales$x_range[1]) / (scales$x_range[2] - scales$x_range[1])
    col <- round(x_frac * (ncol(rendered) - 1)) + left_margin + 1
    
    if (col >= left_margin && col <= width) {
      # Format tick label
      label <- format_axis_label(tick)
      label_chars <- strsplit(label, "")[[1]]
      
      # Center label under tick position
      start_col <- col - floor(length(label_chars) / 2)
      for (i in seq_along(label_chars)) {
        if (start_col + i - 1 >= 1 && start_col + i - 1 <= width) {
          output[x_row, start_col + i - 1] <- label_chars[i]
        }
      }
    }
  }
  
  return(output)
}


#' Format Axis Label
#'
#' @param value Numeric value
#' @return Formatted string
#' @keywords internal
format_axis_label <- function(value) {
  if (abs(value) < 0.01 || abs(value) >= 10000) {
    sprintf("%.1e", value)
  } else if (abs(value) < 1) {
    sprintf("%.2f", value)
  } else if (abs(value) < 100) {
    sprintf("%.1f", value)
  } else {
    sprintf("%.0f", value)
  }
}


# ============================================================================
# Faceting Support
# ============================================================================

#' Get Facet Information from Layout
#'
#' @param layout The layout object from ggplot_build
#' @return List with facet info
#' @keywords internal
get_facet_info <- function(layout) {
  # Check for faceting
  facet <- layout$facet
  facet_class <- class(facet)[1]
  
  # Get panel layout
  panel_layout <- layout$layout
  
  if (facet_class == "FacetNull" || is.null(panel_layout) || nrow(panel_layout) <= 1) {
    return(list(
      has_facets = FALSE,
      type = "none",
      n_panels = 1,
      n_rows = 1,
      n_cols = 1,
      layout = NULL,
      facet_vars = NULL
    ))
  }
  
  # Determine facet type and dimensions
  n_panels <- nrow(panel_layout)
  n_rows <- max(panel_layout$ROW)
  n_cols <- max(panel_layout$COL)
  
  # Get facet variable names
  facet_vars <- setdiff(names(panel_layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
  
  # Get facet labels for each panel
  panel_labels <- lapply(seq_len(n_panels), function(i) {
    row <- panel_layout[i, ]
    labels <- sapply(facet_vars, function(v) as.character(row[[v]]))
    paste(labels, collapse = ", ")
  })
  
  list(
    has_facets = TRUE,
    type = facet_class,
    n_panels = n_panels,
    n_rows = n_rows,
    n_cols = n_cols,
    layout = panel_layout,
    facet_vars = facet_vars,
    panel_labels = panel_labels
  )
}


#' Render Faceted Plot
#'
#' @param built Result from ggplot_build
#' @param facet_info Facet information from get_facet_info
#' @param width Total width
#' @param height Total height
#' @param canvas_type Canvas type
#' @param show_axes Whether to show axes
#' @param plot_title Plot title
#' @keywords internal
render_faceted_plot <- function(built, facet_info, width, height, canvas_type, 
                                 show_axes, plot_title) {
  n_rows <- facet_info$n_rows
  n_cols <- facet_info$n_cols
  panel_layout <- facet_info$layout
  
  # Calculate dimensions for each panel
  # Reserve space for: title, facet labels, axes
  top_margin <- if (!is.null(plot_title)) 2 else 1
  left_margin <- if (show_axes) 6 else 0
  
  # Calculate panel dimensions
  panel_width <- floor((width - left_margin) / n_cols)
  panel_height <- floor((height - top_margin - 2) / n_rows)  # -2 for x-axis
  
  # Create output matrix
  output <- matrix(" ", nrow = height, ncol = width)
  
  # Add title if present
  if (!is.null(plot_title)) {
    title_chars <- strsplit(substr(plot_title, 1, width), "")[[1]]
    start_col <- max(1, floor((width - length(title_chars)) / 2))
    for (i in seq_along(title_chars)) {
      output[1, start_col + i - 1] <- title_chars[i]
    }
  }
  
  # Render each panel
  for (panel_idx in seq_len(facet_info$n_panels)) {
    panel_row <- panel_layout$ROW[panel_idx]
    panel_col <- panel_layout$COL[panel_idx]
    panel_id <- panel_layout$PANEL[panel_idx]
    
    # Calculate panel position in output
    out_row_start <- top_margin + (panel_row - 1) * panel_height
    out_col_start <- left_margin + (panel_col - 1) * panel_width
    
    # Create canvas for this panel (slightly smaller for facet label)
    canvas_height <- panel_height - 1  # Reserve 1 row for facet label
    canvas_width <- panel_width - 1     # Small gap between panels
    
    if (canvas_height < 3 || canvas_width < 5) {
      warning("Panel too small to render")
      next
    }
    
    canvas <- create_canvas(canvas_width, canvas_height, canvas_type)
    
    # Create scales for this panel
    panel_params <- built$layout$panel_params[[panel_idx]]
    scales <- create_panel_scales(panel_params, canvas$pixel_width, canvas$pixel_height)
    
    # Render layers for this panel
    for (layer_idx in seq_along(built$data)) {
      layer_data <- built$data[[layer_idx]]
      layer <- built$plot$layers[[layer_idx]]
      
      # Filter data for this panel
      if ("PANEL" %in% names(layer_data)) {
        panel_data <- layer_data[layer_data$PANEL == panel_id, ]
      } else {
        panel_data <- layer_data
      }
      
      if (nrow(panel_data) == 0) next
      
      # Get geom handler
      geom_class <- class(layer$geom)[1]
      handler <- get_geom_handler(geom_class)
      
      if (is.null(handler)) next
      
      # Render
      tryCatch({
        handler(panel_data, canvas, scales, layer$aes_params)
      }, error = function(e) {
        warning(sprintf("Error rendering %s in panel %d: %s", 
                        geom_class, panel_idx, e$message))
      })
    }
    
    # Get rendered canvas
    rendered <- canvas$render()
    
    # Add facet label
    label <- facet_info$panel_labels[[panel_idx]]
    label <- substr(label, 1, canvas_width)  # Truncate if needed
    label_chars <- strsplit(label, "")[[1]]
    label_start <- out_col_start + max(0, floor((canvas_width - length(label_chars)) / 2))
    for (i in seq_along(label_chars)) {
      if (label_start + i - 1 <= width) {
        output[out_row_start, label_start + i - 1] <- label_chars[i]
      }
    }
    
    # Copy canvas to output
    for (i in seq_len(nrow(rendered))) {
      for (j in seq_len(ncol(rendered))) {
        out_r <- out_row_start + i
        out_c <- out_col_start + j - 1
        if (out_r <= height && out_c <= width && out_c >= 1) {
          output[out_r, out_c] <- rendered[i, j]
        }
      }
    }
    
    # Add Y axis for leftmost panels
    if (panel_col == 1 && show_axes) {
      y_ticks <- pretty(scales$y_range, n = 3)
      y_ticks <- y_ticks[y_ticks >= scales$y_range[1] & y_ticks <= scales$y_range[2]]
      
      for (tick in y_ticks) {
        y_frac <- (tick - scales$y_range[1]) / (scales$y_range[2] - scales$y_range[1])
        row <- out_row_start + 1 + round((1 - y_frac) * (canvas_height - 1))
        
        if (row >= 1 && row <= height) {
          label <- format_axis_label(tick)
          label_chars <- strsplit(label, "")[[1]]
          start_col <- max(1, left_margin - length(label_chars))
          for (i in seq_along(label_chars)) {
            if (start_col + i - 1 <= left_margin && start_col + i - 1 >= 1) {
              output[row, start_col + i - 1] <- label_chars[i]
            }
          }
        }
      }
    }
    
    # Add X axis for bottom panels
    if (panel_row == n_rows && show_axes) {
      x_row <- out_row_start + canvas_height + 1
      if (x_row <= height) {
        x_ticks <- pretty(scales$x_range, n = 3)
        x_ticks <- x_ticks[x_ticks >= scales$x_range[1] & x_ticks <= scales$x_range[2]]
        
        for (tick in x_ticks) {
          x_frac <- (tick - scales$x_range[1]) / (scales$x_range[2] - scales$x_range[1])
          col <- out_col_start + round(x_frac * (canvas_width - 1))
          
          if (col >= 1 && col <= width) {
            label <- format_axis_label(tick)
            label_chars <- strsplit(label, "")[[1]]
            start_col <- col - floor(length(label_chars) / 2)
            for (i in seq_along(label_chars)) {
              if (start_col + i - 1 >= 1 && start_col + i - 1 <= width) {
                output[x_row, start_col + i - 1] <- label_chars[i]
              }
            }
          }
        }
      }
    }
  }
  
  # Print
  cat("\n")
  for (i in seq_len(nrow(output))) {
    cat(paste(output[i, ], collapse = ""), "\n")
  }
  
  invisible(NULL)
}


#' Create Scales for a Single Panel
#'
#' @param panel_params Panel parameters from ggplot_build
#' @param plot_width Pixel width
#' @param plot_height Pixel height
#' @return List with scale functions
#' @keywords internal
create_panel_scales <- function(panel_params, plot_width, plot_height) {
  # Get x and y ranges from panel params
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range
  
  # Fallback if ranges not available
  if (is.null(x_range)) x_range <- c(0, 1)
  if (is.null(y_range)) y_range <- c(0, 1)
  
  # Create scaling functions
  x_scale <- function(x) {
    ((x - x_range[1]) / (x_range[2] - x_range[1])) * (plot_width - 1) + 1
  }
  
  y_scale <- function(y) {
    plot_height - ((y - y_range[1]) / (y_range[2] - y_range[1])) * (plot_height - 1)
  }
  
  list(
    x = x_scale,
    y = y_scale,
    x_range = x_range,
    y_range = y_range,
    width = plot_width,
    height = plot_height
  )
}

