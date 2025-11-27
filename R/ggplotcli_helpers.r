#' Extract Plot Style from ggplot Theme
#' @keywords internal
extract_plot_style <- function(built, border, grid, legend) {
  theme <- built$plot$theme
  
  # Determine border style
  if (border == "auto") {
    # Check if theme has panel.border
    has_border <- !is.null(theme$panel.border) && 
                  !inherits(theme$panel.border, "element_blank")
    # Or axis lines (like theme_classic)
    has_axis_line <- !is.null(theme$axis.line) && 
                     !inherits(theme$axis.line, "element_blank")
    border <- has_border || has_axis_line
  }
  
  # Determine grid style
  if (grid == "auto") {
    has_major <- !is.null(theme$panel.grid.major) && 
                 !inherits(theme$panel.grid.major, "element_blank")
    has_minor <- !is.null(theme$panel.grid.minor) && 
                 !inherits(theme$panel.grid.minor, "element_blank")
    
    if (has_major && has_minor) {
      grid <- "both"
    } else if (has_major) {
      grid <- "major"
    } else if (has_minor) {
      grid <- "minor"
    } else {
      grid <- "none"
    }
  }
  
  # Determine legend position
  if (legend == "auto") {
    if (!is.null(theme$legend.position)) {
      pos <- theme$legend.position
      if (is.character(pos)) {
        legend <- pos
      } else {
        legend <- "right"  # Default for numeric positions
      }
    } else {
      legend <- "right"
    }
  }
  
  list(
    border = border,
    grid = grid,
    legend = legend
  )
}


#' Extract Plot Labels from ggplot
#' @keywords internal
extract_plot_labels <- function(built, title, subtitle, caption, axis_labels) {
  labels <- built$plot$labels
  
  list(
    title = if (!is.null(title)) title else labels$title,
    subtitle = if (subtitle) labels$subtitle else NULL,
    caption = if (caption) labels$caption else NULL,
    x = if (axis_labels) labels$x else NULL,
    y = if (axis_labels) labels$y else NULL,
    colour = labels$colour,
    fill = labels$fill
  )
}


#' Extract Legend Information from ggplot
#' @keywords internal
extract_legend_info <- function(built) {
  # Try to get color/colour scale
  color_scale <- built$plot$scales$get_scales("colour")
  fill_scale <- built$plot$scales$get_scales("fill")
  
  legend_items <- list()
  
  # Extract from color scale
  if (!is.null(color_scale)) {
    tryCatch({
      breaks <- color_scale$get_breaks()
      labels <- color_scale$get_labels()
      n <- length(breaks)
      if (n > 0) {
        colors <- color_scale$palette(n)
        legend_items$colour <- list(
          title = built$plot$labels$colour %||% "colour",
          labels = labels,
          colors = colors
        )
      }
    }, error = function(e) NULL)
  }
  
  # Extract from fill scale
  if (!is.null(fill_scale)) {
    tryCatch({
      breaks <- fill_scale$get_breaks()
      labels <- fill_scale$get_labels()
      n <- length(breaks)
      if (n > 0) {
        colors <- fill_scale$palette(n)
        legend_items$fill <- list(
          title = built$plot$labels$fill %||% "fill",
          labels = labels,
          colors = colors
        )
      }
    }, error = function(e) NULL)
  }
  
  legend_items
}


#' Render a single panel (non-faceted) plot
#' @keywords internal
render_single_panel <- function(built, width, height, canvas_type, style_opts) {
  labels <- style_opts$labels
  show_axes <- style_opts$show_axes
  
  # Calculate margins
  left_margin <- if (show_axes) 7 else 0
  right_margin <- 1
  bottom_margin <- if (show_axes) 2 else 0
  if (!is.null(labels$x)) bottom_margin <- bottom_margin + 1
  
  # Top margin for title/subtitle
  top_margin <- 0
  if (!is.null(labels$title)) top_margin <- top_margin + 1
  if (!is.null(labels$subtitle)) top_margin <- top_margin + 1
  
  # Bottom for caption
  caption_margin <- if (!is.null(labels$caption)) 1 else 0
  
  # Calculate plot area dimensions
  plot_width <- width - left_margin - right_margin
  plot_height <- height - bottom_margin - top_margin - caption_margin
  
  if (plot_width < 5 || plot_height < 3) {
    warning("Plot area too small")
    return(invisible(NULL))
  }
  
  # Create canvas for plot area
  canvas <- create_canvas(plot_width, plot_height, canvas_type)
  
  # Create scales (with border padding if needed)
  scales <- create_scales(built, canvas$pixel_width, canvas$pixel_height, 
                          has_border = style_opts$border)
  
  # Draw grid lines first (behind data)
  if (style_opts$grid != "none") {
    draw_grid(canvas, scales, style_opts$grid)
  }
  
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
    
    # Call handler (pass style_opts for geoms that need it, like boxplot)
    tryCatch({
      handler(layer_data, canvas, scales, params, style_opts)
    }, error = function(e) {
      warning(sprintf("Error rendering %s: %s", geom_class, e$message))
    })
  }
  
  # Draw border if requested
  if (style_opts$border) {
    draw_border(canvas)
  }
  
  # Extract legend information
  legend_info <- extract_legend_info(built)
  
  # Build the final output matrix
  output <- build_plot_output_v2(
    canvas = canvas,
    scales = scales,
    width = width,
    height = height,
    style_opts = style_opts,
    left_margin = left_margin,
    top_margin = top_margin,
    legend_info = legend_info
  )
  
  # Print
  cat("\n")
  for (i in seq_len(nrow(output))) {
    cat(paste(output[i, ], collapse = ""), "\n")
  }
  
  invisible(canvas)
}


#' Draw Grid Lines on Canvas
#' @keywords internal
draw_grid <- function(canvas, scales, grid_type) {
  # Use a subtle character for grid
  grid_color <- "silver"
  
  # Major grid at tick positions
  if (grid_type %in% c("major", "both")) {
    # Vertical lines at x ticks
    x_ticks <- pretty(scales$x_range, n = 5)
    x_ticks <- x_ticks[x_ticks > scales$x_range[1] & x_ticks < scales$x_range[2]]
    
    for (tick in x_ticks) {
      x <- scales$x(tick)
      canvas$draw_vline(round(x), color = grid_color)
    }
    
    # Horizontal lines at y ticks
    y_ticks <- pretty(scales$y_range, n = 5)
    y_ticks <- y_ticks[y_ticks > scales$y_range[1] & y_ticks < scales$y_range[2]]
    
    for (tick in y_ticks) {
      y <- scales$y(tick)
      canvas$draw_hline(round(y), color = grid_color)
    }
  }
  
  # Minor grid (more lines)
  if (grid_type %in% c("minor", "both")) {
    x_ticks <- pretty(scales$x_range, n = 10)
    x_ticks <- x_ticks[x_ticks > scales$x_range[1] & x_ticks < scales$x_range[2]]
    
    for (tick in x_ticks) {
      x <- scales$x(tick)
      # Only draw if not already a major grid line
      if (grid_type == "minor" || !(tick %in% pretty(scales$x_range, n = 5))) {
        canvas$draw_vline(round(x), color = grid_color)
      }
    }
    
    y_ticks <- pretty(scales$y_range, n = 10)
    y_ticks <- y_ticks[y_ticks > scales$y_range[1] & y_ticks < scales$y_range[2]]
    
    for (tick in y_ticks) {
      y <- scales$y(tick)
      if (grid_type == "minor" || !(tick %in% pretty(scales$y_range, n = 5))) {
        canvas$draw_hline(round(y), color = grid_color)
      }
    }
  }
}


#' Draw Border Around Canvas
#' @keywords internal
draw_border <- function(canvas) {
  # Draw rectangle around the entire canvas
  canvas$draw_rect(1, 1, canvas$pixel_width, canvas$pixel_height, color = NULL)
}


#' Build Plot Output with Axes and Title (v2)
#'
#' @param canvas The rendered canvas
#' @param scales The scales object
#' @param width Total width
#' @param height Total height
#' @param style_opts Style options
#' @param left_margin Left margin size
#' @param top_margin Top margin size
#' @param legend_info Legend information from extract_legend_info
#' @return Character matrix
#' @keywords internal
build_plot_output_v2 <- function(canvas, scales, width, height, style_opts, 
                                  left_margin, top_margin, legend_info = NULL) {
  # Get rendered canvas
  rendered <- canvas$render()
  labels <- style_opts$labels
  show_axes <- style_opts$show_axes
  title_align <- style_opts$title_align
  legend_position <- style_opts$legend
  
  # Create output matrix
  output <- matrix(" ", nrow = height, ncol = width)
  
  current_row <- 1
  
  # Add title if present
  if (!is.null(labels$title)) {
    title_text <- substr(labels$title, 1, width - 2)
    title_chars <- strsplit(title_text, "")[[1]]
    
    if (title_align == "center") {
      start_col <- max(1, floor((width - length(title_chars)) / 2))
    } else {
      start_col <- left_margin + 1
    }
    
    for (i in seq_along(title_chars)) {
      if (start_col + i - 1 <= width) {
        output[current_row, start_col + i - 1] <- title_chars[i]
      }
    }
    current_row <- current_row + 1
  }
  
  # Add subtitle if present
  if (!is.null(labels$subtitle)) {
    sub_text <- substr(labels$subtitle, 1, width - 2)
    sub_chars <- strsplit(sub_text, "")[[1]]
    
    if (title_align == "center") {
      start_col <- max(1, floor((width - length(sub_chars)) / 2))
    } else {
      start_col <- left_margin + 1
    }
    
    for (i in seq_along(sub_chars)) {
      if (start_col + i - 1 <= width) {
        output[current_row, start_col + i - 1] <- sub_chars[i]
      }
    }
    current_row <- current_row + 1
  }
  
  # Copy canvas to output (offset by margins)
  for (i in seq_len(nrow(rendered))) {
    for (j in seq_len(ncol(rendered))) {
      out_row <- i + top_margin
      out_col <- j + left_margin
      if (out_row <= height && out_col <= width) {
        output[out_row, out_col] <- rendered[i, j]
      }
    }
  }
  
  # Draw Y axis values
  if (show_axes) {
    y_ticks <- pretty(scales$y_range, n = 5)
    y_ticks <- y_ticks[y_ticks >= scales$y_range[1] & y_ticks <= scales$y_range[2]]
    
    for (tick in y_ticks) {
      y_frac <- (tick - scales$y_range[1]) / (scales$y_range[2] - scales$y_range[1])
      row <- round(nrow(rendered) - y_frac * (nrow(rendered) - 1)) + top_margin
      
      if (row >= 1 && row <= height) {
        label <- format_axis_label(tick)
        label_chars <- strsplit(label, "")[[1]]
        
        start_col <- max(1, left_margin - length(label_chars))
        for (i in seq_along(label_chars)) {
          if (start_col + i - 1 < left_margin) {
            output[row, start_col + i - 1] <- label_chars[i]
          }
        }
      }
    }
    
    # Draw X axis values
    x_row <- top_margin + nrow(rendered) + 1
    x_ticks <- pretty(scales$x_range, n = 5)
    x_ticks <- x_ticks[x_ticks >= scales$x_range[1] & x_ticks <= scales$x_range[2]]
    
    for (tick in x_ticks) {
      x_frac <- (tick - scales$x_range[1]) / (scales$x_range[2] - scales$x_range[1])
      col <- round(x_frac * (ncol(rendered) - 1)) + left_margin + 1
      
      if (col >= left_margin && col <= width && x_row <= height) {
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
  
  # Add Y axis label (rotated - shown vertically on left)
  if (!is.null(labels$y) && left_margin >= 3) {
    y_label <- labels$y
    # For terminal, just show abbreviated label at top-left
    y_chars <- strsplit(substr(y_label, 1, min(nchar(y_label), nrow(rendered))), "")[[1]]
    label_start <- top_margin + floor((nrow(rendered) - length(y_chars)) / 2)
    for (i in seq_along(y_chars)) {
      row <- label_start + i
      if (row >= 1 && row <= height) {
        output[row, 1] <- y_chars[i]
      }
    }
  }
  
  # Add X axis label (centered below x values)
  if (!is.null(labels$x)) {
    x_row <- top_margin + nrow(rendered) + 2
    if (x_row <= height) {
      x_label <- substr(labels$x, 1, ncol(rendered))
      x_chars <- strsplit(x_label, "")[[1]]
      start_col <- left_margin + floor((ncol(rendered) - length(x_chars)) / 2)
      for (i in seq_along(x_chars)) {
        if (start_col + i - 1 >= 1 && start_col + i - 1 <= width) {
          output[x_row, start_col + i - 1] <- x_chars[i]
        }
      }
    }
  }
  
  # Add caption (bottom right)
  if (!is.null(labels$caption)) {
    cap_row <- height
    cap_text <- substr(labels$caption, 1, width - 2)
    cap_chars <- strsplit(cap_text, "")[[1]]
    start_col <- width - length(cap_chars)
    for (i in seq_along(cap_chars)) {
      if (start_col + i - 1 >= 1) {
        output[cap_row, start_col + i - 1] <- cap_chars[i]
      }
    }
  }
  
  # Add legend if present and not "none"
  if (!is.null(legend_info) && length(legend_info) > 0 && 
      !identical(legend_position, "none")) {
    output <- add_legend_to_output(output, legend_info, legend_position, 
                                    top_margin, nrow(rendered))
  }
  
  return(output)
}


#' Add Legend to Output Matrix
#' @keywords internal
add_legend_to_output <- function(output, legend_info, position, top_margin, plot_height) {
  # Get the first legend (colour or fill)
  legend <- legend_info$colour %||% legend_info$fill
  if (is.null(legend)) return(output)
  
  n_items <- length(legend$labels)
  if (n_items == 0) return(output)
  
  # Calculate legend dimensions
  max_label_len <- max(nchar(legend$labels))
  legend_width <- max_label_len + 3  # "* label"
  
  height <- nrow(output)
  width <- ncol(output)
  
  if (position %in% c("right", "auto")) {
    # Add legend to the right side
    # Create legend column
    legend_col <- matrix(" ", nrow = height, ncol = legend_width)
    
    # Center legend vertically in plot area
    legend_start_row <- top_margin + max(1, floor((plot_height - n_items - 1) / 2))
    
    # Add title if present
    if (!is.null(legend$title) && nchar(legend$title) > 0) {
      title_chars <- strsplit(substr(legend$title, 1, legend_width - 1), "")[[1]]
      for (i in seq_along(title_chars)) {
        if (legend_start_row <= height) {
          legend_col[legend_start_row, i] <- title_chars[i]
        }
      }
      legend_start_row <- legend_start_row + 1
    }
    
    # Add each legend item
    for (i in seq_len(n_items)) {
      row <- legend_start_row + i - 1
      if (row <= height && row >= 1) {
        # Color indicator (use terminal color)
        term_color <- color_to_term(legend$colors[i])
        legend_col[row, 1] <- make_colored("*", term_color)
        
        # Label
        label_chars <- strsplit(legend$labels[i], "")[[1]]
        for (j in seq_along(label_chars)) {
          if (j + 2 <= legend_width) {
            legend_col[row, j + 2] <- label_chars[j]
          }
        }
      }
    }
    
    # Append legend to output
    output <- cbind(output, legend_col)
  }
  
  output
}


#' Build Plot Output with Axes and Title (legacy)
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
  # Legacy wrapper - convert to new style
  style_opts <- list(
    show_axes = show_axes,
    title_align = "center",
    legend = "none",
    labels = list(title = title, subtitle = NULL, caption = NULL, x = NULL, y = NULL)
  )
  
  left_margin <- if (show_axes) 6 else 0
  top_margin <- if (!is.null(title)) 1 else 0
  
  build_plot_output_v2(canvas, scales, width, height, style_opts, left_margin, top_margin, NULL)
}


#' Format Axis Label
#'
#' @param value Numeric value
#' @return Formatted string
#' @keywords internal
format_axis_label <- function(value) {
  # Handle exact zero
  if (value == 0) {
    return("0")
  }
  # Use scientific notation for very small or very large numbers
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
#' @param style_opts Style options
#' @keywords internal
render_faceted_plot <- function(built, facet_info, width, height, canvas_type, 
                                 style_opts) {
  n_rows <- facet_info$n_rows
  n_cols <- facet_info$n_cols
  panel_layout <- facet_info$layout
  labels <- style_opts$labels
  show_axes <- style_opts$show_axes
  
  # Calculate dimensions for each panel
  # Reserve space for: title, facet labels, axes
  top_margin <- if (!is.null(labels$title)) 2 else 1
  left_margin <- if (show_axes) 7 else 0
  
  # Calculate panel dimensions
  panel_width <- floor((width - left_margin) / n_cols)
  panel_height <- floor((height - top_margin - 2) / n_rows)  # -2 for x-axis
  
  # Create output matrix
  output <- matrix(" ", nrow = height, ncol = width)
  
  # Add title if present
  if (!is.null(labels$title)) {
    title_chars <- strsplit(substr(labels$title, 1, width), "")[[1]]
    if (style_opts$title_align == "center") {
      start_col <- max(1, floor((width - length(title_chars)) / 2))
    } else {
      start_col <- left_margin + 1
    }
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
    
    # Create scales for this panel (with border padding if needed)
    panel_params <- built$layout$panel_params[[panel_idx]]
    scales <- create_panel_scales(panel_params, canvas$pixel_width, canvas$pixel_height, 
                                   has_border = style_opts$border)
    
    # Draw grid lines first (behind data)
    if (style_opts$grid != "none") {
      draw_grid(canvas, scales, style_opts$grid)
    }
    
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
      
      # Render (pass style_opts for geoms that need it, like boxplot)
      tryCatch({
        handler(panel_data, canvas, scales, layer$aes_params, style_opts)
      }, error = function(e) {
        warning(sprintf("Error rendering %s in panel %d: %s", 
                        geom_class, panel_idx, e$message))
      })
    }
    
    # Draw border if requested
    if (style_opts$border) {
      draw_border(canvas)
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
  
  # Add legend if present
  legend_info <- extract_legend_info(built)
  if (!is.null(legend_info) && length(legend_info) > 0 && 
      !identical(style_opts$legend, "none")) {
    # Calculate plot height for legend centering
    plot_height <- panel_height * n_rows
    output <- add_legend_to_output(output, legend_info, style_opts$legend, 
                                    top_margin, plot_height)
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
#' @param has_border Whether a border will be drawn (adds padding)
#' @return List with scale functions
#' @keywords internal
create_panel_scales <- function(panel_params, plot_width, plot_height, has_border = FALSE) {

  # Get x and y ranges from panel params
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range
  
  # Fallback if ranges not available
  if (is.null(x_range)) x_range <- c(0, 1)
  if (is.null(y_range)) y_range <- c(0, 1)
  
  # Add padding if border is present to prevent data from overlapping border
  padding <- if (has_border) 2 else 0
  x_min <- 1 + padding
  x_max <- plot_width - padding
  y_min <- 1 + padding
  y_max <- plot_height - padding
  
  # Ensure we have valid ranges
  if (x_max <= x_min) {
    x_min <- 1
    x_max <- plot_width
  }
  if (y_max <= y_min) {
    y_min <- 1
    y_max <- plot_height
  }
  
  # Create scaling functions
  x_scale <- function(x) {
    ((x - x_range[1]) / (x_range[2] - x_range[1])) * (x_max - x_min) + x_min
  }
  
  y_scale <- function(y) {
    y_max - ((y - y_range[1]) / (y_range[2] - y_range[1])) * (y_max - y_min)
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

