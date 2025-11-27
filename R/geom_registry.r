#' Geom Registry and Dispatch System
#'
#' This module provides a registry for geom rendering functions and
#' a dispatch system for converting ggplot2 geoms to terminal plots.
#'
#' @name GeomRegistry
#' @importFrom grDevices col2rgb
#' @importFrom stats density
#' @import R6
#' @import ggplot2
NULL

#' Geom Registry Environment
#'
#' Internal environment storing registered geom handlers
#' @keywords internal
.geom_registry <- new.env(parent = emptyenv())

#' Register a Geom Handler
#'
#' Register a function that can render a specific ggplot2 geom to a canvas.
#'
#' @param geom_name Name of the geom (e.g., "GeomPoint", "GeomLine")
#' @param handler Function that takes (data, canvas, scales, params) and draws to canvas
#' @export
#'
#' @examples
#' register_geom("GeomPoint", function(data, canvas, scales, params) {
#'   # Draw points on canvas
#' })
register_geom <- function(geom_name, handler) {
  if (!is.function(handler)) {
    stop("handler must be a function")
  }
  .geom_registry[[geom_name]] <- handler
  invisible(NULL)
}

#' Get a Geom Handler
#'
#' Retrieve the registered handler for a geom, or NULL if not found.
#'
#' @param geom_name Name of the geom
#' @return The handler function or NULL
#' @export
get_geom_handler <- function(geom_name) {
  if (exists(geom_name, envir = .geom_registry)) {
    return(.geom_registry[[geom_name]])
  }
  return(NULL)
}

#' List Registered Geoms
#'
#' @return Character vector of registered geom names
#' @export
list_registered_geoms <- function() {
  ls(envir = .geom_registry)
}

#' Check if a Geom is Registered
#'
#' @param geom_name Name of the geom
#' @return Logical
#' @export
is_geom_registered <- function(geom_name) {
  exists(geom_name, envir = .geom_registry)
}


# ============================================================================
# Scale Helpers for Geom Handlers
# ============================================================================

#' Create Scale Object from ggplot_build data
#'
#' @param built Result from ggplot_build()
#' @param plot_width Canvas pixel width
#' @param plot_height Canvas pixel height
#' @return List with x_scale and y_scale functions
#' @export
create_scales <- function(built, plot_width, plot_height) {
  # Get the panel parameters (contains scale ranges)
  layout <- built$layout
  panel_params <- layout$panel_params[[1]]
  
  # X scale
  x_range <- panel_params$x.range
  if (is.null(x_range)) {
    x_range <- range(built$data[[1]]$x, na.rm = TRUE)
  }
  
  # Y scale
  y_range <- panel_params$y.range
  if (is.null(y_range)) {
    y_range <- range(built$data[[1]]$y, na.rm = TRUE)
  }
  
  # Create scaling functions
  x_scale <- function(x) {
    ((x - x_range[1]) / (x_range[2] - x_range[1])) * (plot_width - 1) + 1
  }
  
  y_scale <- function(y) {
    # Invert Y because canvas has origin at top-left
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


# ============================================================================
# Built-in Geom Handlers
# ============================================================================

#' GeomPoint Handler
#'
#' Renders points as individual pixels or small shapes
#' @keywords internal
geom_point_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Get color mapping
  colors <- if ("colour" %in% names(data)) data$colour else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    x <- scales$x(data$x[i])
    y <- scales$y(data$y[i])
    
    # Get color for this point
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    # Draw point (could add size support later)
    canvas$set_pixel(round(x), round(y), color)
  }
}

#' GeomLine Handler
#'
#' Renders connected lines
#' @keywords internal
geom_line_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Sort by x to ensure proper line connections
  data <- data[order(data$x), ]
  
  # Group by colour/group if present
  if ("group" %in% names(data)) {
    groups <- unique(data$group)
  } else {
    groups <- 1
    data$group <- 1
  }
  
  for (grp in groups) {
    grp_data <- data[data$group == grp, ]
    if (nrow(grp_data) < 2) next
    
    # Get color
    color <- if ("colour" %in% names(grp_data)) {
      color_to_term(grp_data$colour[1])
    } else {
      NULL
    }
    
    # Scale coordinates
    xs <- sapply(grp_data$x, scales$x)
    ys <- sapply(grp_data$y, scales$y)
    
    # Draw polyline
    canvas$draw_polyline(xs, ys, color)
  }
}

#' GeomPath Handler
#'
#' Renders connected paths (order by data, not x)
#' @keywords internal
geom_path_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Group by colour/group if present
  if ("group" %in% names(data)) {
    groups <- unique(data$group)
  } else {
    groups <- 1
    data$group <- 1
  }
  
  for (grp in groups) {
    grp_data <- data[data$group == grp, ]
    if (nrow(grp_data) < 2) next
    
    # Get color
    color <- if ("colour" %in% names(grp_data)) {
      color_to_term(grp_data$colour[1])
    } else {
      NULL
    }
    
    # Scale coordinates (keep original order)
    xs <- sapply(grp_data$x, scales$x)
    ys <- sapply(grp_data$y, scales$y)
    
    # Draw polyline
    canvas$draw_polyline(xs, ys, color)
  }
}

#' GeomBar/GeomCol Handler
#'
#' Renders bar charts
#' @keywords internal
geom_bar_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Get colors
  colors <- if ("fill" %in% names(data)) data$fill else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    # Bar coordinates
    xmin <- scales$x(data$xmin[i])
    xmax <- scales$x(data$xmax[i])
    ymin <- scales$y(data$ymin[i])
    ymax <- scales$y(data$ymax[i])
    
    # Get color
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    # Fill rectangle (note: y is inverted)
    canvas$fill_rect(round(xmin), round(ymax), round(xmax), round(ymin), color)
  }
}

#' GeomArea Handler
#'
#' Renders filled areas
#' @keywords internal
geom_area_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Sort by x
  data <- data[order(data$x), ]
  
  # Get color
  color <- if ("fill" %in% names(data)) {
    color_to_term(data$fill[1])
  } else {
    NULL
  }
  
  # Scale coordinates
  xs <- sapply(data$x, scales$x)
  ys <- sapply(data$y, scales$y)
  
  # Fill area
  canvas$fill_area(xs, ys, color)
}

#' GeomSegment Handler
#'
#' Renders line segments
#' @keywords internal
geom_segment_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  colors <- if ("colour" %in% names(data)) data$colour else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    x0 <- scales$x(data$x[i])
    y0 <- scales$y(data$y[i])
    x1 <- scales$x(data$xend[i])
    y1 <- scales$y(data$yend[i])
    
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    canvas$draw_segment(x0, y0, x1, y1, arrow_end = FALSE, color = color)
  }
}

#' GeomHline Handler
#'
#' Renders horizontal lines
#' @keywords internal
geom_hline_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  colors <- if ("colour" %in% names(data)) data$colour else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    y <- scales$y(data$yintercept[i])
    
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    canvas$draw_hline(round(y), color = color)
  }
}

#' GeomVline Handler
#'
#' Renders vertical lines
#' @keywords internal
geom_vline_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  colors <- if ("colour" %in% names(data)) data$colour else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    x <- scales$x(data$xintercept[i])
    
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    canvas$draw_vline(round(x), color = color)
  }
}

#' GeomRect Handler
#'
#' Renders rectangles
#' @keywords internal
geom_rect_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  colors <- if ("fill" %in% names(data)) data$fill else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    xmin <- scales$x(data$xmin[i])
    xmax <- scales$x(data$xmax[i])
    ymin <- scales$y(data$ymin[i])
    ymax <- scales$y(data$ymax[i])
    
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    # Note: y is inverted
    canvas$fill_rect(round(xmin), round(ymax), round(xmax), round(ymin), color)
  }
}

#' GeomSmooth Handler
#'
#' Renders smoothed lines (just draws the line, ignores confidence interval)
#' @keywords internal
geom_smooth_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Sort by x
  data <- data[order(data$x), ]
  
  # Get color
  color <- if ("colour" %in% names(data)) {
    color_to_term(data$colour[1])
  } else {
    NULL
  }
  
  # Scale coordinates
  xs <- sapply(data$x, scales$x)
  ys <- sapply(data$y, scales$y)
  
  # Draw polyline
  canvas$draw_polyline(xs, ys, color)
}

#' GeomDensity Handler
#'
#' Renders density curves
#' @keywords internal
geom_density_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Group by group if present
  if ("group" %in% names(data)) {
    groups <- unique(data$group)
  } else {
    groups <- 1
    data$group <- 1
  }
  
  for (grp in groups) {
    grp_data <- data[data$group == grp, ]
    grp_data <- grp_data[order(grp_data$x), ]
    
    if (nrow(grp_data) < 2) next
    
    # Get color
    color <- if ("colour" %in% names(grp_data)) {
      color_to_term(grp_data$colour[1])
    } else {
      NULL
    }
    
    # Use density as y
    xs <- sapply(grp_data$x, scales$x)
    ys <- sapply(grp_data$density, scales$y)
    
    # Draw polyline
    canvas$draw_polyline(xs, ys, color)
  }
}

#' GeomHistogram Handler
#'
#' Renders histograms (same as bar)
#' @keywords internal
geom_histogram_handler <- geom_bar_handler


#' GeomBoxplot Handler
#'
#' Renders boxplots with whiskers, box, median line, and outliers.
#' Supports two styles: "ascii" (box-drawing characters) and "braille" (Braille dots).
#' @keywords internal
geom_boxplot_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  # Get boxplot style (default to "ascii" for classic look)
  boxplot_style <- if (!is.null(style_opts) && !is.null(style_opts$boxplot_style)) {
    style_opts$boxplot_style
  } else {
    "ascii"
  }
  
  # Get colors
  fill_colors <- if ("fill" %in% names(data)) data$fill else rep("white", nrow(data))
  outline_colors <- if ("colour" %in% names(data)) data$colour else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    # Get boxplot statistics
    x <- data$x[i]
    xmin <- data$xmin[i]
    xmax <- data$xmax[i]
    ymin <- data$ymin[i]  # Lower whisker
    lower <- data$lower[i]  # Q1
    middle <- data$middle[i]  # Median
    upper <- data$upper[i]  # Q3
    ymax <- data$ymax[i]  # Upper whisker
    outliers <- data$outliers[[i]]
    
    # Get color
    fill_color <- fill_colors[i]
    if (!is.null(fill_color) && !is.na(fill_color)) {
      fill_color <- color_to_term(fill_color)
    } else {
      fill_color <- NULL
    }
    
    outline_color <- outline_colors[i]
    if (!is.null(outline_color) && !is.na(outline_color)) {
      outline_color <- color_to_term(outline_color)
    } else {
      outline_color <- fill_color
    }
    
    # Scale coordinates
    sx <- scales$x(x)
    sxmin <- scales$x(xmin)
    sxmax <- scales$x(xmax)
    symin <- scales$y(ymin)
    slower <- scales$y(lower)
    smiddle <- scales$y(middle)
    supper <- scales$y(upper)
    symax <- scales$y(ymax)
    
    if (boxplot_style == "ascii") {
      # ASCII style: use box-drawing characters directly on the character grid
      # Convert pixel coordinates to character coordinates
      char_x <- round(sx / canvas$x_mult)
      char_xmin <- round(sxmin / canvas$x_mult)
      char_xmax <- round(sxmax / canvas$x_mult)
      char_ymin <- round(symin / canvas$y_mult)
      char_lower <- round(slower / canvas$y_mult)
      char_middle <- round(smiddle / canvas$y_mult)
      char_upper <- round(supper / canvas$y_mult)
      char_ymax <- round(symax / canvas$y_mult)
      
      # Box-drawing characters
      horiz <- "\u2500"  # horizontal line
      vert <- "\u2502"   # vertical line
      top_left <- "\u250c"     # top left corner
      top_right <- "\u2510"    # top right corner
      bottom_left <- "\u2514"  # bottom left corner
      bottom_right <- "\u2518" # bottom right corner
      
      # Get canvas matrix dimensions
      n_rows <- nrow(canvas$matrix)
      n_cols <- ncol(canvas$matrix)
      
      # Clamp values to valid range
      char_x <- max(1, min(n_cols, char_x))
      char_xmin <- max(1, min(n_cols, char_xmin))
      char_xmax <- max(1, min(n_cols, char_xmax))
      char_ymin <- max(1, min(n_rows, char_ymin))
      char_lower <- max(1, min(n_rows, char_lower))
      char_middle <- max(1, min(n_rows, char_middle))
      char_upper <- max(1, min(n_rows, char_upper))
      char_ymax <- max(1, min(n_rows, char_ymax))
      
      # Draw whiskers (vertical lines)
      whisker_rows_lower <- seq(min(char_lower, char_ymin), max(char_lower, char_ymin))
      for (row in whisker_rows_lower) {
        if (row >= 1 && row <= n_rows && char_x >= 1 && char_x <= n_cols) {
          canvas$matrix[row, char_x] <- make_colored(vert, fill_color)
        }
      }
      whisker_rows_upper <- seq(min(char_upper, char_ymax), max(char_upper, char_ymax))
      for (row in whisker_rows_upper) {
        if (row >= 1 && row <= n_rows && char_x >= 1 && char_x <= n_cols) {
          canvas$matrix[row, char_x] <- make_colored(vert, fill_color)
        }
      }
      
      # Draw box (Q1 to Q3) - calculate box dimensions first
      box_left <- max(1, char_xmin)
      box_right <- min(n_cols, char_xmax)
      box_top <- min(char_lower, char_upper)
      box_bottom <- max(char_lower, char_upper)
      
      # Draw whisker caps (horizontal lines) - same width as box
      for (col in box_left:box_right) {
        if (char_ymin >= 1 && char_ymin <= n_rows) {
          canvas$matrix[char_ymin, col] <- make_colored(horiz, fill_color)
        }
        if (char_ymax >= 1 && char_ymax <= n_rows) {
          canvas$matrix[char_ymax, col] <- make_colored(horiz, fill_color)
        }
      }
      
      # Top and bottom of box
      for (col in box_left:box_right) {
        if (box_top >= 1 && box_top <= n_rows) {
          canvas$matrix[box_top, col] <- make_colored(horiz, fill_color)
        }
        if (box_bottom >= 1 && box_bottom <= n_rows) {
          canvas$matrix[box_bottom, col] <- make_colored(horiz, fill_color)
        }
      }
      
      # Sides of box
      for (row in box_top:box_bottom) {
        if (row >= 1 && row <= n_rows) {
          if (box_left >= 1 && box_left <= n_cols) {
            canvas$matrix[row, box_left] <- make_colored(vert, fill_color)
          }
          if (box_right >= 1 && box_right <= n_cols) {
            canvas$matrix[row, box_right] <- make_colored(vert, fill_color)
          }
        }
      }
      
      # Corners
      if (box_top >= 1 && box_top <= n_rows) {
        if (box_left >= 1 && box_left <= n_cols) {
          canvas$matrix[box_top, box_left] <- make_colored(top_left, fill_color)
        }
        if (box_right >= 1 && box_right <= n_cols) {
          canvas$matrix[box_top, box_right] <- make_colored(top_right, fill_color)
        }
      }
      if (box_bottom >= 1 && box_bottom <= n_rows) {
        if (box_left >= 1 && box_left <= n_cols) {
          canvas$matrix[box_bottom, box_left] <- make_colored(bottom_left, fill_color)
        }
        if (box_right >= 1 && box_right <= n_cols) {
          canvas$matrix[box_bottom, box_right] <- make_colored(bottom_right, fill_color)
        }
      }
      
      # Median line - only inside the box (not including the border)
      if (char_middle >= 1 && char_middle <= n_rows) {
        for (col in (box_left + 1):(box_right - 1)) {
          if (col >= 1 && col <= n_cols) {
            canvas$matrix[char_middle, col] <- make_colored(horiz, fill_color)
          }
        }
      }
      
      # Outliers
      if (length(outliers) > 0 && !all(is.na(outliers))) {
        for (out in outliers) {
          if (!is.na(out)) {
            char_y_out <- round(scales$y(out) / canvas$y_mult)
            if (char_y_out >= 1 && char_y_out <= n_rows &&
                char_x >= 1 && char_x <= n_cols) {
              canvas$matrix[char_y_out, char_x] <- make_colored("*", fill_color)
            }
          }
        }
      }
      
    } else {
      # Braille style: use canvas drawing methods (high resolution)
      # Draw whiskers (vertical lines from box to whisker ends)
      canvas$draw_segment(round(sx), round(slower), round(sx), round(symin), color = fill_color)
      canvas$draw_segment(round(sx), round(supper), round(sx), round(symax), color = fill_color)
      
      # Draw whisker caps (horizontal lines at whisker ends)
      cap_width <- (sxmax - sxmin) / 2
      canvas$draw_segment(round(sx - cap_width/2), round(symin), round(sx + cap_width/2), round(symin), color = fill_color)
      canvas$draw_segment(round(sx - cap_width/2), round(symax), round(sx + cap_width/2), round(symax), color = fill_color)
      
      # Draw box (rectangle from Q1 to Q3)
      canvas$draw_rect(round(sxmin), round(supper), round(sxmax), round(slower), color = fill_color)
      
      # Draw median line
      canvas$draw_segment(round(sxmin), round(smiddle), round(sxmax), round(smiddle), color = fill_color)
      
      # Draw outliers
      if (length(outliers) > 0 && !all(is.na(outliers))) {
        for (out in outliers) {
          if (!is.na(out)) {
            sy_out <- scales$y(out)
            canvas$set_pixel(round(sx), round(sy_out), fill_color)
          }
        }
      }
    }
  }
}


#' GeomText Handler
#'
#' Renders text labels
#' @keywords internal
geom_text_handler <- function(data, canvas, scales, params, style_opts = NULL) {
  colors <- if ("colour" %in% names(data)) data$colour else rep("white", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    x <- scales$x(data$x[i])
    y <- scales$y(data$y[i])
    label <- as.character(data$label[i])
    
    color <- colors[i]
    if (!is.null(color) && !is.na(color)) {
      color <- color_to_term(color)
    } else {
      color <- NULL
    }
    
    # Convert pixel position to character position for text
    char_x <- round(x / canvas$x_mult)
    char_y <- round(y / canvas$y_mult)
    
    canvas$draw_text(char_x, char_y, label, color)
  }
}


# ============================================================================
# Color Conversion
# ============================================================================

#' Convert ggplot2 color to terminal color name
#'
#' @param color A color value (hex, name, or R color)
#' @return A terminal color name (blue, red, green, etc.) or NULL
#' @export
color_to_term <- function(color) {
  if (is.null(color) || is.na(color)) return(NULL)
  
  # If already a terminal color name, return as-is
  term_colors <- c("blue", "red", "green", "yellow", "magenta", "cyan", "white", "black", "silver")
  if (tolower(color) %in% term_colors) {
    return(tolower(color))
  }
  
  # Try to convert hex/named color to RGB and find closest terminal color
  tryCatch({
    rgb <- col2rgb(color)
    r <- rgb[1, 1]
    g <- rgb[2, 1]
    b <- rgb[3, 1]
    
    # Simple heuristic: find dominant channel
    max_val <- max(r, g, b)
    min_val <- min(r, g, b)
    
    if (max_val < 50) return("black")
    if (min_val > 200) return("white")
    
    # Check for grayscale
    if (max_val - min_val < 30) {
      if (max_val > 180) return("white")
      if (max_val > 100) return("silver")
      return("black")
    }
    
    # Find dominant color
    if (r >= g && r >= b) {
      if (g > b && g > 100) return("yellow")
      if (b > g && b > 100) return("magenta")
      return("red")
    } else if (g >= r && g >= b) {
      if (r > b && r > 100) return("yellow")
      if (b > r && b > 100) return("cyan")
      return("green")
    } else {
      if (r > g && r > 100) return("magenta")
      if (g > r && g > 100) return("cyan")
      return("blue")
    }
  }, error = function(e) {
    return(NULL)
  })
}


# ============================================================================
# Register Built-in Geoms
# ============================================================================

.onLoad_geoms <- function() {
  register_geom("GeomPoint", geom_point_handler)
  register_geom("GeomLine", geom_line_handler)
  register_geom("GeomPath", geom_path_handler)
  register_geom("GeomBar", geom_bar_handler)
  register_geom("GeomCol", geom_bar_handler)
  register_geom("GeomArea", geom_area_handler)
  register_geom("GeomSegment", geom_segment_handler)
  register_geom("GeomHline", geom_hline_handler)
  register_geom("GeomVline", geom_vline_handler)
  register_geom("GeomRect", geom_rect_handler)
  register_geom("GeomSmooth", geom_smooth_handler)
  register_geom("GeomDensity", geom_density_handler)
  register_geom("GeomHistogram", geom_histogram_handler)
  register_geom("GeomText", geom_text_handler)
  register_geom("GeomBoxplot", geom_boxplot_handler)
}

# Register geoms when the file is sourced
.onLoad_geoms()

