#' Get data subset for a specific geom
#'
#' This function returns a subset of the data for a specific geom.
#'
#' @param geom_name The name of the geom for which the data subset is needed.
#' @param data The data to be subsetted.
#' @param aes The aesthetic mappings for the geom.
#' @param p_build The ggplot build object.
#' @return A list containing the data subset for the specified geom.
#' @export
get_data_subset <- function(geom_name, data, aes, p_build) {

  out = data

  if (geom_name == "GeomDensity") {
    density_data <- density(data[[rlang::as_name(aes$x)]])
    out = list(x = density_data$x, y = density_data$y)
  } 

  if(geom_name == "GeomSmooth") out =list(x = p_build$data[[2]]$x, y = p_build$data[[2]]$y)

  if(geom_name %in% c("GeomBoxplot")) {

          out$y = data[[rlang::as_name(aes$x)]]
          out$x = rep(1, length(out$y))

  }

  if(!geom_name %in% c("GeomDensity", "GeomSmooth", "GeomBoxplot", "GeomBar")) {

          out$x = data[[rlang::as_name(aes$x)]]
          out$y = data[[rlang::as_name(aes$y)]]

  }

  return(out)

}

#' Safely extract aesthetic name from ggplot mapping
#'
#' Handles quosures, simple symbols, and calls like factor(cyl)
#'
#' @param aes_expr The aesthetic expression (can be a quosure)
#' @return Character string of the column name, or NULL
#' @keywords internal
safe_aes_name <- function(aes_expr) {
  if (is.null(aes_expr)) return(NULL)
  
  # If it's a quosure, extract the expression
  if (rlang::is_quosure(aes_expr)) {
    aes_expr <- rlang::quo_get_expr(aes_expr)
  }
  
  # If it's a simple symbol, use as_name
  if (rlang::is_symbol(aes_expr)) {
    return(rlang::as_name(aes_expr))
  }
  
  # If it's a call (like factor(cyl)), try to extract the first argument
  if (rlang::is_call(aes_expr)) {
    # Get the call arguments
    args <- rlang::call_args(aes_expr)
    if (length(args) > 0) {
      # Recursively try to get the name from the first argument
      return(safe_aes_name(args[[1]]))
    }
  }
  
  # Fallback: try to deparse and clean up
  tryCatch({
    # Last resort: deparse the expression
    deparsed <- deparse(aes_expr)
    # Try to extract column name from factor(col) or similar
    if (grepl("^\\w+\\((.+)\\)$", deparsed)) {
      return(gsub("^\\w+\\((.+)\\)$", "\\1", deparsed))
    }
    return(deparsed)
  }, error = function(e) {
    return(NULL)
  })
}

#' ggplotcli - Render ggplot2 objects in the terminal
#'
#' Convert any ggplot2 plot to a terminal-based visualization using Unicode 
#' Braille characters or ASCII. Supports 15+ geom types, faceting, themes,
#' and color aesthetics.
#'
#' @param p A ggplot2 object to render
#' @param width Character width of the plot (default: 60)
#' @param height Character height of the plot (default: 20)
#' @param canvas_type Type of canvas: "braille" (high-res), "block" (medium), or "ascii" (basic). Default: "braille"
#' @param border Draw border around plot. "auto" uses ggplot theme, or TRUE/FALSE (default: "auto")
#' @param grid Grid lines: "none", "major", "minor", "both", or "auto" (default: "none")
#' @param show_axes Whether to show axis values (default: TRUE)
#' @param axis_labels Whether to show axis labels from ggplot (default: TRUE)
#' @param legend Legend display: "auto", "right", "bottom", "none" (default: "auto")
#' @param title_align Title alignment: "center" or "left" (default: "center")
#' @param subtitle Whether to show subtitle (default: TRUE)
#' @param caption Whether to show caption (default: TRUE)
#' @param title Optional title override (NULL uses ggplot title)
#' @param boxplot_style Style for boxplots: "ascii" uses box-drawing characters (default), 
#'   "braille" uses Braille dots like other geoms
#'
#' @return Invisibly returns the canvas object
#' @export
#'
#' @examples
#' library(ggplot2)
#' 
#' # Basic scatter plot
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' ggplotcli(p)
#' 
#' # With styling
#' ggplotcli(p, border = TRUE, grid = "major")
#' 
#' # Faceted plot
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
#'   geom_point() + 
#'   facet_wrap(~cyl)
#' ggplotcli(p, width = 70, height = 16)
#' 
#' # Multiple geoms
#' p <- ggplot(mtcars, aes(x = mpg)) +
#'   geom_histogram(aes(y = after_stat(density)), bins = 10) +
#'   geom_density(color = "red")
#' ggplotcli(p)
ggplotcli <- function(p,
                      width = 60,
                      height = 20,
                      canvas_type = "braille",
                      border = "auto",
                      grid = "none",
                      show_axes = TRUE,
                      axis_labels = TRUE,
                      legend = "auto",
                      title_align = "center",
                      subtitle = TRUE,
                      caption = TRUE,
                      title = NULL,
                      boxplot_style = "ascii") {
  
  # Build the plot to get computed data
  built <- ggplot2::ggplot_build(p)
  
  # Initialize color mapping for all colors in the plot
  # This ensures we minimize color repetition across groups
  all_colors <- c()
  for (layer_data in built$data) {
    if ("colour" %in% names(layer_data)) {
      all_colors <- c(all_colors, layer_data$colour)
    }
    if ("fill" %in% names(layer_data)) {
      all_colors <- c(all_colors, layer_data$fill)
    }
  }
  init_color_mapping(unique(all_colors))
  
  # Extract styling from ggplot theme
  style <- extract_plot_style(built, border, grid, legend)
  
  # Get plot labels
  labels <- extract_plot_labels(built, title, subtitle, caption, axis_labels)
  
  # Create style options object
  style_opts <- list(
    border = style$border,
    grid = style$grid,
    show_axes = show_axes,
    axis_labels = axis_labels,
    legend = style$legend,
    title_align = title_align,
    labels = labels,
    boxplot_style = boxplot_style
  )
  
  # Check for faceting
  layout <- built$layout

facet_info <- get_facet_info(layout)
  
  if (facet_info$has_facets) {
    # Render faceted plot
    render_faceted_plot(built, facet_info, width, height, canvas_type, 
                        style_opts)
    } else {
    # Render single panel plot
    render_single_panel(built, width, height, canvas_type, style_opts)
  }
}
