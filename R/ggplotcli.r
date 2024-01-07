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

  #if(geom_name %in% c("GeomBar")) {

  #        out$y = data[[rlang::as_name(aes$x)]]
  #        out$x = rep(1, length(out$y))

  #}
    
  if(!geom_name %in% c("GeomDensity", "GeomSmooth", "GeomBoxplot", "GeomBar")) {

          out$x = data[[rlang::as_name(aes$x)]]
          out$y = data[[rlang::as_name(aes$y)]]

  }

  return(out)

}

#' ggplotcli - Render ggplot objects in the terminal
#'
#' This function takes a ggplot object and renders it in the terminal using ASCII or Braille characters.
#'
#' @param ggplot_obj A ggplot object to be rendered in the terminal.
#' @param plot_width Width of the terminal plot in characters (default: 80).
#' @param plot_height Height of the terminal plot in characters (default: 40).
#' @param braille Use Braille characters for higher resolution (default: TRUE).
#'
#' @return A TerminalPlot object.
#' @export
ggplotcli <- function(ggplot_obj, plot_width = 80, plot_height = 40, braille = TRUE) {

  geoms_valid <- data.frame(
    geom = c("GeomPoint", "GeomLine", "GeomDensity", "GeomSmooth", "GeomBar", "GeomBoxplot"),
    plot_type = c("scatter", "line", "line", "line", "barplot", "boxplot")
  )

  p_build = ggplot2::ggplot_build(ggplot_obj)

  data <- ggplot_obj$data
  aes <- ggplot_obj$mapping
  geoms <- lapply(ggplot_obj$layers, function(layer) layer$geom)
  geom_names = unlist(lapply(geoms, function(geom) class(geom)[[1]]))

  terminal_plot <- plotcli$new(
    plot_width,
    plot_height,
    x_label = ggplot_obj$labels$x,
    y_label = ggplot_obj$labels$y,
    title = ggplot_obj$labels$title
  )

  for (i in 1:length(geoms)) {

    geom = geoms[[i]]
    geom_name <- geom_names[i]
    plot_type <- geoms_valid[geoms_valid$geom == geom_name, "plot_type"]

    color_aes <- if (!is.null(aes$colour)) rlang::as_name(aes$colour) else NULL

    # FIXME: need to get barplot to work
    if (!geom_name %in% geoms_valid$geom | geom_name == "GeomBar") stop("Unsupported geom: ", geom_name)

    if (!is.null(color_aes)) {

      aes_levels <- as.character(unique(data[[color_aes]]))
      num_levels <- length(aes_levels)

      this_term_colors <- get_term_colors(num_levels)

      for (j in 1:num_levels) {

        data_subset <- data[data[[color_aes]] == aes_levels[j], ]
        data_subset <- get_data_subset(geom_name, data_subset, aes, p_build)

        terminal_plot$add_data(list(
          x = data_subset$x,
          y = data_subset$y,
          color = this_term_colors[j],
          name = aes_levels[j],
          braille = braille,
          type = plot_type
        ))
      }

    } else {

      data_subset <- get_data_subset(geom_name, data, aes, p_build)
      this_color = ggplot_obj$layers[[i]]$aes_params$colour

      if(!is.null(this_color)) {
        
        if(this_color != "") {

          if(!this_color %in% get_term_colors()) this_color = get_term_colors()[i]

        }

      }

      terminal_plot$add_data(list(
        x = data_subset$x,
        y = data_subset$y,
        color = this_color,
        name = "",
        braille = braille,
        type = plot_type
      ))
    }
  }

  return(terminal_plot)

}