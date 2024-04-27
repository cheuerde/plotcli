#' @title Scatter plot using plotcli
#' @description Create a scatter plot using plotcli. Short alias: \code{pclis}.
#' @param x A numeric vector of x values
#' @param y A numeric vector of y values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "scatter")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#' 
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' plotcli_scatter(x, y)
plotcli_scatter <- function(
                            y,
                            x = NULL, 
                            plot_width = getOption("plotcli.plot_width", 80),
                            plot_height = getOption("plotcli.plot_height", 40),
                            x_label = "x", 
                            y_label = "y", 
                            color = NULL,
                            braille = getOption("plotcli.braille", TRUE),
                            name = "scatter",
                            ...
                            ) {

  if(is.null(y)) stop("Please provide x and/or y values.")
  if(is.null(x)) x <- 1:length(y)

  plot <- plotcli$new(
                      plot_width = plot_width, 
                      plot_height = plot_height, 
                      x_label = x_label,
                      y_label = y_label, 
                      draw_legend = FALSE,
                      ...
                      )

  data <- list(x = x, y = y, type = "scatter", color = color, braille = braille, name = name)
  plot$add_data(data)
  return(plot)

}

#' @title Density plot using plotcli
#' @description Create a density plot using plotcli. Short alias: \code{pcld}.
#' @param x A numeric vector of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "Density")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "density")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' plotcli_density(x)
plotcli_density <- function(
                            x, 
                            plot_width = getOption("plotcli.plot_width", 80),
                            plot_height = getOption("plotcli.plot_height", 40),
                            x_label = "x", 
                            y_label = "Density", 
                            color = NULL,
                            braille = getOption("plotcli.braille", TRUE),
                            name = "density",
                            ...
                            ) {

  y <- stats::density(x)$y
  x <- stats::density(x)$x

  plot <- plotcli$new(
                      plot_width = plot_width,
                      plot_height = plot_height,
                      x_label = x_label,
                      y_label = y_label,
                      draw_legend = FALSE,
                      ...
                      )

  data <- list(x = x, y = y, type = "line", color = color, braille = braille, name = name)
  plot$add_data(data)
  return(plot)

}

#' @title Line plot using plotcli
#' @description Create a line plot using plotcli. Short alias: \code{pcli}.
#' @param x A numeric vector of x values
#' @param y A numeric vector of y values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "line")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- 1:10
#' y <- x^2
#' plotcli_line(x, y)
plotcli_line <- function(
                         y, 
                         x = NULL,
                         plot_width = getOption("plotcli.plot_width", 80),
                         plot_height = getOption("plotcli.plot_height", 40),
                         x_label = "x", 
                         y_label = "y", 
                         color = NULL,
                         braille = getOption("plotcli.braille", TRUE),
                         name = "line",
                         ...
                         ) {

  if(is.null(y)) stop("Please provide x and/or y values.")
  if(is.null(x)) x <- 1:length(y)

  plot <- plotcli$new(
                      plot_width = plot_width,
                      plot_height = plot_height,
                      x_label = x_label,
                      y_label = y_label,
                      draw_legend = FALSE,
                      ...
                      )

  data <- list(x = x, y = y, type = "line", color = color, braille = braille, name = name)
  plot$add_data(data)
  return(plot)

}

#' @title Histogram plot using plotcli
#' @description Create a histogram plot using plotcli. Short alias: \code{pclih}.
#' @param x A numeric vector of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "Frequency")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param bin_width Width of the bins (default: NULL)
#' @param ylim y limits (default: NULL)
#' @param name Name of the plot element (default: "histogram")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' plotcli_histogram(x)
plotcli_histogram <- function(
                              x, 
                              plot_width = getOption("plotcli.plot_width", 80),
                              plot_height = getOption("plotcli.plot_height", 40),
                              x_label = "x", 
                              y_label = "Frequency", 
                              color = NULL,
                              braille = getOption("plotcli.braille", TRUE),
                              bin_width = NULL,
                              ylim = NULL,
                              name = "histogram",
                              ...
                              ) {

  hist_data <- graphics::hist(
                    x, 
                    plot = FALSE, 
                    breaks = if (!is.null(bin_width)) seq(min(x), max(x) + bin_width, by = bin_width) else "Sturges"
                    )

  y <- hist_data$counts
  x <- hist_data$mids

  if(is.null(ylim)) ylim <- c(0, max(y) * 1.05)

  plot <- plotcli$new(
                      plot_width = plot_width,
                      plot_height = plot_height,
                      x_label = x_label,
                      y_label = y_label,
                      draw_legend = FALSE,
                      ylim = ylim,
                      ...
                      )

  data <- list(x = x, y = y, type = "barplot", color = color, braille = braille, name = name)
  plot$add_data(data)
  return(plot)

}

#' @title Bar plot using plotcli
#' @description Create a bar plot using plotcli. Short alias: \code{pclb}.
#' @param x A vector of categories
#' @param y A numeric vector of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "barplot")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- c(10, 15, 8, 12, 6)
#' plotcli_bar(x, y)
plotcli_bar <- function(
                        y, 
                        x = NULL,
                        plot_width = getOption("plotcli.plot_width", 80),
                        plot_height = getOption("plotcli.plot_height", 40),
                        x_label = "x", 
                        y_label = "y", 
                        color = NULL,
                        braille = getOption("plotcli.braille", TRUE),
                        name = "barplot",
                        ...
                        ) {

  if(is.null(y)) stop("Please provide x and/or y values.")
  if(is.null(x)) x <- 1:length(y)

  plot <- plotcli$new(
                      plot_width = plot_width,
                      plot_height = plot_height,
                      x_label = x_label,
                      y_label = y_label,
                      draw_legend = FALSE,
                      ...
                      )


  data <- list(x = x, y = y, type = "barplot", color = color, braille = braille, name = name)
  plot$add_data(data)
  return(plot)

}

#' @title Box plot using plotcli
#' @description Create a box plot using plotcli. Short alias: \code{pclbx}.
#' @param x A vector of categories
#' @param y A list of numeric vectors of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "boxplot")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' y <- rnorm(50, mean = 0)
#' plotcli_box(y)
plotcli_box <- function(
                     y, 
                     plot_width = getOption("plotcli.plot_width", 80),
                     plot_height = getOption("plotcli.plot_height", 40),
                     x_label = "x", 
                     y_label = "y", 
                     color = NULL,
                     braille = getOption("plotcli.braille", TRUE),
                     name = "boxplot",
                     ...
                     ) {

  plot <- plotcli$new(
                      plot_width = plot_width,
                      plot_height = plot_height,
                      x_label = x_label,
                      y_label = y_label,
                      draw_legend = FALSE,
                      ...
                      )

  x = rep(1, length(y))
  data <- list(x = x, y = y, type = "boxplot", color = color, braille = braille, name = name)
  plot$add_data(data)
  return(plot)

}

# short versions

#' @title Short version of plotcli_scatter
#' @description Short version of plotcli_scatter function.
#' @param x A numeric vector of x values
#' @param y A numeric vector of y values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "scatter")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#' 
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' pclis(x, y)
pclis <- plotcli_scatter

#' @title Short version of plotcli_density
#' @description Short version of plotcli_density function.
#' @param x A numeric vector of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "Density")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "density")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' pclid(x)
pclid <- plotcli_density

#' @title Short version of plotcli_line
#' @description Short version of plotcli_line function.
#' @param x A numeric vector of x values
#' @param y A numeric vector of y values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "line")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- 1:10
#' y <- x^2
#' pclil(x, y)
pclil <- plotcli_line

#' @title Short version of plotcli_histogram
#' @description Short version of plotcli_histogram function.
#' @param x A numeric vector of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "Frequency")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param bin_width Width of the bins (default: NULL)
#' @param ylim y limits (default: NULL)
#' @param name Name of the plot element (default: "histogram")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' pclih(x)
pclih <- plotcli_histogram

#' @title Short version of plotcli_bar
#' @description Short version of plotcli_bar function.
#' @param x A vector of categories
#' @param y A numeric vector of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "barplot")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- c(10, 15, 8, 12, 6)
#' pclib(x, y)
pclib <- plotcli_bar

#' @title Short version of plotcli_box
#' @description Short version of plotcli_box function.
#' @param x A vector of categories
#' @param y A list of numeric vectors of values
#' @param plot_width Width of the plot (default: 80)
#' @param plot_height Height of the plot (default: 40)
#' @param x_label Label for the x-axis (default: "x")
#' @param y_label Label for the y-axis (default: "y")
#' @param color Color of the plot elements (default: NULL)
#' @param braille Use Braille characters for the plot (default: TRUE)
#' @param name Name of the plot element (default: "boxplot")
#' @param ... Additional arguments passed to the plotcli$new() function
#' @export
#'
#' @examples
#' y <- rnorm(50, mean = 0)
#' pclib(y)
pclibx <- plotcli_box