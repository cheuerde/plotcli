#' plotcli R6 Class
#'
#' This class provides a set of methods to create and customize command-line plots using R6.
#' It supports various plot types, such as scatter, line, bar, and box plots, and allows
#' customization of plot elements, such as title, axis labels, ticks, and legend.
#'
#' @section Usage:
#' \preformatted{
#' plotcli <- plotcli$new()
#' plotcli$add_data(data)
#' plotcli$print_plot()
#' }
#' @field plot_width The width of the plot
#' @field plot_height The height of the plot
#' @field plot_canvas The canvas for drawing the plot
#' @field plot_matrix The matrix containing the entire plot, including borders, labels, and title
#' @field data A list containing the data sets to be plotted
#' @field title The title of the plot
#' @field x_label The label for the x-axis
#' @field y_label The label for the y-axis
#' @field ylim The limits for the y-axis
#' @field xlim The limits for the x-axis
#' @field x_min The minimum value of the x-axis
#' @field x_max The maximum value of the x-axis
#' @field y_min The minimum value of the y-axis
#' @field y_max The maximum value of the y-axis
#' @field plot_matrix_canvas_row_start The starting row of the plot canvas within the plot matrix
#' @field plot_matrix_canvas_col_start The starting column of the plot canvas within the plot matrix
#' @field is_boxplot A logical value indicating if the plot is a boxplot
#' @field draw_legend A logical value indicating if the legend should be drawn
#'
#' @section Methods:
#' \describe{
#'   \item{initialize()}{Initializes the PlotCLI object with parameters.}
#'   \item{initialize_plot_matrix()}{Initializes the plot matrix with the plot canvas.}
#'   \item{print()}{Default print method for PlotCLI object.}
#'   \item{add_row()}{Adds a single row to the plot matrix.}
#'   \item{add_col()}{Adds a single column to the plot matrix.}
#'   \item{add_borders()}{Adds borders around the plot canvas.}
#'   \item{add_row_col_index()}{Adds row and column index to the plot matrix.}
#'   \item{add_title()}{Adds a title to the plot matrix.}
#'   \item{add_y_ticks()}{Adds y-axis tick labels to the plot matrix.}
#'   \item{add_y_label()}{Adds a y-axis label to the plot matrix.}
#'   \item{add_x_ticks()}{Adds x-axis tick labels to the plot matrix.}
#'   \item{add_x_label()}{Adds an x-axis label to the plot matrix.}
#'   \item{add_legend()}{Adds a legend to the plot matrix.}
#'   \item{add_data()}{Adds data to the object.}
#'   \item{get_min_max()}{Gets minimum and maximum values for x and y.}
#'   \item{remove_out_of_range_data()}{Removes out of range data points if xlim and ylim were given.}
#'   \item{draw_scatter_plot()}{Draws a scatter plot on the plot canvas.}
#'   \item{draw_line_plot()}{Draws a line plot on the plot canvas.}
#'   \item{draw_barplot()}{Draws a bar plot on the plot canvas.}
#'   \item{draw_barplot_braille()}{Draws a bar plot with braille characters on the plot canvas.}
#'   \item{draw_boxplot()}{Draws a box plot on the plot canvas.}
#'   \item{print_plot()}{Assembles all plot elements and prints the plot to the console.}
#' }
#' @export
#'
#' @examples
#' # Create a new plotcli object
#' plotcli <- plotcli$new()
#'
#' # Add data for a scatter plot
#' plotcli$add_data(list(x = 1:10, y = rnorm(10), type = "scatter", color = "red"))
#'
#' # Print the plot
#' plotcli$print_plot()
#'
plotcli <- R6Class("plotcli",
  public = list(
    plot_width = getOption("plotcli.plot_width", 80),
    plot_height = getOption("plotcli.plot_height", 40),
    plot_canvas = NULL,
    plot_matrix = NULL,
    data = NULL,
    title = NULL,
    x_label = "x",
    y_label = "y",
    ylim = NULL,
    xlim = NULL,
    x_min = NULL,
    x_max = NULL,
    y_min = NULL,
    y_max = NULL,
    # keeping track of the location of the canvas inside the entire plot matrix
    plot_matrix_canvas_row_start = NULL,
    plot_matrix_canvas_col_start = NULL,
    is_boxplot = FALSE,
    draw_legend = TRUE,

    #' @description Initialize object
    #' @param plot_width integer, width of the plot canvas
    #' @param plot_height integer, height of the plot canvas
    #' @param title character, title of the plot
    #' @param x_label character, label for the x-axis
    #' @param y_label character, label for the y-axis
    #' @param xlim numeric vector, limits for the x-axis
    #' @param ylim numeric vector, limits for the y-axis
    #' @param is_boxplot logical, whether the plot is a boxplot
    #' @param draw_legend logical, whether to draw the legend
    initialize = function(
                          plot_width = 60, 
                          plot_height = 20, 
                          x_label = "x", 
                          y_label = "y", 
                          ylim = NULL, 
                          xlim = NULL, 
                          title = NULL, 
                          is_boxplot = FALSE,
                          draw_legend = TRUE
                          ) {
      self$plot_canvas <- matrix(" ", nrow = plot_height, ncol = plot_width)
      self$x_label <- x_label
      self$y_label <- y_label
      self$ylim <- ylim
      self$xlim <- xlim
      self$title <- title
      self$plot_width <- plot_width
      self$plot_height <- plot_height
      self$is_boxplot <- is_boxplot
      self$draw_legend <- draw_legend
    },

    #' This function initializes the plot matrix based on the plot canvas.
    #' @description Initialize the plot matrix
    #' @param plot_width The width of the plot
    #' @param plot_height The height of the plot
    #' @return A plot matrix object
    initialize_plot_matrix = function() {
      self$plot_matrix <- self$plot_canvas
      self$plot_matrix_canvas_row_start <- 1
      self$plot_matrix_canvas_col_start <- 1
    },

    #' @description Default print method for plotcli object
    #' @param ... Additional arguments passed to the print method
    #' @return The plotcli object, invisibly
    print = function(...) {
      self$print_plot()
      invisible(self)
    },

    #' @description Add a single row to the plot matrix
    #' @param bottom logical, if TRUE, add row to the bottom of the matrix, otherwise add to the top (default: FALSE)
    add_row = function(bottom = FALSE) {
      plot_matrix <- self$plot_matrix
      if (bottom) {
        plot_matrix <- rbind(plot_matrix, " ")
      } else {
        plot_matrix <- rbind(" ", plot_matrix)
      }

      # update the canvas location
      self$plot_matrix_canvas_row_start <- self$plot_matrix_canvas_row_start + 1

      self$plot_matrix <- plot_matrix
    },

    #' @description Add a single column to the plot matrix
    add_col = function() {
      plot_matrix <- self$plot_matrix
      plot_matrix <- cbind(" ", plot_matrix)

      # update the canvas location
      self$plot_matrix_canvas_col_start <- self$plot_matrix_canvas_col_start + 1

      self$plot_matrix <- plot_matrix
    },

    #' @description Add borders to the plot matrix
    add_borders = function() {

      plot_matrix <- self$plot_matrix

      # Add horizontal borders
      plot_matrix <- rbind(
        c(top_left_corner_char, rep(horiz_border_char, ncol(plot_matrix)), top_right_corner_char),
        cbind(vert_border_char, plot_matrix, vert_border_char),
        c(bottom_left_corner_char, rep(horiz_border_char, ncol(plot_matrix)), bottom_right_corner_char)
      )

      # update the canvas location
      self$plot_matrix_canvas_row_start <- self$plot_matrix_canvas_row_start + 1
      self$plot_matrix_canvas_col_start <- self$plot_matrix_canvas_col_start + 1

      self$plot_matrix <- plot_matrix
    },

    #' @description Add row and column index to the plot matrix
    add_row_col_index = function() {
      plot_matrix <- self$plot_matrix

      plot_matrix <- rbind(
        c(
          rep(" ", times = self$plot_matrix_canvas_col_start - 1),
          # only take the last character of the number
          substr(1:ncol(self$plot_canvas), nchar(1:ncol(self$plot_canvas)), nchar(1:ncol(self$plot_canvas))),
          rep(" ", times = ncol(plot_matrix) - (self$plot_matrix_canvas_col_start + ncol(self$plot_canvas) - 1))
        ),
        plot_matrix
      )

      plot_matrix <- cbind(
        c(
          rep(" ", times = self$plot_matrix_canvas_row_start),
          substr(1:nrow(self$plot_canvas), nchar(1:nrow(self$plot_canvas)), nchar(1:nrow(self$plot_canvas))),
          rep(" ", times = nrow(plot_matrix) - (self$plot_matrix_canvas_row_start + nrow(self$plot_canvas)))
        ),
        plot_matrix
      )

      # update the canvas location
      self$plot_matrix_canvas_row_start <- self$plot_matrix_canvas_row_start + 1
      self$plot_matrix_canvas_col_start <- self$plot_matrix_canvas_col_start + 1

      self$plot_matrix <- plot_matrix
    },

    #' Add title to the plot matrix
    #'
    #' @param title character, title of the plot
    add_title = function() {
      if (!is.null(self$title)) {
        title <- self$title
        plot_matrix <- self$plot_matrix
        title_vec <- strsplit(title, "")[[1]]
        nchar_title <- length(title_vec)
        title_col_start <- self$plot_matrix_canvas_col_start + floor((ncol(self$plot_canvas) - nchar_title) / 2)
        if (title_col_start < self$plot_matrix_canvas_col_start) title_col_start <- self$plot_matrix_canvas_col_start
        title_col_end <- title_col_start + nchar_title - 1
        plot_matrix <- rbind(rep(" ", ncol(plot_matrix)), plot_matrix)
        plot_matrix[1, title_col_start:title_col_end] <- title_vec

        # update the canvas location
        self$plot_matrix_canvas_row_start <- self$plot_matrix_canvas_row_start + 1

        self$plot_matrix <- plot_matrix
      }
    },

    #' Add y-ticks label to the plot matrix
    #'
    #' @param n_ticks numeric, number of ticks
    add_y_ticks = function(n_ticks = 5) {
      if (n_ticks < 2) stop("n_ticks must be at least 2")
      plot_matrix <- self$plot_matrix
      y_min <- self$y_min
      y_max <- self$y_max

      offset <- 1

      # hence the function format_four_chars
      char_length <- 4

      # initialize matrix for the y ticks
      y_tick_matrix <- matrix(" ", nrow = nrow(plot_matrix), ncol = offset + char_length)

      # now we need want to place the y ticks at the appropriate places within our y tick matrix and space
      # them out evenly
      y_ticks <- seq(y_max, y_min, length.out = n_ticks)

      # Place the y ticks into the y tick matrix at the appropriate spots
      y_tick_positions <- round(seq(1, nrow(self$plot_canvas), length.out = n_ticks)) + self$plot_matrix_canvas_row_start - 1

      for (i in 1:length(y_ticks)) {
        y_tick_matrix[y_tick_positions[i], 1:(offset + char_length)] <- c(unlist(stringr::str_split(format_four_chars(y_ticks[i]), pattern = "")), rep(" ", times = offset))
      }

      # Combine the y tick matrix with the plot matrix
      plot_matrix <- cbind(y_tick_matrix, plot_matrix)

      # update the canvas location
      self$plot_matrix_canvas_col_start <- self$plot_matrix_canvas_col_start + offset + char_length

      self$plot_matrix <- plot_matrix
    },

    #' Add y-axis label to the plot matrix
    #'
    #' @description Add a y-axis label to the plot matrix
    #' @param y_label character, the y-axis label to be added
    add_y_label = function(y_label = self$y_label) {
      if(!is.null(y_label)) {
         plot_matrix <- self$plot_matrix
         y_min <- self$y_min
         y_max <- self$y_max

         offset <- 3

         # hence the function format_four_chars
         char_length <- nchar(y_label)

         # initialize matrix for the y label
         y_label_matrix <- matrix(" ", nrow = nrow(plot_matrix), ncol = offset + char_length)

         # Place the y label into the y label matrix at the appropriate spot
         y_label_position <- floor(nrow(self$plot_canvas) / 2) + self$plot_matrix_canvas_row_start - 1

         y_label_matrix[y_label_position, 1:(offset + char_length)] <- c(unlist(stringr::str_split(y_label, "")), rep(" ", times = offset))

         # Combine the y label matrix with the plot matrix
         plot_matrix <- cbind(y_label_matrix, plot_matrix)

         # update the canvas location
         self$plot_matrix_canvas_col_start <- self$plot_matrix_canvas_col_start + offset + char_length

         self$plot_matrix <- plot_matrix
      }
    },

    #' Add x-ticks label to the plot matrix
    #'
    #' @param n_ticks numeric, number of ticks
    add_x_ticks = function(n_ticks = 5) {
      if (n_ticks < 2) stop("n_ticks must be at least 2")
      plot_matrix <- self$plot_matrix

      x_min <- self$x_min
      x_max <- self$x_max

      offset <- 1

      # hence the function format_four_chars
      char_length <- 4

      # initialize matrix for the x ticks
      x_tick_matrix <- matrix(" ", nrow = 1 + offset, ncol = ncol(plot_matrix))

      if (self$is_boxplot) {
        x_ticks <- unlist(lapply(self$data, function(dat) dat$name))
        x_tick_positions <- unlist(lapply(self$data, function(dat) dat$x_position)) + self$plot_matrix_canvas_col_start - 1 - (nchar(x_ticks) %/% 2)
      } else {
        # now we need want to place the x ticks at the appropriate places within our x tick matrix and space
        # them out evenly
        x_ticks <- seq(x_min, x_max, length.out = n_ticks)

        # Place the x ticks into the x tick matrix at the appropriate spots
        x_tick_positions <- round(seq(1, ncol(self$plot_canvas), length.out = n_ticks), digits = 0) + self$plot_matrix_canvas_col_start - char_length

        # center the x ticks
        x_tick_positions[2:(length(x_ticks) - 1)] <- x_tick_positions[2:(length(x_ticks) - 1)] + floor(char_length / 2)

        # first element needs to start earlier
        x_tick_positions[1] <- x_tick_positions[1] + char_length - 1
      }

      for (i in 1:length(x_ticks)) {
        if (self$is_boxplot) {
          x_tick_matrix[1 + offset, x_tick_positions[i]:(x_tick_positions[i] + nchar(x_ticks[i]) - 1)] <- unlist(stringr::str_split(x_ticks[i], pattern = ""))
        } else {
          x_tick_matrix[1 + offset, x_tick_positions[i]:(x_tick_positions[i] + char_length - 1)] <- unlist(stringr::str_split(format_four_chars(x_ticks[i]), pattern = ""))
        }
      }


      # Combine the x tick matrix with the plot matrix
      plot_matrix <- rbind(plot_matrix, x_tick_matrix)

      self$plot_matrix <- plot_matrix
    },

    #' Add x-axis label to the plot matrix
    #'
    #' @description Add x-axis label to the plot matrix
    #' @param x_label x label
    add_x_label = function(x_label = self$x_label) {
      if(!is.null(self$x_label)) {
        plot_matrix <- self$plot_matrix
        x_label <- self$x_label

        offset <- 1

        # initialize matrix for the x label
        x_label_matrix <- matrix(" ", nrow = 1 + offset, ncol = ncol(plot_matrix))

        # Place the x label into the x label matrix at the center
        x_label_position <- floor(ncol(self$plot_canvas) / 2) - floor(nchar(x_label) / 2) + self$plot_matrix_canvas_col_start - 1

        x_label_matrix[1 + offset, x_label_position:(x_label_position + nchar(x_label) - 1)] <- unlist(stringr::str_split(x_label, pattern = ""))

        # Combine the x label matrix with the plot matrix
        plot_matrix <- rbind(plot_matrix, x_label_matrix)

        self$plot_matrix <- plot_matrix
      }
    },

    #' Add legend to the plot matrix
    #'
    #' @description Add legend to the plot matrix
    add_legend = function() {
      if (!self$draw_legend) return()
      plot_matrix <- self$plot_matrix

      legend_names <- unlist(lapply(self$data, function(dat) dat$name))
      n_legend <- length(legend_names)

      # Center the legend based on nrow of plot_matrix
      offset <- 3
      legend_start <- floor(nrow(self$plot_canvas) / 2) + self$plot_matrix_canvas_row_start - 1 - floor(n_legend / 2)
      legend_rows <- legend_start:(legend_start + n_legend - 1)

      # Initialize matrix for the legend
      legend_matrix <- matrix(" ", nrow = nrow(plot_matrix), ncol = n_legend + offset)

      # Place the legend names into the legend matrix at the appropriate spots
      for (i in 1:n_legend) {
        legend_matrix[legend_rows[i], (offset + 1)] <- make_colored(legend_names[i], self$data[[i]]$color)
      }

      # Combine the legend matrix with the plot matrix
      plot_matrix <- cbind(plot_matrix, legend_matrix)

      self$plot_matrix <- plot_matrix
    },

    #' Add data to the object.
    #'
    #' @param data list, list with elements: x, y, type, color, braille, name
    add_data = function(data) {
      # check if we have a boxplot
      if (
        (data$type != "boxplot" & self$is_boxplot) |
          (data$type == "boxplot" & length(self$data) > 0 & !self$is_boxplot)
      ) {
        stop("boxplots cannot be combined with other types at the moment")
      }

      # if there is no name, give it a name based on the order (like data_1
      if (is.null(data$name)) {
        data$name <- paste("data", length(self$data) + 1, sep = "_")
      } else {
        # Ensure unique names
        existing_names <- unlist(lapply(self$data, function(dat) dat$name))
        all_names <- c(existing_names, data$name)
        unique_names <- make_unique_names(all_names)
        data$name <- unique_names[length(unique_names)]
      }

      # add matrix_colored to the data
      data$matrix_colored <- list()
      if (is.null(data$braille)) data$braille <- getOption("plotcli.braille", TRUE)

      if (is.null(self$data)) {
        self$data <- list(data)
      } else {
        self$data <- append(self$data, list(data))
      }

      if (data$type == "boxplot") {
        self$is_boxplot <- TRUE
      }
    },

    #' Get minimum and maximum values for x and y
    #'
    #' @description Calculate the minimum and maximum values for x and y
    get_min_max = function() {
      cushion_percentage <- 0.00

      self$x_min <- min(unlist(lapply(self$data, function(dat) dat$x)))
      self$x_max <- max(unlist(lapply(self$data, function(dat) dat$x)))
      self$y_min <- min(unlist(lapply(self$data, function(dat) dat$y)))
      self$y_max <- max(unlist(lapply(self$data, function(dat) dat$y)))

      if (is.null(self$xlim)) {
        x_range <- self$x_max - self$x_min
        cushion_x <- x_range * cushion_percentage
        self$x_min <- self$x_min - cushion_x
        self$x_max <- self$x_max + cushion_x
      } else {
        self$x_min <- self$xlim[1]
        self$x_max <- self$xlim[2]
      }

      if (is.null(self$ylim)) {
        y_range <- self$y_max - self$y_min
        cushion_y <- y_range * cushion_percentage
        self$y_min <- self$y_min - cushion_y
        self$y_max <- self$y_max + cushion_y
      } else {
        self$y_min <- self$ylim[1]
        self$y_max <- self$ylim[2]
      }
    },

    #' Function to remove out of range data points if xlim and ylim were given
    #'
    #' @description Remove data points that are outside the specified xlim and ylim
    remove_out_of_range_data = function() {
      for (i in 1:length(self$data)) {
        in_x <- self$data[[i]]$x >= self$x_min & self$data[[i]]$x <= self$x_max
        in_y <- self$data[[i]]$y >= self$y_min & self$data[[i]]$y <= self$y_max
        in_both <- in_x & in_y

        self$data[[i]]$x <- self$data[[i]]$x[in_both]
        self$data[[i]]$y <- self$data[[i]]$y[in_both]
      }
    },

    #' Draw a scatter plot to the plot canvas.
    #'
    #' @description Draw a scatter plot of the specified data set on the plot canvas.
    #' @param set_idx numeric, the data element index to be drawn
    draw_scatter_plot = function(set_idx) {
      plot_canvas <- self$plot_canvas

      # Calculate the overall minimum and maximum values for all data sets
      x_min <- self$x_min
      x_max <- self$x_max
      y_min <- self$y_min
      y_max <- self$y_max

      x <- self$data[[set_idx]]$x
      y <- self$data[[set_idx]]$y

      braille <- self$data[[set_idx]]$braille

      if (braille) {
        # Calculate Braille resolution
        braille_width <- (ncol(plot_canvas)) * 2
        braille_height <- (nrow(plot_canvas)) * 4

        # Normalize x and y data points to fit within the Braille plot area
        x_norm <- round(normalize_data(x, x_min, x_max, braille_width))
        y_norm <- round(normalize_data(y, y_min, y_max, braille_height))

        # Draw data points using Braille characters
        for (i in 1:length(x)) {
          # Calculate Braille character row and column
          braille_row <- nrow(plot_canvas) - ceiling(y_norm[i] / 4) + 1
          braille_col <- x_norm[i] %/% 2 + 1

          if (braille_row < 1) braille_row <- 1
          if (braille_row > nrow(plot_canvas)) braille_row <- nrow(plot_canvas)

          if (braille_col < 1) braille_col <- 1
          if (braille_col > ncol(plot_canvas)) braille_col <- ncol(plot_canvas)

          # Calculate Braille dot position within the 2x4 grid
          dot_row <- 4 - (y_norm[i] - 1) %% 4
          dot_col <- 2 - (x_norm[i] - 1) %% 2

          # Calculate Braille dot index (1 to 8)
          dot_index <- (dot_col - 1) * 4 + (dot_row - 1) %/% 2 + 1

          # Calculate Braille character Unicode offset
          braille_offset <- 2^(dot_index)

          # Update the Braille character in the plot matrix
          current_char <- plot_canvas[braille_row, braille_col]

          if (!is_braille(current_char) | current_char == " ") {
            current_code <- 0x2800
          } else {
            current_code <- utf8ToInt(current_char)
          }

          # Check if the Braille dot is already set
          braille_dot_already_set <- (current_code & (2^(dot_index - 1))) != 0

          # new_code <- current_code + braille_offset
          # Check if the new code is within the Braille character range
          if (current_code + braille_offset <= 0x28FF) {
            new_code <- current_code + braille_offset
          } else {
            new_code <- current_code
          }

          plot_canvas[braille_row, braille_col] <- intToUtf8(new_code)

          # color tracking
          self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = braille_row, col = braille_col)
        }
      } else {
        # Normalize x and y data points to fit within the plot area
        x_norm <- round(normalize_data(x, x_min, x_max, ncol(plot_canvas) - 0))
        y_norm <- round(normalize_data(y, y_min, y_max, nrow(plot_canvas) - 0))

        # Draw data points using asterisk
        for (i in 1:length(x)) {
          ascii_row <- (nrow(plot_canvas)) - y_norm[i] + 1
          ascii_col <- x_norm[i] + 0
          self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = ascii_row, col = ascii_col)
          plot_canvas[ascii_row, ascii_col] <- "*"
        }
      }

      self$plot_canvas <- plot_canvas
    },

    #' Draw a line plot to the plot canvas.
    #'
    #' @param set_idx numeric, the data element index to be drawn
    draw_line_plot = function(set_idx) {
      plot_canvas <- self$plot_canvas

      x_min <- self$x_min
      x_max <- self$x_max
      y_min <- self$y_min
      y_max <- self$y_max

      x <- self$data[[set_idx]]$x
      y <- self$data[[set_idx]]$y

      braille <- self$data[[set_idx]]$braille

      if (braille) {
        braille_width <- (ncol(plot_canvas)) * 2
        braille_height <- (nrow(plot_canvas)) * 4

        x_norm <- round(normalize_data(x, x_min, x_max, braille_width))
        y_norm <- round(normalize_data(y, y_min, y_max, braille_height))
      } else {
        x_norm <- round(normalize_data(x, x_min, x_max, ncol(plot_canvas) - 0))
        y_norm <- round(normalize_data(y, y_min, y_max, nrow(plot_canvas) - 0))
      }

      for (i in 1:(length(x) - 1)) {
        if (braille) {
          points <- bresenham(
            x_norm[i] %/% 2, nrow(plot_canvas) - ceiling(y_norm[i] / 4) - 0,
            x_norm[i + 1] %/% 2, nrow(plot_canvas) - ceiling(y_norm[i + 1] / 4) - 0
          )
          points <- lapply(points, function(x) {
            x + c(0, 1)
          })
        } else {
          points <- bresenham(
            x_norm[i], (nrow(plot_canvas)) - y_norm[i],
            x_norm[i + 1], (nrow(plot_canvas)) - y_norm[i + 1]
          )
          points <- lapply(points, function(x) {
            x + c(0, 1)
          })
        }

        for (point in points) {
          if (braille) {
            if (point[2] < 1) point[2] <- 1
            if (point[2] > nrow(plot_canvas)) point[2] <- nrow(plot_canvas)

            if (point[1] < 1) point[1] <- 1
            if (point[2] > ncol(plot_canvas)) point[2] <- ncol(plot_canvas)

            dot_row <- 4 - (y_norm[i] - 1) %% 4
            dot_col <- 2 - (x_norm[i] - 1) %% 2
            dot_index <- (dot_col - 1) * 4 + (dot_row - 1) %/% 2 + 1
            braille_offset <- 2^(dot_index - 1)
            current_char <- plot_canvas[point[2], point[1]]

            if (!is_braille(current_char) | current_char == " ") {
              current_code <- 0x2800
            } else {
              current_code <- utf8ToInt(current_char)
            }

            if (current_code + braille_offset <= 0x28FF) {
              new_code <- current_code + braille_offset
            } else {
              new_code <- current_code
            }

            plot_canvas[point[2], point[1]] <- intToUtf8(new_code)

            # color tracking
            self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = point[2], col = point[1])
          } else {
            # color tracking
            self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = point[2], col = point[1])
            plot_canvas[point[2], point[1]] <- "*"
          }
        }
      }

      self$plot_canvas <- plot_canvas
    },

    #' Draw a barplot to the plot canvas.
    #'
    #' @param set_idx numeric, the data element index to be drawn
    draw_barplot = function(set_idx) {
      plot_canvas <- self$plot_canvas

      x_min <- self$x_min
      x_max <- self$x_max
      y_min <- self$y_min
      y_max <- self$y_max

      x <- self$data[[set_idx]]$x
      y <- self$data[[set_idx]]$y

      # Normalize x and y data points to fit within the plot area
      x_norm <- round(normalize_data(x, x_min, x_max, ncol(plot_canvas)))
      y_norm <- round(normalize_data(y, y_min, y_max, nrow(plot_canvas)))

      for (i in 1:length(x)) {
        for (j in 1:y_norm[i]) {
          plot_canvas[nrow(plot_canvas) - j + 1, x_norm[i]] <- full_block_char
          self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- 
            list(row = nrow(plot_canvas) - j + 1, col = x_norm[i])
        }
      }

      self$plot_canvas <- plot_canvas
    },

#' Draw a barplot to the plot canvas with braille characters.
#'
#' @param set_idx numeric, the data element index to be drawn
draw_barplot_braille = function(set_idx) {
  plot_canvas <- self$plot_canvas

  x_min <- self$x_min
  x_max <- self$x_max
  y_min <- self$y_min
  y_max <- self$y_max

  x <- self$data[[set_idx]]$x
  y <- self$data[[set_idx]]$y

  # Normalize x and y data points to fit within the plot area
  x_norm <- round(normalize_data(x, x_min, x_max, ncol(plot_canvas)))
  y_norm <- round(normalize_data(y, y_min, y_max, nrow(plot_canvas) * 4)) # Multiply by 4 for Braille resolution

  for (i in 1:length(x)) {
    for (j in 1:y_norm[i]) {
      braille_row <- nrow(plot_canvas) - ceiling(j / 4) + 1
      braille_col <- x_norm[i]

      if (braille_row < 1) braille_row <- 1
      if (braille_row > nrow(plot_canvas)) braille_row <- nrow(plot_canvas)

      dot_row <- 4 - (j - 1) %% 4
      dot_col <- 2 - (j - 1) %% 2
      dot_index <- (dot_col - 1) * 4 + (dot_row - 1) %/% 2 + 1
      braille_offset <- 2^(dot_index - 1)
      current_char <- plot_canvas[braille_row, braille_col]

      if (!is_braille(current_char) | current_char == " ") {
        current_code <- 0x2800
      } else {
        current_code <- utf8ToInt(current_char)
      }

      if (current_code + braille_offset <= 0x28FF) {
        new_code <- current_code + braille_offset
      } else {
        new_code <- current_code
      }

      plot_canvas[braille_row, braille_col] <- intToUtf8(new_code)
      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = braille_row, col = braille_col)
    }
  }

  self$plot_canvas <- plot_canvas
},



    #' Draw a boxplot to the plot canvas.
    #'
    #' @param set_idx numeric, the data element index to be drawn
    draw_boxplot = function(set_idx) {
      plot_canvas <- self$plot_canvas

      # x_center <- median(1:ncol(plot_canvas))
      x_center <- self$data[[set_idx]]$x_position
      y <- self$data[[set_idx]]$y

      y_min <- self$y_min
      y_max <- self$y_max

      # Calculate box plot statistics on the original y values
      box_stats <- boxplot.stats(y)

      # Normalize the box plot statistics to fit within the plot area
      stats_norm <- round(normalize_data(box_stats$stats, y_min, y_max, nrow(plot_canvas)))

      # Draw the box plot using the normalized statistics
      box_top <- nrow(plot_canvas) - stats_norm[5] + 1
      box_bottom <- nrow(plot_canvas) - stats_norm[1] + 1
      box_left <- x_center - 4 # Adjust the left position to create a wider box
      box_right <- x_center + 4 # Adjust the right position to create a wider box

      # Draw vertical lines
      plot_canvas[box_top:(stats_norm[5] - stats_norm[4] + box_top), x_center] <- box_vert_char
      plot_canvas[box_bottom:(box_bottom - (stats_norm[2] - stats_norm[1])), x_center] <- box_vert_char

      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <-
        list(row = box_bottom:(box_bottom - (stats_norm[2] - stats_norm[1])), col = x_center)
      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <-
        list(row = box_top:(stats_norm[5] - stats_norm[4] + box_top), col = x_center)


      # Draw horizontal lines
      for (i in 1:5) {
        plot_canvas[nrow(plot_canvas) - stats_norm[i] + 1, (box_left + 1):(box_right - 1)] <- box_horiz_char
        # Add colored matrix elements for horizontal lines
        for (col_idx in (box_left + 1):(box_right - 1)) {
          self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - stats_norm[i] + 1, col = col_idx)
        }
      }

      # Draw corners
      plot_canvas[nrow(plot_canvas) - stats_norm[2] + 1, box_left] <- box_top_left_corner_char
      plot_canvas[nrow(plot_canvas) - stats_norm[2] + 1, box_right] <- box_top_right_corner_char
      plot_canvas[nrow(plot_canvas) - stats_norm[4] + 1, box_left] <- box_bottom_left_corner_char
      plot_canvas[nrow(plot_canvas) - stats_norm[4] + 1, box_right] <- box_bottom_right_corner_char

      plot_canvas[nrow(plot_canvas) - stats_norm[2] + 1, box_left] <- box_bottom_left_corner_char
      plot_canvas[nrow(plot_canvas) - stats_norm[2] + 1, box_right] <- box_bottom_right_corner_char
      plot_canvas[nrow(plot_canvas) - stats_norm[4] + 1, box_left] <- box_top_left_corner_char
      plot_canvas[nrow(plot_canvas) - stats_norm[4] + 1, box_right] <- box_top_right_corner_char

      # Add colored matrix elements for corners
      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - stats_norm[2] + 1, col = box_left)
      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - stats_norm[2] + 1, col = box_right)
      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - stats_norm[4] + 1, col = box_left)
      self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - stats_norm[4] + 1, col = box_right)

      for (i in (stats_norm[4] - 2):(stats_norm[2])) {
        plot_canvas[nrow(plot_canvas) - i + 0, box_left] <- box_vert_char
        plot_canvas[nrow(plot_canvas) - i + 0, box_right] <- box_vert_char
        # Add colored matrix elements for vertical lines
        self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - i + 0, col = box_left)
        self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = nrow(plot_canvas) - i + 0, col = box_right)
      }

      # Draw outliers
      outliers <- box_stats$out
      if (length(outliers) > 0) {
        outliers_norm <- round(normalize_data(outliers, y_min, y_max, nrow(plot_canvas)))
        for (i in 1:length(outliers)) {
          outlier_row <- nrow(plot_canvas) - outliers_norm[i] + 1
          plot_canvas[outlier_row, x_center] <- "*"
          self$data[[set_idx]]$matrix_colored[[length(self$data[[set_idx]]$matrix_colored) + 1]] <- list(row = outlier_row, col = x_center)
        }
      }

      # Update the plot canvas
      self$plot_canvas <- plot_canvas
    },

    #' Draw colors to the canvas
    #'
    #' @description In the draw_ functions we have been keeping track of the locations of the colored matrix elements.
    #' These are now being colored.
    draw_colors = function() {
      plot_canvas <- self$plot_canvas
      # get the colors in
      for (set_idx in 1:length(self$data)) {
        for (i in 1:length(self$data[[set_idx]]$matrix_colored)) {
          this_row <- self$data[[set_idx]]$matrix_colored[[i]]$row
          this_col <- self$data[[set_idx]]$matrix_colored[[i]]$col
          plot_canvas[this_row, this_col] <- make_colored(plot_canvas[this_row, this_col], self$data[[set_idx]]$color)
        }
      }

      self$plot_canvas <- plot_canvas
    },

    #' Draw the different plots types from all data elements to the canvas
    #'
    #' @description This function iterates through all data elements and calls the appropriate draw_ function
    #' based on the plot type (scatter, line, boxplot, or barplot).
    draw_plot = function() {
      for (set_idx in 1:length(self$data)) {
        if (self$data[[set_idx]]$type == "scatter") {
          self$draw_scatter_plot(set_idx)
        } else if (self$data[[set_idx]]$type == "line") {
          self$draw_line_plot(set_idx)
        } else if (self$data[[set_idx]]$type == "boxplot") {
          self$draw_boxplot(set_idx)
        } else if (self$data[[set_idx]]$type == "barplot") {

          if(self$data[[set_idx]]$braille) {
            self$draw_barplot_braille(set_idx)
          } else {
            self$draw_barplot(set_idx)
          }
        }
      }
    },

    #' Make plot matrix: assembles all plot elements (canvas + borders + title + axes + legend)
    #'
    #' @description This function assembles all plot elements (canvas + borders + title + axes + legend)
    #' and creates the final plot matrix.
    make_plot_matrix = function() {
      self$get_min_max()
      self$remove_out_of_range_data()

      # Calculate x-axis positions for boxplots
      boxplot_count <- sum(sapply(self$data, function(dat) dat$type == "boxplot"))
      if (boxplot_count > 0) {
        boxplot_positions <- seq(from = 1, to = ncol(self$plot_canvas), length.out = boxplot_count + 1)
        boxplot_positions <- round(boxplot_positions[-1] - diff(boxplot_positions) / 2)
        boxplot_idx <- 1
        for (set_idx in 1:length(self$data)) {
          if (self$data[[set_idx]]$type == "boxplot") {
            self$data[[set_idx]]$x_position <- boxplot_positions[boxplot_idx]
            boxplot_idx <- boxplot_idx + 1
          }
        }
      }

      self$draw_plot()
      self$draw_colors()

      x_min <- self$x_min
      x_max <- self$x_max
      y_min <- self$y_min
      y_max <- self$y_max

      # Call the functions to add borders, title, x/y labels, and ticks, and legend
      self$initialize_plot_matrix()
      self$add_borders()
      # NOTE: this function is for debugging: prints row and column index around the canvas
      # self$add_row_col_index()
      self$add_title()
      self$add_y_ticks()
      self$add_y_label()
      self$add_x_ticks()
      self$add_x_label()
      if(self$draw_legend) self$add_legend()
      self$add_col()
      self$add_col()
      self$add_row()
      self$add_row(bottom = TRUE)
    },

    #' Export plot matrix
    #'
    #' @description This function exports the plot matrix.
    #' @return The plot matrix.
    export_plot_matrix = function() {
      self$make_plot_matrix()
      return(self$plot_matrix)
    },

    #' Main plotting function: assembles all plot elements (canvas + borders + title + axes + legend) and prints the plot
    #' by 'cat'ing the plot matrix to the console.
    #'
    #' @description This function assembles all plot elements (canvas + borders + title + axes + legend) and
    #' prints the final plot by 'cat'ing the plot matrix to the console.
    print_plot = function() {
      self$make_plot_matrix()
      cat_plot_matrix(self$plot_matrix)
    },

    #' Merge two plotcli objects
    #'
    #' This method combines the data from two plotcli objects into a single plotcli object.
    #' It takes the maximum of the plot_width and plot_height, combines the titles, and sets
    #' the xlim and ylim to the minimum and maximum values of both objects.
    #'
    #' @param other A plotcli object to be merged with the current object.
    #'
    #' @return A new plotcli object containing the combined data from both objects.
    #'
    merge = function(other) {
      if (!inherits(other, "plotcli")) {
        stop("Can only merge with another plotcli object")
      }
    
      # Combine data from both objects
      combined_data <- c(self$data, other$data)
    
      # Create a new plotcli object with combined data
      new_plotcli <- plotcli$new(
        plot_width = max(self$plot_width, other$plot_width),
        plot_height = max(self$plot_height, other$plot_height),
        x_label = self$x_label,
        y_label = self$y_label,
        ylim = if (!is.null(self$ylim) && !is.null(other$ylim)) c(min(self$ylim[1], other$ylim[1]), max(self$ylim[2], other$ylim[2])) else NULL,
        xlim = if (!is.null(self$xlim) && !is.null(other$xlim)) c(min(self$xlim[1], other$xlim[1]), max(self$xlim[2], other$xlim[2])) else NULL,
        title = if (!is.null(self$title) && !is.null(other$title)) paste(self$title, other$title, sep = " & ") else NULL,
        draw_legend = TRUE
      )
    
      # Add combined data to the new plotcli object
      for (data in combined_data) {
        new_plotcli$add_data(data)
      }
    
      return(new_plotcli)
    }
  )
)