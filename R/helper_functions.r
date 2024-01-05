#' Print plot matrix
#'
#' This function prints a plot matrix to the console.
#'
#' @param plot_matrix The plot matrix to be printed.
#' @export
#' 
#' @examples
#' cat_plot_matrix(matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2))
cat_plot_matrix <- function(plot_matrix) {
    cat("\n")
    for (i in 1:nrow(plot_matrix)) {
        cat(paste(plot_matrix[i, ], collapse = ""), "\n")
    }
}

#' Combine plot matrices horizontally
#'
#' This function combines multiple plot matrices horizontally, centering them vertically.
#'
#' @param ... A list of plot matrices to be combined.
#' @return A combined plot matrix.
#' @export
#' 
cbind_plots = function(...) {
  plots <- list(...)
  if (any(sapply(plots, function(plot) !inherits(plot, "plotcli")))) {
    stop("All input objects must be of class 'plotcli'")
  }
  max_rows <- max(sapply(plots, function(plot) nrow(plot$plot_matrix)))

  # Initialize the combined plot matrix
  combined_matrix <- matrix(" ", nrow = max_rows, ncol = 0)

  for (i in seq_along(plots)) {
    plot <- plots[[i]]
    # Center the plot matrix vertically
    row_offset <- floor((max_rows - nrow(plot$plot_matrix)) / 2)
    centered_matrix <- matrix(" ", nrow = max_rows, ncol = ncol(plot$plot_matrix))
    centered_matrix[(1 + row_offset):(nrow(plot$plot_matrix) + row_offset), ] <- plot$plot_matrix

    # Remove the last endline in the first plot_matrix
    if (i == 1) {
      centered_matrix <- centered_matrix[, -ncol(centered_matrix)]
    }

    # Combine the centered plot matrix with the combined matrix
    combined_matrix <- cbind(combined_matrix, centered_matrix)
  }

  return(combined_matrix)
}

#' Combine plot matrices vertically
#'
#' This function combines multiple plot matrices vertically, centering them horizontally.
#'
#' @param ... A list of plot matrices to be combined.
#' @return A combined plot matrix.
#' @export
#' 
rbind_plots = function(...) {
  plots <- list(...)
  if (any(sapply(plots, function(plot) !inherits(plot, "plotcli")))) {
    stop("All input objects must be of class 'plotcli'")
  }
  max_cols <- max(sapply(plots, function(plot) ncol(plot$plot_matrix)))

  # Initialize the combined plot matrix
  combined_matrix <- matrix(" ", nrow = 0, ncol = max_cols)

  for (plot in plots) {
    # Center the plot matrix horizontally
    col_offset <- floor((max_cols - ncol(plot$plot_matrix)) / 2)
    centered_matrix <- matrix(" ", nrow = nrow(plot$plot_matrix), ncol = max_cols)
    centered_matrix[, (1 + col_offset):(ncol(plot$plot_matrix) + col_offset)] <- plot$plot_matrix

    # Combine the centered plot matrix with the combined matrix
    combined_matrix <- rbind(combined_matrix, centered_matrix)
  }

  return(combined_matrix)
}

#' Make colored text
#'
#' This function applies a specified color to a given text string.
#'
#' @param x The text string to be colored.
#' @param color The color to be applied to the text. If NULL, the color codes will be removed.
#' @return A colored text string or a text string with color codes removed.
#' @export
#' 
#' @examples
#' make_colored("Hello, world!", "blue")
#' make_colored("Hello, world!", NULL)
make_colored <- function(x,
                         color = NULL) {
    if (is.null(color)) {
        out <- remove_color_codes(x)
    } else {
        color_function <- get(color, mode = "function", envir = asNamespace("crayon"))
        out <- color_function(remove_color_codes(x))
    }
    return(out)
}

#' Bresenham's line algorithm
#'
#' This function generates a list of points that form a line between two given points using Bresenham's line algorithm.
#'
#' @param x0 The x-coordinate of the starting point.
#' @param y0 The y-coordinate of the starting point.
#' @param x1 The x-coordinate of the ending point.
#' @param y1 The y-coordinate of the ending point.
#' @return A list of points that form a line between the two given points.
#' @export
#' 
#' @examples
#' bresenham(0, 0, 5, 5)
#' bresenham(0, 0, -5, -5)
bresenham <- function(x0,
                      y0,
                      x1,
                      y1) {
    points <- list()
    dx <- abs(x1 - x0)
    dy <- abs(y1 - y0)
    sx <- ifelse(x0 < x1, 1, -1)
    sy <- ifelse(y0 < y1, 1, -1)
    err <- dx - dy

    while (TRUE) {
        points <- append(points, list(c(x0, y0)))

        if (x0 == x1 && y0 == y1) {
            break
        }

        e2 <- 2 * err
        if (e2 > -dy) {
            err <- err - dy
            x0 <- x0 + sx
        }
        if (e2 < dx) {
            err <- err + dx
            y0 <- y0 + sy
        }
    }

    return(points)
}

#' Get terminal colors
#'
#' This function returns a vector of terminal colors.
#'
#' @param n The number of colors to return.
#' @return A vector of terminal colors.
#' @export
#' 
#' @examples
#' get_term_colors(5)
#' get_term_colors(10)
get_term_colors <- function(n = NULL) {
    term_colors <- c(
        "blue",
        "red",
        "green",
        "yellow",
        "magenta",
        # "grey",
        "cyan",
        "silver"
    )

    if (is.null(n)) n <- length(term_colors)

    if (n > length(term_colors)) term_colors <- rep(term_colors, times = ceiling(n / length(term_colors)))

    return(term_colors[1:n])
}

#' Normalize data
#'
#' This function normalizes the given data to a specified plot range.
#'
#' @param data The data to be normalized.
#' @param data_min The minimum value of the data.
#' @param data_max The maximum value of the data.
#' @param plot_range The range to normalize the data to.
#' @return The normalized data.
#' @export
#' 
#' @examples
#' normalize_data(c(1, 2, 3, 4, 5), 1, 5, 10)
#' normalize_data(c(10, 20, 30, 40, 50), 10, 50, 100)
normalize_data <- function(data,
                           data_min,
                           data_max,
                           plot_range) {
    val <- ((data - data_min) / (data_max - data_min) * (plot_range)) + 1
    val[val < 1] <- 1
    val[val > plot_range] <- plot_range
    return(val)
}

#' Format number to four characters
#'
#' This function formats a number to a string of exactly four characters.
#'
#' @param num The number to be formatted.
#' @return A string representation of the number with exactly four characters.
#' @export
#' 
#' @examples
#' format_four_chars(123)
#' format_four_chars(-12.34)
format_four_chars <- function(num) {
    if (num < 0) {
        # Negative numbers
        if (num <= -100) {
            formatted_num <- sprintf("%0.0e", num) # Use scientific notation
        } else {
            formatted_num <- sprintf("%.1f", num) # One decimal place
        }
    } else {
        # Positive numbers
        if (num < 100) {
            formatted_num <- sprintf("%.1f", num) # One decimal place
        } else {
            formatted_num <- sprintf("%.0f", num) # No decimal places
        }
    }

    # Ensure the string is exactly 4 characters long
    if (nchar(formatted_num) < 4) {
        formatted_num <- stringr::str_pad(formatted_num, width = 4, side = "left", pad = " ")
    } else if (nchar(formatted_num) > 4) {
        formatted_num <- substr(formatted_num, 1, 4)
    }

    return(formatted_num)
}

#' Remove color codes from a string
#'
#' This function removes ANSI color codes from a given text string.
#'
#' @param s The text string containing ANSI color codes.
#' @return A text string with ANSI color codes removed.
#' @export
#' 
#' @examples
#' colored_text <- make_colored("Hello, world!", "blue")
#' remove_color_codes(colored_text)
remove_color_codes <- function(s) {
    # ANSI color code pattern
    ansi_pattern <- "\033\\[\\d+(;\\d+)?m"
    # Remove the ANSI color codes using gsub
    gsub(ansi_pattern, "", s, perl = TRUE)
}

#' Check if a character is a Braille character
#'
#' This function checks if a given character is a Braille character.
#'
#' @param char The character to be checked.
#' @return A boolean value indicating whether the character is a Braille character or not.
#' @export
#' 
#' @examples
#' is_braille("⠋")
#' is_braille("A")
is_braille <- function(char) {
    code <- utf8ToInt(char)
    return((code >= 0x2800 && code <= 0x28FF))
}