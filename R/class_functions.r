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


#' Overload the "+" operator for plotcli objects
#'
#' This function overloads the "+" operator to merge two plotcli objects.
#'
#' @param plot1 A plotcli object to be merged.
#' @param plot2 A plotcli object to be merged.
#' @return A new plotcli object containing the combined data from both objects.
#' @export
`+.plotcli` <- function(plot1, plot2) {
  return(plot1$merge(plot2))
}

#' Generic function for combining plotcli objects horizontally
#'
#' @param ... plotcli objects to be combined.
#' @param deparse.level The deparsing level for the arguments.
#' @return A combined plot matrix.
#' @export
cbind.plotcli <- function(..., deparse.level = 1) {
  plots <- list(...)
  if (length(plots) != 2) {
    stop("cbind.plotcli only supports combining exactly two plotcli objects.")
  }
  plot1 <- plots[[1]]
  plot2 <- plots[[2]]
  combined_matrix <- cbind_plots(plot1, plot2)
  return(combined_matrix)
}

#' Generic function for combining plotcli objects vertically
#'
#' @param ... plotcli objects to be combined.
#' @param deparse.level The deparsing level for the arguments.
#' @return A combined plot matrix.
#' @export
rbind.plotcli <- function(..., deparse.level = 1) {
  plots <- list(...)
  if (length(plots) != 2) {
    stop("rbind.plotcli only supports combining exactly two plotcli objects.")
  }
  plot1 <- plots[[1]]
  plot2 <- plots[[2]]
  combined_matrix <- rbind_plots(plot1, plot2)
  return(combined_matrix)
}