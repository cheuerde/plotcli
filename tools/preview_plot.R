#' Preview plotcli output in browser with proper ANSI color rendering
#'
#' This utility creates an HTML file that properly renders terminal output
#' with ANSI color codes, Unicode characters, and Braille patterns.
#'
#' @param plot A plotcli object or character vector of terminal output
#' @param file Output HTML file path (default: tempfile)
#' @param open Whether to open in browser (default: TRUE)
#' @return The path to the HTML file (invisibly)
#' @export
preview_plot <- function(plot, file = NULL, open = TRUE) {
  

  # Capture the plot output
  if (inherits(plot, "plotcli")) {
    output <- capture.output(plot$print_plot())
  } else if (is.character(plot))

{
    output <- plot
  } else if (is.matrix(plot)) {
    # Handle raw plot matrix
    output <- apply(plot, 1, paste, collapse = "")
  } else {
    stop("Input must be a plotcli object, character vector, or matrix")
  }
  
  # Collapse to single string with newlines
  raw_output <- paste(output, collapse = "\n")
  
  # Convert ANSI codes to HTML spans
  html_output <- ansi_to_html(raw_output)
  
  # Create HTML document
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>plotcli Preview</title>
  <style>
    body {
      background-color: #1e1e1e;
      padding: 20px;
      font-family: "Fira Code", "JetBrains Mono", "SF Mono", "Monaco", "Inconsolata", "Roboto Mono", monospace;
    }
    .terminal {
      background-color: #0d1117;
      color: #c9d1d9;
      padding: 20px;
      border-radius: 8px;
      font-size: 14px;
      line-height: 1.4;
      white-space: pre;
      overflow-x: auto;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
    }
    /* ANSI color classes */
    .ansi-black { color: #000000; }
    .ansi-red { color: #ff6b6b; }
    .ansi-green { color: #69db7c; }
    .ansi-yellow { color: #ffd43b; }
    .ansi-blue { color: #74c0fc; }
    .ansi-magenta { color: #da77f2; }
    .ansi-cyan { color: #66d9e8; }
    .ansi-white { color: #ffffff; }
    .ansi-silver { color: #adb5bd; }
    .ansi-bright-black { color: #495057; }
    .ansi-bright-red { color: #ff8787; }
    .ansi-bright-green { color: #8ce99a; }
    .ansi-bright-yellow { color: #ffe066; }
    .ansi-bright-blue { color: #a5d8ff; }
    .ansi-bright-magenta { color: #e599f7; }
    .ansi-bright-cyan { color: #99e9f2; }
    .ansi-bright-white { color: #f8f9fa; }
    .ansi-bold { font-weight: bold; }
    .ansi-dim { opacity: 0.7; }
    .ansi-italic { font-style: italic; }
    .ansi-underline { text-decoration: underline; }
    
    h1 {
      color: #c9d1d9;
      font-weight: 300;
      margin-bottom: 20px;
    }
    .info {
      color: #8b949e;
      font-size: 12px;
      margin-top: 15px;
    }
  </style>
</head>
<body>
  <h1>plotcli Preview</h1>
  <div class="terminal">%s</div>
  <div class="info">
    Generated: %s | 
    Characters: %d | 
    Lines: %d
  </div>
</body>
</html>
', html_output, Sys.time(), nchar(raw_output), length(output))
  
  # Write to file
 if (is.null(file)) {
    file <- tempfile(fileext = ".html")
  }
  writeLines(html_content, file)
  
  # Open in browser
  if (open) {
    browseURL(file)
  }
  
  invisible(file)
}


#' Convert ANSI escape codes to HTML spans
#'
#' @param text Character string with ANSI codes
#' @return Character string with HTML spans
ansi_to_html <- function(text) {
  # Escape HTML special characters first
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  
  # ANSI color code mapping (foreground colors)
  color_map <- list(
    "30" = "ansi-black",
    "31" = "ansi-red",
    "32" = "ansi-green",
    "33" = "ansi-yellow",
    "34" = "ansi-blue",
    "35" = "ansi-magenta",
    "36" = "ansi-cyan",
    "37" = "ansi-white",
    "90" = "ansi-bright-black",
    "91" = "ansi-bright-red",
    "92" = "ansi-bright-green",
    "93" = "ansi-bright-yellow",
    "94" = "ansi-bright-blue",
    "95" = "ansi-bright-magenta",
    "96" = "ansi-bright-cyan",
    "97" = "ansi-bright-white"
  )
  
  style_map <- list(
    "1" = "ansi-bold",
    "2" = "ansi-dim",
    "3" = "ansi-italic",
    "4" = "ansi-underline"
  )
  
  result <- text
  
  # Replace reset codes with closing spans
  # Pattern: ESC[0m, ESC[m, ESC[39m (default foreground), ESC[49m (default background)
  result <- gsub("\033\\[(0|39|49)?m", "</span>", result, perl = TRUE)
  
  # Replace color codes with opening spans
  # Pattern: ESC[XXm where XX is the color code
  for (code in names(color_map)) {
    pattern <- sprintf("\033\\[%sm", code)
    replacement <- sprintf('<span class="%s">', color_map[[code]])
    result <- gsub(pattern, replacement, result, perl = TRUE)
  }
  
  # Replace style codes
  for (code in names(style_map)) {
    pattern <- sprintf("\033\\[%sm", code)
    replacement <- sprintf('<span class="%s">', style_map[[code]])
    result <- gsub(pattern, replacement, result, perl = TRUE)
  }
  
  # Handle combined codes like ESC[1;31m (bold red)
  result <- gsub("\033\\[1;(\\d+)m", '<span class="ansi-bold \\1">', result, perl = TRUE)
  
  # Clean up any remaining escape codes
  result <- gsub("\033\\[[0-9;]*m", "", result, perl = TRUE)
  
  return(result)
}


#' Generate a test suite of plots for visual verification
#'
#' @param output_dir Directory to save HTML files
#' @return List of generated file paths
#' @export
generate_test_plots <- function(output_dir = "tools/test_output") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  files <- list()
  
  # Test 1: Basic scatter plot
  message("Generating: scatter plot...")
  p1 <- plotcli_scatter(
    x = iris$Sepal.Width, 
    y = iris$Sepal.Length, 
    color = "blue",
    braille = TRUE,
    x_label = "Sepal Width",
    y_label = "Sepal Length"
  )
  files$scatter <- preview_plot(p1, file.path(output_dir, "01_scatter.html"), open = FALSE)
  
  # Test 2: Multi-series line plot
  message("Generating: line plot...")
  x <- seq(0, 2 * pi, length.out = 100)
  p2 <- plotcli$new(plot_width = 80, plot_height = 30, x_label = "x", y_label = "y")
  p2$add_data(list(x = x, y = sin(x), name = "sin(x)", color = "red", type = "line", braille = TRUE))
  p2$add_data(list(x = x, y = cos(x), name = "cos(x)", color = "blue", type = "line", braille = TRUE))
  files$line <- preview_plot(p2, file.path(output_dir, "02_line.html"), open = FALSE)
  
  # Test 3: Histogram
  message("Generating: histogram...")
  p3 <- plotcli_histogram(rnorm(1000), color = "green", braille = TRUE)
  files$histogram <- preview_plot(p3, file.path(output_dir, "03_histogram.html"), open = FALSE)
  
  # Test 4: Density plot
  message("Generating: density plot...")
  p4 <- plotcli_density(rnorm(1000), color = "magenta", braille = TRUE)
  files$density <- preview_plot(p4, file.path(output_dir, "04_density.html"), open = FALSE)
  
  # Test 5: Box plot
  message("Generating: box plot...")
  p5 <- plotcli_box(iris$Sepal.Length, color = "cyan")
  files$boxplot <- preview_plot(p5, file.path(output_dir, "05_boxplot.html"), open = FALSE)
  
  # Test 6: ggplotcli conversion
  message("Generating: ggplotcli conversion...")
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    tryCatch({
      gg <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) + 
        geom_point() +
        labs(title = "MPG vs Weight by Cylinders", x = "Miles per Gallon", y = "Weight")
      p6 <- ggplotcli(gg, braille = TRUE)
      files$ggplotcli <- preview_plot(p6, file.path(output_dir, "06_ggplotcli.html"), open = FALSE)
    }, error = function(e) {
      message("  Skipping ggplotcli test due to error: ", e$message)
    })
  }
  
  # Test 7: ASCII mode (no braille)
  message("Generating: ASCII mode...")
  p7 <- plotcli_scatter(
    x = 1:20, 
    y = (1:20)^2, 
    color = "yellow",
    braille = FALSE,
    x_label = "x",
    y_label = "x²"
  )
  files$ascii <- preview_plot(p7, file.path(output_dir, "07_ascii.html"), open = FALSE)
  
  # Generate index page
  message("Generating: index page...")
  index_html <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>plotcli Test Suite</title>
  <style>
    body {
      background-color: #1e1e1e;
      color: #c9d1d9;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
      padding: 40px;
      max-width: 800px;
      margin: 0 auto;
    }
    h1 { color: #58a6ff; }
    ul { list-style: none; padding: 0; }
    li { margin: 10px 0; }
    a {
      color: #58a6ff;
      text-decoration: none;
      padding: 10px 15px;
      background: #21262d;
      border-radius: 6px;
      display: inline-block;
    }
    a:hover { background: #30363d; }
    .timestamp { color: #8b949e; font-size: 14px; }
  </style>
</head>
<body>
  <h1>plotcli Test Suite</h1>
  <p class="timestamp">Generated: %s</p>
  <ul>
    <li><a href="01_scatter.html">1. Scatter Plot (Braille)</a></li>
    <li><a href="02_line.html">2. Multi-series Line Plot</a></li>
    <li><a href="03_histogram.html">3. Histogram</a></li>
    <li><a href="04_density.html">4. Density Plot</a></li>
    <li><a href="05_boxplot.html">5. Box Plot</a></li>
    <li><a href="06_ggplotcli.html">6. ggplotcli Conversion</a></li>
    <li><a href="07_ascii.html">7. ASCII Mode (no Braille)</a></li>
  </ul>
</body>
</html>
', Sys.time())
  
  index_file <- file.path(output_dir, "index.html")
  writeLines(index_html, index_file)
  files$index <- index_file
  
  message(sprintf("\nTest suite generated in: %s", output_dir))
  message(sprintf("Open %s to view all tests", index_file))
  
  return(files)
}

