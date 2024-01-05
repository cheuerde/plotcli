## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(plotcli)
# Generate sample data
sample_data <- rnorm(1000, mean = 5, sd = 2)

# Create a density plot
plotcli_density(sample_data, color = "blue", braille = TRUE)

## -----------------------------------------------------------------------------
# Generate sample data
sample_data <- rnorm(1000, mean = 5, sd = 2)

# Create a histogram
plotcli_histogram(sample_data, color = "yellow")

## -----------------------------------------------------------------------------
# Create a scatter plot
plotcli_scatter(x = iris$`Sepal.Width`, y = iris$`Sepal.Length`, color = "magenta", braille = FALSE)

## -----------------------------------------------------------------------------

# make sin 
x <- seq(0, 2*pi, length.out = 50)
y <- sin(x)

# Create a line plot
plotcli_line(x = x, y = cos(y), color = "green")
plotcli_line(x = x, y = x, color = "magenta", braille = FALSE)

