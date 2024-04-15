library(testthat)
library(plotcli)

test_that("plotcli_density works", {
  sample_data <- rnorm(1000, mean = 5, sd = 2)
  expect_silent(plotcli_density(sample_data, color = "blue", braille = TRUE))
})

test_that("plotcli_histogram works", {
  sample_data <- rnorm(1000, mean = 5, sd = 2)
  expect_silent(plotcli_histogram(sample_data, color = "yellow"))
})

test_that("plotcli_scatter works", {
  expect_silent(plotcli_scatter(x = iris$Sepal.Width, y = iris$Sepal.Length, color = "magenta", braille = FALSE))
})

test_that("plotcli_line works", {
  x <- seq(0, 2*pi, length.out = 50)
  y <- sin(x)
  expect_silent(plotcli_line(x = x, y = cos(y), color = "green"))
  expect_silent(plotcli_line(x = x, y = x, color = "magenta", braille = FALSE))
})

test_that("plotcli R6 class works", {
  plot_width = 80
  plot_height = 40
  x <- seq(0, 2 * pi, length.out = 100)
  y1 <- sin(x)
  y2 <- cos(x)

  data_1 = list(x = x, y = y1, name = "y1 = sin(x)", color = "blue", type = "line", braille = FALSE)
  data_2 = list(x = x, y = y2, name = "y2 = cos(x)", color = "red", type = "line", braille = FALSE)

  plot <- plotcli$new(plot_width = plot_width, plot_height = plot_height, x_label = "X-axis", y_label = "Y-axis")
  expect_silent(plot$add_data(data_1))
  expect_silent(plot$add_data(data_2))
  expect_output(plot$print_plot())
})