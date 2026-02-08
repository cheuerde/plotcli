library(testthat)
library(plotcli)
library(ggplot2)

test_that("GeomTile works", {
  df_tile <- expand.grid(x = 1:5, y = 1:5)
  df_tile$z <- runif(25)
  p <- ggplot(df_tile, aes(x, y, fill = z)) + geom_tile()

  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomPolygon warns about missing handler", {
  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
  values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  )
  positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3, 0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5, 2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  )
  datapoly <- merge(values, positions, by = c("id"))
  p <- ggplot(datapoly, aes(x = x, y = y)) + geom_polygon(aes(fill = value, group = id))

  expect_warning(res <- ggplotcli(p), "No handler registered for geom: GeomPolygon")
  expect_s3_class(res, "Canvas")
})

test_that("Faceting works", {
  p <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point() +
    facet_wrap(~ cyl)

  expect_output(res <- ggplotcli(p))
})

test_that("GeomBar works", {
  p <- ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomStep works", {
  df <- data.frame(x = 1:10, y = cumsum(rnorm(10)))
  p <- ggplot(df, aes(x, y)) + geom_step()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomAbline works", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_abline(intercept = 37, slope = -5, color = "red")
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomRibbon works", {
  df <- data.frame(x = 1:20, y = sin(1:20 / 3),
                   ymin = sin(1:20 / 3) - 0.5, ymax = sin(1:20 / 3) + 0.5)
  p <- ggplot(df, aes(x, y)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "steelblue") +
    geom_line()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomSmooth with ribbon works", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_smooth(method = "lm")
  expect_output(res <- suppressMessages(ggplotcli(p)))
  expect_s3_class(res, "Canvas")
})

test_that("GeomErrorbar works", {
  df <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 4),
                   ymin = c(1, 3, 2, 4, 3), ymax = c(3, 5, 4, 6, 5))
  p <- ggplot(df, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_errorbar(width = 0.3) + geom_point()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomLinerange works", {
  df <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 4),
                   ymin = c(1, 3, 2, 4, 3), ymax = c(3, 5, 4, 6, 5))
  p <- ggplot(df, aes(x, y, ymin = ymin, ymax = ymax)) + geom_linerange()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomPointrange works", {
  df <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 4),
                   ymin = c(1, 3, 2, 4, 3), ymax = c(3, 5, 4, 6, 5))
  p <- ggplot(df, aes(x, y, ymin = ymin, ymax = ymax)) + geom_pointrange()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomCrossbar works", {
  df <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 4),
                   ymin = c(1, 3, 2, 4, 3), ymax = c(3, 5, 4, 6, 5))
  p <- ggplot(df, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_crossbar(fill = "lightblue")
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomRug works", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rug()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomLabel works", {
  df <- data.frame(x = c(1, 2, 3), y = c(3, 1, 2), lab = c("A", "B", "C"))
  p <- ggplot(df, aes(x, y, label = lab)) + geom_label()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomRaster works", {
  df <- expand.grid(x = 1:10, y = 1:10)
  df$z <- runif(100)
  p <- ggplot(df, aes(x, y, fill = z)) + geom_raster()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})

test_that("GeomViolin works", {
  p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin()
  expect_output(res <- ggplotcli(p))
  expect_s3_class(res, "Canvas")
})
