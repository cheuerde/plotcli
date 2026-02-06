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
