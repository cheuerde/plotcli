#!/usr/bin/env Rscript
# Test script for ggplotcli2 with geom registry

# Force color output
options(crayon.enabled = TRUE)

# Load dependencies
library(R6)
library(ggplot2)
library(crayon)

# Source the package files
source("R/helper_functions.r")
source("R/canvas.r")
source("R/geom_registry.r")
source("R/ggplotcli2.r")

cat("\n=== ggplotcli2 Tests with Geom Registry ===\n\n")

# Test 1: Scatter plot
cat("1. Scatter Plot (geom_point)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  ggtitle("MPG vs Weight by Cylinders")

ggplotcli2(p1, width = 60, height = 18)

# Test 2: Line plot
cat("\n2. Line Plot (geom_line)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

df <- data.frame(
  x = 1:50,
  y = sin(1:50 / 5) * 10 + 20
)

p2 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  ggtitle("Sine Wave")

ggplotcli2(p2, width = 60, height = 15)

# Test 3: Multiple series
cat("\n3. Multiple Lines (grouped geom_line)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

df2 <- data.frame(
  x = rep(1:30, 2),
  y = c(sin(1:30 / 5) * 10, cos(1:30 / 5) * 10),
  series = rep(c("sin", "cos"), each = 30)
)

p3 <- ggplot(df2, aes(x = x, y = y, color = series, group = series)) +
  geom_line() +
  ggtitle("Sin and Cos")

ggplotcli2(p3, width = 60, height = 15)

# Test 4: Bar chart
cat("\n4. Bar Chart (geom_bar/geom_col)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

df3 <- data.frame(
  category = LETTERS[1:6],
  value = c(15, 25, 20, 30, 18, 22)
)

p4 <- ggplot(df3, aes(x = category, y = value, fill = category)) +
  geom_col() +
  ggtitle("Category Values")

ggplotcli2(p4, width = 50, height = 15)

# Test 5: Density
cat("\n5. Density Plot (geom_density)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

p5 <- ggplot(mtcars, aes(x = mpg, color = factor(cyl))) +
  geom_density() +
  ggtitle("MPG Density by Cylinders")

ggplotcli2(p5, width = 60, height = 15)

# Test 6: Histogram
cat("\n6. Histogram (geom_histogram)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

p6 <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "steelblue") +
  ggtitle("MPG Distribution")

ggplotcli2(p6, width = 60, height = 15)

# Test 7: Smooth
cat("\n7. Smooth Line (geom_smooth)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

p7 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "gray") +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("MPG vs Weight with Smooth")

suppressMessages(ggplotcli2(p7, width = 60, height = 15))

# Test 8: List registered geoms
cat("\n8. Registered Geoms\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
cat("Registered geom handlers:\n")
for (geom in sort(list_registered_geoms())) {
  cat(sprintf("  - %s\n", geom))
}

# Test 9: Different canvas types
cat("\n9. Canvas Type Comparison\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

simple_plot <- ggplot(data.frame(x = 1:20, y = sin(1:20 / 3) * 5 + 10), aes(x, y)) +
  geom_line(color = "green") +
  ggtitle("Canvas Comparison")

cat("\n  Braille Canvas:\n")
ggplotcli2(simple_plot, width = 50, height = 10, canvas_type = "braille")

cat("\n  Block Canvas:\n")
ggplotcli2(simple_plot, width = 50, height = 10, canvas_type = "block")

cat("\n  ASCII Canvas:\n")
ggplotcli2(simple_plot, width = 50, height = 10, canvas_type = "ascii")

# Test 10: Facet Wrap
cat("\n10. Facet Wrap (facet_wrap)\n")
cat(paste(rep("-", 70), collapse = ""), "\n")

p10 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  facet_wrap(~cyl) +
  ggtitle("MPG vs Weight by Cylinders")

ggplotcli2(p10, width = 70, height = 20)

# Test 11: Facet Grid
cat("\n11. Facet Grid (facet_grid)\n")
cat(paste(rep("-", 70), collapse = ""), "\n")

p11 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "green") +
  facet_grid(am ~ cyl) +
  ggtitle("MPG vs Weight: AM rows, Cyl columns")

ggplotcli2(p11, width = 70, height = 25)

# Test 12: Facet with lines
cat("\n12. Facet with Lines\n")
cat(paste(rep("-", 70), collapse = ""), "\n")

df_facet <- data.frame(
  x = rep(1:20, 3),
  y = c(sin(1:20 / 3), cos(1:20 / 3), sin(1:20 / 3) + cos(1:20 / 3)),
  panel = rep(c("sin", "cos", "sin+cos"), each = 20)
)

p12 <- ggplot(df_facet, aes(x = x, y = y)) +
  geom_line(color = "magenta") +
  facet_wrap(~panel, nrow = 1) +
  ggtitle("Trig Functions")

ggplotcli2(p12, width = 75, height = 15)

cat("\n=== All ggplotcli2 Tests Complete ===\n\n")

