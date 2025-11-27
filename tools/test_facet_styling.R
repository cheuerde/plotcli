#!/usr/bin/env Rscript
# Test facet styling options (borders, grids)

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

cat("\n=== Facet Styling Options ===\n\n")

# Base faceted plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  facet_wrap(~cyl) +
  labs(title = "MPG vs Weight by Cylinders")

# 1. Default (no borders)
cat("1. Default Facet (no borders)\n")
cat(paste(rep("-", 75), collapse = ""), "\n")
ggplotcli2(p, width = 75, height = 18)

# 2. With borders around each panel
cat("\n2. Facet with Panel Borders\n")
cat(paste(rep("-", 75), collapse = ""), "\n")
ggplotcli2(p, width = 75, height = 18, border = TRUE)

# 3. With grid lines
cat("\n3. Facet with Grid Lines\n")
cat(paste(rep("-", 75), collapse = ""), "\n")
ggplotcli2(p, width = 75, height = 18, grid = "major")

# 4. With both borders and grid
cat("\n4. Facet with Borders + Grid\n")
cat(paste(rep("-", 75), collapse = ""), "\n")
ggplotcli2(p, width = 75, height = 18, border = TRUE, grid = "major")

# 5. Facet grid with styling
cat("\n5. Facet Grid with Borders\n")
cat(paste(rep("-", 75), collapse = ""), "\n")

p2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "green") +
  facet_grid(am ~ cyl) +
  labs(title = "MPG: AM (rows) × Cylinders (cols)")

ggplotcli2(p2, width = 75, height = 24, border = TRUE)

# 6. Line plots faceted with styling
cat("\n6. Faceted Lines with Grid\n")
cat(paste(rep("-", 75), collapse = ""), "\n")

df <- data.frame(
  x = rep(1:20, 3),
  y = c(sin(1:20/3), cos(1:20/3), sin(1:20/3) + cos(1:20/3)),
  func = rep(c("sin", "cos", "combined"), each = 20)
)

p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "magenta") +
  facet_wrap(~func, nrow = 1) +
  labs(title = "Trig Functions")

ggplotcli2(p3, width = 75, height = 14, border = TRUE, grid = "major")

# 7. Density plots faceted
cat("\n7. Faceted Density with Borders\n")
cat(paste(rep("-", 75), collapse = ""), "\n")

p4 <- ggplot(mtcars, aes(x = mpg)) +
  geom_density(color = "cyan") +
  facet_wrap(~cyl, nrow = 1) +
  labs(title = "MPG Density by Cylinders")

ggplotcli2(p4, width = 75, height = 14, border = TRUE)

cat("\n=== Facet Styling Tests Complete ===\n\n")

