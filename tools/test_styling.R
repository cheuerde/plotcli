#!/usr/bin/env Rscript
# Test script for new styling options

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

cat("\n=== ggplotcli2 Styling Options Tests ===\n\n")

# Create a base plot with all labels
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(
    title = "Fuel Efficiency vs Weight",
    subtitle = "Data from 1974 Motor Trend",
    x = "Weight (1000 lbs)",
    y = "MPG",
    caption = "Source: mtcars dataset",
    color = "Cylinders"
  )

# Test 1: Default (extracts all from ggplot)
cat("1. Default - All Labels from ggplot\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 22)

# Test 2: With border
cat("\n2. With Border\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 22, border = TRUE)

# Test 3: With major grid
cat("\n3. With Major Grid Lines\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 22, grid = "major")

# Test 4: Border + Grid
cat("\n4. Border + Grid\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 22, border = TRUE, grid = "major")

# Test 5: Left-aligned title
cat("\n5. Left-Aligned Title\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 20, title_align = "left")

# Test 6: No subtitle/caption
cat("\n6. No Subtitle or Caption\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 18, subtitle = FALSE, caption = FALSE)

# Test 7: No axis labels (just values)
cat("\n7. No Axis Labels\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 70, height = 18, axis_labels = FALSE)

# Test 8: Minimal (no axes at all)
cat("\n8. Minimal - No Axes\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
ggplotcli2(p, width = 60, height = 15, show_axes = FALSE, border = TRUE)

# Test 9: theme_bw (should auto-detect border)
cat("\n9. theme_bw (auto-detects border)\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
p_bw <- p + theme_bw()
ggplotcli2(p_bw, width = 70, height = 20)

# Test 10: theme_classic (should auto-detect axis lines)
cat("\n10. theme_classic (auto-detects axis lines)\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
p_classic <- p + theme_classic()
ggplotcli2(p_classic, width = 70, height = 20)

# Test 11: Line plot with grid
cat("\n11. Line Plot with Grid\n")
cat(paste(rep("-", 70), collapse = ""), "\n")

df <- data.frame(
  x = 1:50,
  y = cumsum(rnorm(50))
)

p_line <- ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  labs(
    title = "Random Walk",
    x = "Step",
    y = "Position"
  )

ggplotcli2(p_line, width = 70, height = 18, grid = "major", border = TRUE)

cat("\n=== All Styling Tests Complete ===\n\n")

