#!/usr/bin/env Rscript
# Test that ggplot2 themes are automatically applied

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

cat("\n=== ggplot2 Theme Auto-Detection ===\n\n")

# Base plot
base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "MPG vs Weight")

# 1. theme_gray (default) - has grid, no border
cat("1. theme_gray() [DEFAULT]\n")
cat("   Expected: Grid lines, NO border\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
p1 <- base_plot + theme_gray()
ggplotcli2(p1, width = 60, height = 14, border = "auto", grid = "auto")

# 2. theme_bw - has grid AND border
cat("\n2. theme_bw()\n")
cat("   Expected: Grid lines AND border\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
p2 <- base_plot + theme_bw()
ggplotcli2(p2, width = 60, height = 14, border = "auto", grid = "auto")

# 3. theme_classic - has axis lines (border), NO grid
cat("\n3. theme_classic()\n")
cat("   Expected: Border/axis lines, NO grid\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
p3 <- base_plot + theme_classic()
ggplotcli2(p3, width = 60, height = 14, border = "auto", grid = "auto")

# 4. theme_minimal - has grid, NO border
cat("\n4. theme_minimal()\n")
cat("   Expected: Grid lines, NO border\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
p4 <- base_plot + theme_minimal()
ggplotcli2(p4, width = 60, height = 14, border = "auto", grid = "auto")

# 5. theme_void - NO grid, NO border
cat("\n5. theme_void()\n")
cat("   Expected: NO grid, NO border\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
p5 <- base_plot + theme_void()
ggplotcli2(p5, width = 60, height = 14, border = "auto", grid = "auto")

# 6. theme_linedraw - has grid AND border
cat("\n6. theme_linedraw()\n")
cat("   Expected: Grid lines AND border\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
p6 <- base_plot + theme_linedraw()
ggplotcli2(p6, width = 60, height = 14, border = "auto", grid = "auto")

# 7. Faceted with theme_bw
cat("\n7. Faceted + theme_bw()\n")
cat("   Expected: Grid + borders on EACH panel\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
p7 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "green") +
  facet_wrap(~cyl) +
  labs(title = "Faceted with theme_bw") +
  theme_bw()
ggplotcli2(p7, width = 70, height = 16, border = "auto", grid = "auto")

# 8. Faceted with theme_classic
cat("\n8. Faceted + theme_classic()\n")
cat("   Expected: Borders, NO grid on each panel\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
p8 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "magenta") +
  facet_wrap(~cyl) +
  labs(title = "Faceted with theme_classic") +
  theme_classic()
ggplotcli2(p8, width = 70, height = 16, border = "auto", grid = "auto")

cat("\n=== Theme Detection Tests Complete ===\n\n")

