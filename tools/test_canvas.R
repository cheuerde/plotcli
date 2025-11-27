#!/usr/bin/env Rscript
# Test script for Canvas abstraction layer

# Force color output
options(crayon.enabled = TRUE)

# Load dependencies
library(R6)
library(crayon)

# Source the package files
source("R/helper_functions.r")
source("R/canvas.r")

cat("\n=== Canvas Abstraction Layer Tests ===\n\n")

# Test 1: ASCII Canvas
cat("1. ASCII Canvas - Scatter Plot\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
ascii <- AsciiCanvas$new(40, 10)

# Draw some points
for (i in 1:40) {
  y <- round(5 + 4 * sin(i / 5))
  ascii$set_pixel(i, y, "blue")
}
ascii$print()

# Test 2: Braille Canvas - Scatter
cat("\n2. Braille Canvas - Scatter Plot (8x resolution)\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
braille <- BrailleCanvas$new(40, 10)

# Draw sine wave at pixel resolution
for (i in 1:(40 * 2)) {
  y <- round(20 + 18 * sin(i / 10))
  braille$set_pixel(i, y, "green")
}
braille$print()

# Test 3: Braille Canvas - Line
cat("\n3. Braille Canvas - Line Plot\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
braille2 <- BrailleCanvas$new(40, 10)

# Draw connected line
xs <- seq(1, 80, by = 2)
ys <- round(20 + 18 * sin(xs / 10))
braille2$draw_polyline(xs, ys, "magenta")
braille2$print()

# Test 4: Block Canvas
cat("\n4. Block Canvas - Bar Chart (2x vertical resolution)\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
block <- BlockCanvas$new(40, 10)

# Draw bars
bar_heights <- c(8, 15, 12, 18, 6, 14, 10, 16, 9, 13)
colors <- c("blue", "red", "green", "yellow", "cyan", "magenta", "blue", "red", "green", "yellow")
for (i in seq_along(bar_heights)) {
  block$fill_bar(i * 4, bar_heights[i], bar_width = 3, color = colors[i])
}
block$print()

# Test 5: Braille Canvas - Multiple colored series
cat("\n5. Braille Canvas - Multiple Series\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
braille3 <- BrailleCanvas$new(50, 12)

# Series 1: Sine (blue)
xs1 <- seq(1, 100, by = 2)
ys1 <- round(24 + 20 * sin(xs1 / 15))
braille3$draw_polyline(xs1, ys1, "blue")

# Series 2: Cosine (red)
xs2 <- seq(1, 100, by = 2)
ys2 <- round(24 + 20 * cos(xs2 / 15))
braille3$draw_polyline(xs2, ys2, "red")

braille3$print()

# Test 6: Fill rectangle
cat("\n6. Braille Canvas - Filled Rectangle\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
braille4 <- BrailleCanvas$new(30, 8)

# Draw a filled rectangle
braille4$fill_rect(10, 8, 40, 24, "cyan")
braille4$print()

# Test 7: Text overlay
cat("\n7. ASCII Canvas - Text Overlay\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
ascii2 <- AsciiCanvas$new(40, 5)
ascii2$draw_text(5, 2, "Hello World!", "yellow")
ascii2$draw_text(10, 4, "Canvas Test", "green")
ascii2$print()

# Test 8: create_canvas factory function
cat("\n8. Factory Function Test\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
for (type in c("ascii", "braille", "block")) {
  canvas <- create_canvas(20, 5, type)
  cat(sprintf("  Created %s canvas: %dx%d chars, %dx%d pixels\n",
              type, canvas$width, canvas$height, canvas$pixel_width, canvas$pixel_height))
}

# Test 9: New Drawing Primitives
cat("\n9. Drawing Primitives - Rectangle, Circle, Area\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

braille5 <- BrailleCanvas$new(50, 15)

# Draw a rectangle outline
braille5$draw_rect(5, 5, 30, 40, "blue")

# Draw a circle
braille5$draw_circle(60, 25, 15, "red")

# Draw a filled circle
braille5$fill_circle(85, 25, 10, "green")

braille5$print()

# Test 10: Area fill (density-like)
cat("\n10. Area Fill (like geom_area)\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

braille6 <- BrailleCanvas$new(50, 12)

# Create a curve and fill underneath
xs <- 1:100
ys <- round(10 + 30 * exp(-((xs - 50)^2) / 500))  # Gaussian-like curve
braille6$fill_area(xs, ys, "cyan")

braille6$print()

# Test 11: Segment with arrow
cat("\n11. Segments with Arrows\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

braille7 <- BrailleCanvas$new(40, 10)

# Draw arrows pointing in different directions
braille7$draw_segment(10, 20, 70, 20, arrow_end = TRUE, color = "yellow")  # Right
braille7$draw_segment(40, 35, 40, 5, arrow_end = TRUE, color = "magenta")  # Up

braille7$print()

# Test 12: Polygon
cat("\n12. Polygon (Pentagon)\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

braille8 <- BrailleCanvas$new(30, 12)

# Draw a pentagon
cx <- 30; cy <- 24; r <- 18
angles <- seq(from = -pi/2, by = 2*pi/5, length.out = 5)
xs <- cx + r * cos(angles)
ys <- cy + r * sin(angles)
braille8$draw_polygon(xs, ys, closed = TRUE, color = "green")

braille8$print()

cat("\n=== All Canvas Tests Complete ===\n\n")

