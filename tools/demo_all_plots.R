#!/usr/bin/env Rscript
# Comprehensive demo of all ggplotcli2 capabilities

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

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ggplotcli2 - Complete Demo Gallery                      ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

# =============================================================================
# SECTION 1: Basic Plot Types
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 1: Basic Plot Types                                                │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

# 1.1 Scatter Plot
cat("\n▶ 1.1 Scatter Plot with Color Aesthetic\n")
p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Scatter: MPG vs Weight", x = "Weight", y = "MPG")
ggplotcli2(p1, width = 70, height = 16)

# 1.2 Line Plot
cat("\n▶ 1.2 Line Plot\n")
df_line <- data.frame(
  x = 1:50,
  y = cumsum(rnorm(50, 0.2, 1))
)
p2 <- ggplot(df_line, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  labs(title = "Line: Random Walk", x = "Step", y = "Value")
ggplotcli2(p2, width = 70, height = 14, border = TRUE)

# 1.3 Multiple Lines
cat("\n▶ 1.3 Multiple Line Series\n")
df_multi <- data.frame(
  x = rep(1:40, 3),
  y = c(sin(1:40/5)*10, cos(1:40/5)*10, sin(1:40/5)*5 + cos(1:40/5)*5),
  series = rep(c("sin", "cos", "mixed"), each = 40)
)
p3 <- ggplot(df_multi, aes(x = x, y = y, color = series, group = series)) +
  geom_line() +
  labs(title = "Multiple Series: Trig Functions")
ggplotcli2(p3, width = 70, height = 14)

# 1.4 Bar Chart
cat("\n▶ 1.4 Bar Chart\n")
df_bar <- data.frame(
  category = c("A", "B", "C", "D", "E", "F"),
  value = c(23, 45, 12, 67, 34, 89)
)
p4 <- ggplot(df_bar, aes(x = category, y = value, fill = category)) +
  geom_col() +
  labs(title = "Bar Chart: Category Values")
ggplotcli2(p4, width = 60, height = 14)

# 1.5 Histogram
cat("\n▶ 1.5 Histogram\n")
p5 <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 12, fill = "steelblue") +
  labs(title = "Histogram: MPG Distribution", x = "Miles per Gallon", y = "Count")
ggplotcli2(p5, width = 70, height = 14)

# 1.6 Density Plot
cat("\n▶ 1.6 Density Plot\n")
p6 <- ggplot(mtcars, aes(x = mpg, color = factor(cyl))) +
  geom_density() +
  labs(title = "Density: MPG by Cylinders", x = "MPG", y = "Density")
ggplotcli2(p6, width = 70, height = 14)

# =============================================================================
# SECTION 2: Combined Geoms (Multiple Layers)
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 2: Combined Geoms (Multiple Layers)                                │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

# 2.1 Histogram + Density
cat("\n▶ 2.1 Histogram + Density Curve\n")
p7 <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "gray") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram with Density Overlay", x = "MPG")
ggplotcli2(p7, width = 70, height = 14)

# 2.2 Points + Smooth
cat("\n▶ 2.2 Scatter + Smooth Line\n")
p8 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "gray") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Scatter with LOESS Smooth")
suppressMessages(ggplotcli2(p8, width = 70, height = 14))

# 2.3 Multiple Densities
cat("\n▶ 2.3 Multiple Overlapping Densities\n")
p9 <- ggplot(iris, aes(x = Sepal.Length, color = Species)) +
  geom_density() +
  labs(title = "Iris: Sepal Length by Species")
ggplotcli2(p9, width = 70, height = 14)

# 2.4 Points + Lines (same data)
cat("\n▶ 2.4 Points + Connecting Lines\n")
df_pts <- data.frame(x = 1:15, y = c(3, 5, 4, 8, 7, 12, 10, 15, 13, 18, 16, 20, 19, 22, 21))
p10 <- ggplot(df_pts, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Line + Points Combined")
ggplotcli2(p10, width = 60, height = 12)

# =============================================================================
# SECTION 3: Faceting
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 3: Faceting                                                        │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

# 3.1 Facet Wrap
cat("\n▶ 3.1 Facet Wrap (Single Variable)\n")
p11 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  facet_wrap(~cyl) +
  labs(title = "MPG vs Weight by Cylinders")
ggplotcli2(p11, width = 75, height = 18)

# 3.2 Facet Grid
cat("\n▶ 3.2 Facet Grid (Two Variables)\n")
p12 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "green") +
  facet_grid(am ~ cyl) +
  labs(title = "MPG: Transmission (rows) × Cylinders (cols)")
ggplotcli2(p12, width = 75, height = 22)

# 3.3 Faceted Lines
cat("\n▶ 3.3 Faceted Line Plots\n")
df_facet <- data.frame(
  x = rep(1:25, 4),
  y = c(sin(1:25/4), cos(1:25/4), tan(1:25/8)/2, sin(1:25/4) + cos(1:25/4)),
  func = rep(c("sin", "cos", "tan", "sin+cos"), each = 25)
)
p13 <- ggplot(df_facet, aes(x = x, y = y)) +
  geom_line(color = "magenta") +
  facet_wrap(~func, nrow = 2) +
  labs(title = "Trig Functions Faceted")
ggplotcli2(p13, width = 75, height = 20)

# =============================================================================
# SECTION 4: Different Themes/Styles
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 4: Styling Options                                                 │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

base_plot <- ggplot(mtcars, aes(x = hp, y = qsec)) +
  geom_point(color = "cyan") +
  labs(title = "Quarter Mile Time vs Horsepower", 
       subtitle = "From mtcars dataset",
       x = "Horsepower", y = "1/4 Mile Time (sec)",
       caption = "Source: Motor Trend 1974")

# 4.1 Default
cat("\n▶ 4.1 Default Style\n")
ggplotcli2(base_plot, width = 65, height = 16)

# 4.2 With Border
cat("\n▶ 4.2 With Border\n")
ggplotcli2(base_plot, width = 65, height = 16, border = TRUE)

# 4.3 With Grid
cat("\n▶ 4.3 With Major Grid\n")
ggplotcli2(base_plot, width = 65, height = 16, grid = "major")

# 4.4 Border + Grid
cat("\n▶ 4.4 Border + Grid Combined\n")
ggplotcli2(base_plot, width = 65, height = 16, border = TRUE, grid = "major")

# 4.5 Minimal (no axes)
cat("\n▶ 4.5 Minimal Style (no axes)\n")
ggplotcli2(base_plot, width = 55, height = 12, show_axes = FALSE, border = TRUE)

# =============================================================================
# SECTION 5: Canvas Types
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 5: Canvas Types Comparison                                         │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

wave_plot <- ggplot(data.frame(x = 1:30, y = sin(1:30/4)*8 + 10), aes(x, y)) +
  geom_line(color = "green") +
  labs(title = "Same Plot, Different Canvas")

cat("\n▶ 5.1 Braille Canvas (highest resolution)\n")
ggplotcli2(wave_plot, width = 55, height = 10, canvas_type = "braille")

cat("\n▶ 5.2 Block Canvas (medium resolution)\n")
ggplotcli2(wave_plot, width = 55, height = 10, canvas_type = "block")

cat("\n▶ 5.3 ASCII Canvas (basic)\n")
ggplotcli2(wave_plot, width = 55, height = 10, canvas_type = "ascii")

# =============================================================================
# SECTION 6: Real-World Examples
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 6: Real-World Examples                                             │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

# 6.1 Time Series
cat("\n▶ 6.1 Time Series Plot\n")
set.seed(42)
df_ts <- data.frame(
  day = 1:60,
  sales = cumsum(rnorm(60, 5, 10)) + 100,
  returns = cumsum(rnorm(60, 1, 5)) + 20
)
df_ts_long <- data.frame(
  day = rep(df_ts$day, 2),
  value = c(df_ts$sales, df_ts$returns),
  metric = rep(c("Sales", "Returns"), each = 60)
)
p_ts <- ggplot(df_ts_long, aes(x = day, y = value, color = metric, group = metric)) +
  geom_line() +
  labs(title = "Daily Sales & Returns", x = "Day", y = "Amount ($)")
ggplotcli2(p_ts, width = 70, height = 14, border = TRUE)

# 6.2 Distribution Comparison
cat("\n▶ 6.2 Distribution Comparison\n")
df_dist <- data.frame(
  value = c(rnorm(200, 50, 10), rnorm(200, 60, 15)),
  group = rep(c("Control", "Treatment"), each = 200)
)
p_dist <- ggplot(df_dist, aes(x = value, color = group)) +
  geom_density() +
  labs(title = "A/B Test Results: Control vs Treatment", x = "Metric Value")
ggplotcli2(p_dist, width = 70, height = 14)

# 6.3 Correlation Plot
cat("\n▶ 6.3 Correlation with Trend\n")
p_corr <- ggplot(mtcars, aes(x = disp, y = hp)) +
  geom_point(color = "gray") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Engine Displacement vs Horsepower", 
       subtitle = "With linear regression",
       x = "Displacement (cu.in.)", y = "Horsepower")
suppressMessages(ggplotcli2(p_corr, width = 70, height = 16, border = TRUE))

# 6.4 Grouped Bar-like (using facets)
cat("\n▶ 6.4 Grouped Analysis with Facets\n")
p_grouped <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(aes(color = factor(gear))) +
  facet_wrap(~am, labeller = labeller(am = c("0" = "Automatic", "1" = "Manual"))) +
  labs(title = "MPG vs HP by Transmission Type", x = "Horsepower", y = "MPG")
ggplotcli2(p_grouped, width = 75, height = 16)

# =============================================================================
# SECTION 7: What We CAN'T Do Yet
# =============================================================================
cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ SECTION 7: Not Yet Supported (Future Work)                                 │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

cat("
  ⚠ The following geoms are NOT YET implemented:
  
  • geom_tile / geom_raster - Heatmaps
  • geom_boxplot - Box plots  
  • geom_violin - Violin plots
  • geom_ribbon - Ribbon/confidence bands
  • geom_errorbar - Error bars
  • geom_contour - Contour plots
  • geom_sf - Spatial/map data
  • geom_label - Text with background
  
  These would require additional geom handlers in the registry.
  
")

# =============================================================================
# Summary
# =============================================================================
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                              Summary                                       ║\n")
cat("╠════════════════════════════════════════════════════════════════════════════╣\n")
cat("║ Supported Geoms:                                                           ║\n")
cat("║   ✓ geom_point, geom_line, geom_path, geom_bar, geom_col                  ║\n")
cat("║   ✓ geom_histogram, geom_density, geom_area, geom_smooth                  ║\n")
cat("║   ✓ geom_segment, geom_hline, geom_vline, geom_rect, geom_text            ║\n")
cat("║                                                                            ║\n")
cat("║ Features:                                                                  ║\n")
cat("║   ✓ Multiple layers (combine geoms)                                       ║\n")
cat("║   ✓ Color/fill aesthetics                                                 ║\n")
cat("║   ✓ facet_wrap and facet_grid                                             ║\n")
cat("║   ✓ Titles, subtitles, captions, axis labels                              ║\n")
cat("║   ✓ Borders and grid lines                                                ║\n")
cat("║   ✓ Three canvas types (braille/block/ascii)                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

