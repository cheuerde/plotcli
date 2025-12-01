#!/usr/bin/env Rscript
# plotcli Showcase
# Run this script to see all the features of plotcli in action
# Usage: Rscript docs/showcase.R

library(plotcli)
library(ggplot2)
library(dplyr)

cat("\n")
cat("========================================================================\n")
cat("                      plotcli Feature Showcase\n")
cat("========================================================================\n\n")

# -----------------------------------------------------------------------------
cat("1. SCATTER PLOT WITH LEGEND\n")
cat("------------------------------------------------------------------------\n\n")

p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "MPG vs Weight by Cylinders",
       x = "Weight (1000 lbs)", y = "Miles per Gallon",
       color = "Cylinders") +
  theme_bw()
ggplotcli(p, width = 75, height = 20)

cat("\n")

# -----------------------------------------------------------------------------
cat("2. DENSITY PLOT WITH FILL COLORS\n")
cat("------------------------------------------------------------------------\n\n")

p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.7) +
  labs(title = "Iris Sepal Length Distribution",
       x = "Sepal Length (cm)", y = "Density") +
  theme_bw()
ggplotcli(p, width = 75, height = 18)

cat("\n")

# -----------------------------------------------------------------------------
cat("3. BOXPLOT WITH OUTLIERS\n")
cat("------------------------------------------------------------------------\n\n")

set.seed(42)
boxplot_data <- data.frame(
  category = rep(c("Group A", "Group B", "Group C", "Group D", "Group E"), each = 50),
  value = c(
    rnorm(50, mean = 25, sd = 5),
    rnorm(50, mean = 35, sd = 6),
    rnorm(50, mean = 20, sd = 4),
    rnorm(50, mean = 40, sd = 7),
    rnorm(50, mean = 30, sd = 5)
  )
)

p <- ggplot(boxplot_data, aes(x = category, y = value, fill = category)) +
  geom_boxplot() +
  labs(title = "Distribution Comparison by Group",
       x = "Category", y = "Value") +
  theme_bw() +
  theme(legend.position = "none")
ggplotcli(p, width = 75, height = 22, boxplot_style = "ascii")

cat("\n")

# -----------------------------------------------------------------------------
cat("4. HEATMAP (CORRELATION MATRIX)\n")
cat("------------------------------------------------------------------------\n\n")

cor_data <- cor(mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")]) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p <- ggplot(cor_data, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap - mtcars") +
  theme_minimal()
ggplotcli(p, width = 70, height = 20)

cat("\n")

# -----------------------------------------------------------------------------
cat("5. FACETED PLOT (facet_wrap)\n")
cat("------------------------------------------------------------------------\n\n")

p <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 2) +
  facet_wrap(~cyl, ncol = 2) +
  labs(title = "Highway MPG vs Engine Displacement",
       subtitle = "Faceted by Cylinder Count",
       x = "Displacement (L)", y = "Highway MPG",
       color = "Drive") +
  theme_bw()
ggplotcli(p, width = 85, height = 26)

cat("\n")

# -----------------------------------------------------------------------------
cat("6. LINE CHART WITH SMOOTHING\n")
cat("------------------------------------------------------------------------\n\n")

econ <- economics %>% filter(date >= "1990-01-01", date <= "2015-01-01")
p <- ggplot(econ, aes(x = date, y = unemploy/1000)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(color = "red", se = FALSE, method = "loess", span = 0.3) +
  labs(title = "US Unemployment Over Time",
       subtitle = "Blue: actual data, Red: smoothed trend",
       x = "Year", y = "Unemployed (millions)") +
  theme_bw()
ggplotcli(p, width = 80, height = 16)

cat("\n")

# -----------------------------------------------------------------------------
cat("7. BAR CHART (GROUPED)\n")
cat("------------------------------------------------------------------------\n\n")

p <- ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "dodge") +
  labs(title = "Vehicle Classes by Drive Type",
       x = "Vehicle Class", y = "Count",
       fill = "Drive") +
  theme_bw()
ggplotcli(p, width = 80, height = 16)

cat("\n")

# -----------------------------------------------------------------------------
cat("8. HISTOGRAM WITH DENSITY OVERLAY\n")
cat("------------------------------------------------------------------------\n\n")

p <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = after_stat(density)), bins = 12,
                 fill = "steelblue", color = "white") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribution of MPG",
       x = "Miles per Gallon", y = "Density") +
  theme_bw()
ggplotcli(p, width = 70, height = 16)

cat("\n")

# -----------------------------------------------------------------------------
cat("9. CANVAS TYPE COMPARISON\n")
cat("------------------------------------------------------------------------\n\n")

base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "Canvas Comparison") +
  theme_bw()

cat("BRAILLE canvas (high resolution):\n\n")
ggplotcli(base_plot, width = 50, height = 12, canvas_type = "braille")

cat("\nBLOCK canvas (medium resolution):\n\n")
ggplotcli(base_plot, width = 50, height = 12, canvas_type = "block")

cat("\nASCII canvas (maximum compatibility):\n\n")
ggplotcli(base_plot, width = 50, height = 12, canvas_type = "ascii")

cat("\n")

# -----------------------------------------------------------------------------
cat("10. FACET GRID (2D FACETING)\n")
cat("------------------------------------------------------------------------\n\n")

p <- ggplot(mpg %>% filter(cyl %in% c(4, 6, 8)), aes(x = displ, y = hwy)) +
  geom_point(color = "steelblue") +
  facet_grid(drv ~ cyl) +
  labs(title = "Facet Grid: Drive Type vs Cylinders") +
  theme_bw()
ggplotcli(p, width = 90, height = 28)

cat("\n")
cat("========================================================================\n")
cat("                         End of Showcase\n")
cat("========================================================================\n\n")
