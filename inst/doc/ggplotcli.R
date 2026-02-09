## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
library(plotcli)
library(ggplot2)

# Create a ggplot
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "MPG vs Weight by Cylinders")

# Render in terminal
ggplotcli(p, width = 60, height = 16)


## -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Iris: Sepal Dimensions by Species")
ggplotcli(p, width = 60, height = 16)


## -----------------------------------------------------------------------------
# Multiple colored lines
df <- data.frame(
  x = rep(1:30, 2),
  y = c(sin(1:30/4) * 10, cos(1:30/4) * 10),
  type = rep(c("sin", "cos"), each = 30)
)
p <- ggplot(df, aes(x, y, color = type)) +
  geom_line() +
  labs(title = "Sine and Cosine Waves")
ggplotcli(p, width = 60, height = 12)


## -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_histogram(bins = 10, position = "dodge") +
  labs(title = "MPG Distribution by Cylinders")
ggplotcli(p, width = 60, height = 12)


## -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = mpg, color = factor(cyl))) +
  geom_density() +
  labs(title = "MPG Density by Cylinders")
ggplotcli(p, width = 60, height = 12)


## -----------------------------------------------------------------------------
df <- data.frame(
  category = c("A", "B", "C", "D", "E", "F"),
  value = c(25, 45, 30, 60, 35, 50)
)
p <- ggplot(df, aes(x = category, y = value, fill = category)) +
  geom_col() +
  labs(title = "Category Values")
ggplotcli(p, width = 60, height = 12)


## -----------------------------------------------------------------------------
set.seed(42)
df <- data.frame(
  group = factor(rep(paste0("Group ", 1:6), each = 50)),
  value = c(rnorm(50, 10, 3), rnorm(50, 5, 2), rnorm(50, 8, 4),
            rnorm(50, 6, 2), rnorm(50, 12, 3), rnorm(50, 7, 2))
)
p <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot Colored by Group", y = "Value", x = "Group")
ggplotcli(p, width = 80, height = 20, boxplot_style = "ascii")


## -----------------------------------------------------------------------------
ggplotcli(p, width = 80, height = 20, boxplot_style = "braille")


## -----------------------------------------------------------------------------
# Histogram with density overlay
p <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "gray") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Overlay")
ggplotcli(p, width = 60, height = 12)


## -----------------------------------------------------------------------------
# Points with smooth line
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "gray") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Scatter with LOESS Smooth")
suppressMessages(ggplotcli(p, width = 60, height = 12))


## -----------------------------------------------------------------------------
mtcars$cyl_fac <- factor(mtcars$cyl)
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = cyl_fac)) +
  geom_point() +
  facet_wrap(~cyl_fac) +
  labs(title = "MPG vs Weight: Faceted by Cylinders")
ggplotcli(p, width = 75, height = 16)


## -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(gear))) +
  geom_point() +
  facet_grid(am ~ cyl) +
  labs(title = "MPG: AM (rows) x Cylinders (cols)")
ggplotcli(p, width = 80, height = 20)


## -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = hp, y = qsec)) +
  geom_point(color = "cyan") +
  labs(title = "Quarter Mile Time vs HP")

# With border
ggplotcli(p, width = 55, height = 12, border = TRUE)

# With grid
ggplotcli(p, width = 55, height = 12, grid = "major")

# Both
ggplotcli(p, width = 55, height = 12, border = TRUE, grid = "major")


## -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "theme_bw() - Grid + Border")

# theme_bw has both grid and border
ggplotcli(p + theme_bw(), width = 60, height = 14, border = "auto", grid = "auto")

# theme_classic has border but no grid
ggplotcli(p + theme_classic() + labs(title = "theme_classic() - Border Only"), 
           width = 60, height = 14, border = "auto", grid = "auto")


## -----------------------------------------------------------------------------
wave <- ggplot(data.frame(x = 1:25, y = sin(1:25/4)*8), aes(x, y)) +
  geom_line(color = "green")

# Braille (highest resolution - 2x4 dots per character)
ggplotcli(wave + labs(title = "Braille Canvas (highest resolution)"), 
           width = 50, height = 8, canvas_type = "braille")

# Block (medium resolution - uses block characters)
ggplotcli(wave + labs(title = "Block Canvas (medium resolution)"), 
           width = 50, height = 8, canvas_type = "block")

# ASCII (basic, most compatible)
ggplotcli(wave + labs(title = "ASCII Canvas (most compatible)"), 
           width = 50, height = 8, canvas_type = "ascii")


## -----------------------------------------------------------------------------
set.seed(123)
df <- data.frame(
  group = factor(rep(paste0("G", 1:8), each = 30)),
  value = unlist(lapply(1:8, function(i) rnorm(30, mean = i * 2, sd = 1.5)))
)
p <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  labs(title = "8 Groups with Optimized Color Distribution")
ggplotcli(p, width = 85, height = 18, boxplot_style = "ascii")


## -----------------------------------------------------------------------------
set.seed(42)
df <- data.frame(
  treatment = factor(rep(c("Control", "Drug A", "Drug B"), each = 60)),
  timepoint = factor(rep(rep(c("Baseline", "Week 4", "Week 8"), each = 20), 3)),
  response = c(
    rnorm(20, 50, 10), rnorm(20, 52, 10), rnorm(20, 51, 10),  # Control
    rnorm(20, 50, 10), rnorm(20, 65, 12), rnorm(20, 70, 11),  # Drug A
    rnorm(20, 50, 10), rnorm(20, 58, 11), rnorm(20, 62, 10)   # Drug B
  )
)

p <- ggplot(df, aes(x = timepoint, y = response)) +
  geom_boxplot(aes(fill = treatment)) +
  labs(title = "Treatment Response Over Time",
       subtitle = "Faceted by treatment group",
       x = "Timepoint", y = "Response") +
  theme_bw()

ggplotcli(p, width = 100, height = 22, boxplot_style = "ascii")


## -----------------------------------------------------------------------------
# Simulated experiment data
set.seed(123)
x <- seq(0, 10, length.out = 50)
df <- data.frame(
  x = rep(x, 3),
  y = c(
    2 * x + rnorm(50, 0, 1.5),
    1.5 * x + 3 + rnorm(50, 0, 1.2),
    x^1.2 + rnorm(50, 0, 1)
  ),
  group = rep(c("Linear", "Offset", "Power"), each = 50)
)

p <- ggplot(df, aes(x = x, y = y, color = group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Multi-Group Regression Analysis",
       subtitle = "Points with LOESS smoothing",
       x = "Predictor", y = "Response") +
  theme_minimal()

suppressMessages(ggplotcli(p, width = 70, height = 18))


## -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  facet_wrap(~Species) +
  labs(title = "Iris Dataset: Sepal vs Petal Length",
       subtitle = "Faceted by species with color legend") +
  theme_bw()

ggplotcli(p, width = 80, height = 16)

