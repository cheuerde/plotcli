# plotcli: ggplot2 in Your Terminal

[![CRAN status](https://www.r-pkg.org/badges/version/plotcli?color=green)](https://CRAN.R-project.org/package=plotcli)
[![CRAN checks](https://badges.cranchecks.info/worst/plotcli.svg)](https://cran.r-project.org/web/checks/check_results_plotcli.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/plotcli)](https://www.r-pkg.org/pkg/plotcli)
[![Downloads](https://cranlogs.r-pkg.org/badges/plotcli?color=blue)](https://www.r-pkg.org/pkg/plotcli)

<p align="center">
  <img src="docs/scatter.png" alt="plotcli: colored scatter plot in terminal" width="700">
</p>

plotcli renders ggplot2 plots directly in the terminal using Unicode Braille characters and ANSI colors.
Write your ggplot code as usual, then call `ggplotcli()` to see it in the console -- no graphics device needed.

Inspired by the excellent [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl).

## Installation

```r
# From CRAN
install.packages("plotcli")

# Development version from GitHub
remotes::install_github("cheuerde/plotcli")
```

## Quick Start

Any ggplot2 plot works. Build your plot as usual, then pass it to `ggplotcli()`:

```r
library(plotcli)
library(ggplot2)

p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "MPG vs Weight by Cylinders",
       x = "Weight (1000 lbs)", y = "Miles per Gallon",
       color = "Cylinders") +
  theme_bw()

ggplotcli(p)
```

<p align="center">
  <img src="docs/scatter.png" alt="Scatter plot with colored groups" width="700">
</p>

## Gallery

### Boxplots

```r
p <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  labs(title = "Distribution Comparison by Group")
ggplotcli(p)
```

<p align="center">
  <img src="docs/boxplot.png" alt="Boxplot" width="700">
</p>

### Line Charts

```r
p <- ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(color = "steelblue") +
  geom_smooth(color = "red", se = FALSE) +
  labs(title = "US Unemployment Over Time")
ggplotcli(p)
```

<p align="center">
  <img src="docs/line.png" alt="Line chart" width="700">
</p>

### Bar Charts

<p align="center">
  <img src="docs/bar.png" alt="Bar chart" width="700">
</p>

### Histograms

<p align="center">
  <img src="docs/histogram.png" alt="Histogram" width="700">
</p>

### Density Plots

<p align="center">
  <img src="docs/density.png" alt="Density plot" width="700">
</p>

### Heatmaps

```r
cor_mat <- cor(mtcars[, c("mpg", "cyl", "disp", "hp", "wt", "qsec")])
df <- as.data.frame(as.table(cor_mat))
names(df) <- c("Var1", "Var2", "value")
p <- ggplot(df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title = "Correlation Heatmap", fill = "correlation")
ggplotcli(p)
```

<p align="center">
  <img src="docs/heatmap.png" alt="Heatmap" width="700">
</p>

### Faceted Plots

```r
p <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~drv) +
  theme_bw()
ggplotcli(p, width = 80, height = 18)
```

<p align="center">
  <img src="docs/facet.png" alt="Faceted plot" width="700">
</p>

### Canvas Types

**Block canvas** -- medium resolution using block characters:
<p align="center">
  <img src="docs/block.png" alt="Block canvas" width="600">
</p>

**ASCII canvas** -- basic ASCII for maximum compatibility:
<p align="center">
  <img src="docs/ascii.png" alt="ASCII canvas" width="600">
</p>

## Supported Geoms (27)

| Geom | Status |
|------|--------|
| `geom_point` | :white_check_mark: |
| `geom_line`, `geom_path` | :white_check_mark: |
| `geom_step` | :white_check_mark: |
| `geom_bar`, `geom_col`, `geom_histogram` | :white_check_mark: |
| `geom_boxplot` | :white_check_mark: |
| `geom_violin` | :white_check_mark: |
| `geom_density` | :white_check_mark: |
| `geom_smooth` | :white_check_mark: |
| `geom_area` | :white_check_mark: |
| `geom_ribbon` | :white_check_mark: |
| `geom_segment`, `geom_hline`, `geom_vline`, `geom_abline` | :white_check_mark: |
| `geom_errorbar`, `geom_linerange`, `geom_pointrange`, `geom_crossbar` | :white_check_mark: |
| `geom_rect`, `geom_tile`, `geom_raster` (heatmaps) | :white_check_mark: |
| `geom_text`, `geom_label` | :white_check_mark: |
| `geom_rug` | :white_check_mark: |
| `facet_wrap`, `facet_grid` | :white_check_mark: |

## Options

```r
# Control size
ggplotcli(p, width = 80, height = 24)

# Canvas types
ggplotcli(p, canvas_type = "braille")  # High resolution (default)
ggplotcli(p, canvas_type = "block")    # Block characters
ggplotcli(p, canvas_type = "ascii")    # ASCII only
```

## Direct R6 Class Usage

For lower-level control, use the `plotcli` R6 class directly:

```r
pc <- plotcli$new(plot_width = 60, plot_height = 20, x_label = "wt", y_label = "mpg",
                  title = "MPG vs Weight")
pc$add_data(list(x = mtcars$wt, y = mtcars$mpg, type = "scatter", name = "mtcars"))
pc$print_plot()
```

## Similar Projects

- [txtplot](https://github.com/bbnkmp/txtplot/): The OG in R
- [r-plot](https://github.com/geotheory/r-plot): Collection of excellent terminal plotting functions
- [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl): The gold standard for terminal graphics
- [plotext](https://github.com/piccolomo/plotext): Powerful terminal graphics in Python

## License

`plotcli` is released under the LGPL-3 License.
