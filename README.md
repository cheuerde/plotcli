# plotcli: Command-Line Plots for R

[![CRAN status](https://www.r-pkg.org/badges/version/plotcli?color=green)](https://CRAN.R-project.org/package=plotcli)
[![CRAN checks](https://badges.cranchecks.info/worst/plotcli.svg)](https://cran.r-project.org/web/checks/check_results_plotcli.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/plotcli)](https://www.r-pkg.org/pkg/plotcli)
[![Downloads](https://cranlogs.r-pkg.org/badges/plotcli?color=blue)](https://www.r-pkg.org/pkg/plotcli)


![Colored Density Plot on Terminal](docs/boxplot.jpg)

plotcli is an R package that brings the power of command-line plotting to your R environment. 
With a simple and intuitive interface, plotcli allows you to create and customize a 
variety of plot types directly in your console using Unicode Braille characters and ANSI colors.

## Features

- **`ggplotcli`**: Universal ggplot2 converter - render *any* ggplot in the terminal
- **14 Supported Geoms**: points, lines, bars, histograms, density, smooth, area, segments, and more
- **Faceting**: Full support for `facet_wrap()` and `facet_grid()`
- **Theme Auto-Detection**: Automatically respects ggplot2 themes (borders, grids)
- **Multiple Canvas Types**: Braille (high-res), Block, or ASCII
- **Colored Output**: Full ANSI color support for aesthetics
- **R6 Class Interface**: Direct plotting with `plotcli` class

`plotcli` is heavily inspired by the excellent [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl) library.

## Quick Start

```r
library(plotcli)
library(ggplot2)

# Any ggplot2 plot works!
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "MPG vs Weight")

# Render in terminal
ggplotcli(p)
```

Output:
```
                    MPG vs Weight                     
  35.0 ⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤  
       ⠒⠒⣗⢒⠒⠒⠒⠒⡗⠒⠚⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒  
  30.0 ⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉  
m      ⠤⠤⡧⠤⠤⠤⠤⠬⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤  
p 25.0 ⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⡒⠒⡗⠒⠒⠒⠒⠒⡗⠒⡚⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒  
g 20.0 ⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣙⣉⣙⣙⣉⣏⣉⣙⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉  
       ⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠼⡧⠤⠤⡤⠬⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤  
  15.0 ⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠒⡗⠒⠒⠒⠒⠲⡷⡖⠒⠲⠒⠒⡗⠓⠒⠒⠒⠒⡗⠒⠒  
       ⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉⣉⣋⣉⣏⣉⣉⣉⣉⣉⣏⣉⣉  
  10.0 ⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠤⡧⠤⠤⠦⠤⠦⡧⠤⠤  
            2.0         3.0         4.0         5.0        
                             wt                            
```

## Faceting Example

```r
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  facet_wrap(~cyl) +
  labs(title = "MPG by Cylinders") +
  theme_bw()  # Automatically adds borders!

ggplotcli(p, width = 70, height = 16)
```

Output:
```
                       MPG by Cylinders                        
               4                    6                    8     
      ⡏⠉⠩⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹ ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹ ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⡇   ⠁              ⢸ ⡇                  ⢸ ⡇                  ⢸
  30.0⡇⠉                 ⢸ ⡇                  ⢸ ⡇                  ⢸
      ⡇  ⢂               ⢸ ⡇                  ⢸ ⡇                  ⢸
      ⡇   ⠈⡀⢀ ⠈          ⢸ ⡇     ⡀⡀⢀          ⢸ ⡇                  ⢸
  20.0⡇                  ⢸ ⡇     ⠠  ⠠         ⢸ ⡇        ⢀ ⠠       ⢸
      ⡇                  ⢸ ⡇        ⠘         ⢸ ⡇          ⠄⢀      ⢸
      ⡇                  ⢸ ⡇                  ⢸ ⡇       ⠈⠐⡂⠂      ⠠⢸
  10.0⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸ ⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸ ⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣒⣸
        2.0      4.0         2.0      4.0         2.0      4.0     
```

## Styling Options

```r
# With border and grid
ggplotcli(p, border = TRUE, grid = "major")

# Different canvas types
ggplotcli(p, canvas_type = "braille")  # High resolution (default)
ggplotcli(p, canvas_type = "block")    # Medium resolution  
ggplotcli(p, canvas_type = "ascii")    # Basic ASCII
```

## Supported Geoms

| Geom | Status |
|------|--------|
| `geom_point` | ✓ |
| `geom_line`, `geom_path` | ✓ |
| `geom_bar`, `geom_col`, `geom_histogram` | ✓ |
| `geom_density` | ✓ |
| `geom_smooth` | ✓ |
| `geom_area` | ✓ |
| `geom_segment`, `geom_hline`, `geom_vline` | ✓ |
| `geom_rect` | ✓ |
| `geom_text` | ✓ |
| `geom_boxplot` | ✓ |
| `geom_tile` (heatmaps) | Planned |

## Installation

```r
# From CRAN
install.packages("plotcli")

# Or from GitHub for the latest development version
# Using remotes (recommended)
remotes::install_github("cheuerde/plotcli")

# Or using devtools
devtools::install_github("cheuerde/plotcli")
```

## Direct R6 Class Usage

For more control, use the `plotcli` R6 class directly:

```r
# Using plotcli R6 class directly
pc <- plotcli$new(width = 60, height = 20)
pc$add_data(mtcars$wt, mtcars$mpg)
pc$add_title("MPG vs Weight")
pc$print_plot()
```

Check the vignettes for all possible ways of using the package.

## Showcase

![Colored Density Plot on Terminal](docs/scatter.jpg)
![Colored Density Plot on Terminal](docs/density_raw.jpg)
![Colored Density Plot on Terminal](docs/regression.jpg)
![Colored Density Plot on Terminal](docs/hist.jpg)
![Colored Density Plot on Terminal](docs/pca.jpg)

## Similar Projects

 - [txtplot](https://github.com/bbnkmp/txtplot/): The OG in R
 - [r-plot](https://github.com/geotheory/r-plot): Collection of excellent terminal plotting functions
 - [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl): The gold standard for terminal graphics
 - [plotext](https://github.com/piccolomo/plotext): Powerful terminal graphics in python

## License

`plotcli` is released under the LGPL-3 License.
