# plotcli 0.2.0

## Major New Features

### ggplotcli2: Universal ggplot2 Converter

A new function `ggplotcli2()` that renders any ggplot2 plot in the terminal:

* **14 Supported Geoms**: `geom_point`, `geom_line`, `geom_path`, `geom_bar`, 
  `geom_col`, `geom_histogram`, `geom_density`, `geom_smooth`, `geom_area`, 
  `geom_segment`, `geom_hline`, `geom_vline`, `geom_rect`, `geom_text`

* **Faceting Support**: Both `facet_wrap()` and `facet_grid()` work automatically

* **Theme Auto-Detection**: Automatically respects ggplot2 themes - `theme_bw()` 
  adds borders and grids, `theme_classic()` shows only borders, etc.

* **Multiple Canvas Types**:
  - `braille`: Highest resolution using Unicode Braille patterns (2x4 dots per character)
  - `block`: Medium resolution using block elements
  - `ascii`: Basic ASCII characters for maximum compatibility

* **Styling Options**: Configurable borders, grid lines, titles, subtitles, 
  captions, and axis labels

### Canvas Abstraction Layer

New R6-based canvas system providing:

* Pixel-level drawing primitives (points, lines, rectangles, circles, polygons)
* Automatic Braille character composition for smooth curves
* Support for colored output via ANSI escape codes

### Geom Registry

Extensible system for adding custom geom handlers, making it easy to add 
support for additional ggplot2 geoms.

## Improvements

* Improved Braille bit mapping for more accurate scatter and line plots
* Better color handling for multi-group aesthetics
* Fixed issues with quosure parsing in aesthetic mappings

## Documentation

* New vignette: "ggplotcli2: Universal ggplot2 to Terminal Plotting"

---

# plotcli 0.1.0

* Initial CRAN release
* Basic terminal plotting with `plotcli` R6 class
* Support for scatter, line, bar, and box plots
* Original `ggplotcli()` function for basic ggplot2 conversion

