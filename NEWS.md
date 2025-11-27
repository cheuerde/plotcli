# plotcli 0.2.0

## Major New Features

### Enhanced ggplotcli: Universal ggplot2 Converter

The `ggplotcli()` function has been completely rewritten to render any ggplot2 
plot in the terminal:

* **15 Supported Geoms**: `geom_point`, `geom_line`, `geom_path`, `geom_bar`, 
  `geom_col`, `geom_histogram`, `geom_density`, `geom_smooth`, `geom_area`, 
  `geom_segment`, `geom_hline`, `geom_vline`, `geom_rect`, `geom_text`, `geom_boxplot`

* **Legend Support**: Automatic legends for color and fill aesthetics

* **Faceting Support**: Both `facet_wrap()` and `facet_grid()` work automatically

* **Theme Auto-Detection**: Automatically respects ggplot2 themes - `theme_bw()` 
  adds borders and grids, `theme_classic()` shows only borders, etc.

* **Multiple Canvas Types**:
  - `braille`: Highest resolution using Unicode Braille patterns (2x4 dots per character)
  - `block`: Medium resolution using block elements
  - `ascii`: Basic ASCII characters for maximum compatibility

* **Styling Options**: Configurable borders, grid lines, titles, subtitles, 
  captions, and axis labels

### Boxplot Rendering

* New `boxplot_style` parameter: `"ascii"` (box-drawing characters) or `"braille"`
* Renders whiskers, box (Q1-Q3), median line, and outliers
* Perfect centering of whiskers and outliers

### Optimized Color Mapping

* Intelligent color assignment minimizes repetition across groups
* 6 groups get 6 distinct terminal colors
* 8+ groups use all available colors before repeating
* Colors sorted by hue for visual consistency

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
* Fixed axis label formatting (0 now displays as "0", not "0.0e+0")
* Fixed border overlap issue - data no longer appears on/within borders
* Fixed is_braille error with ANSI color codes

## Documentation

* Comprehensive vignette: "ggplotcli: Universal ggplot2 to Terminal Plotting"
* Showcase examples demonstrating maximum complexity plots

---

# plotcli 0.1.0

* Initial CRAN release
* Basic terminal plotting with `plotcli` R6 class
* Support for scatter, line, bar, and box plots
* Basic `ggplotcli()` function for ggplot2 conversion
