#!/usr/bin/env Rscript
# Explore what ggplot2 provides for theming/styling

library(ggplot2)

cat("\n=== Exploring ggplot2 Theme and Layout Info ===\n\n")

# Create a sample plot with various elements
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(
    title = "Main Title",
    subtitle = "Subtitle here",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    caption = "Source: mtcars dataset",
    color = "Cylinders"
  ) +
  theme_minimal()

built <- ggplot_build(p)

cat("1. Labels available:\n")
print(names(built$plot$labels))
cat("\n")
for (label in names(built$plot$labels)) {
  cat(sprintf("   %s: %s\n", label, built$plot$labels[[label]]))
}

cat("\n2. Theme elements:\n")
theme <- built$plot$theme
cat("   Theme class:", class(theme), "\n")
cat("   Theme elements available:\n")
theme_elements <- names(theme)
# Show key ones
key_elements <- c("panel.background", "panel.border", "panel.grid.major", 
                  "panel.grid.minor", "axis.line", "axis.ticks", "axis.text",
                  "plot.title", "plot.subtitle", "plot.caption", "legend.position")
for (elem in key_elements) {
  if (elem %in% theme_elements) {
    cat(sprintf("   - %s: %s\n", elem, class(theme[[elem]])[1]))
  }
}

cat("\n3. Panel parameters:\n")
panel_params <- built$layout$panel_params[[1]]
cat("   Available params:", paste(names(panel_params), collapse = ", "), "\n")
cat("   x.range:", panel_params$x.range, "\n")
cat("   y.range:", panel_params$y.range, "\n")

cat("\n4. Scales:\n")
scales <- built$plot$scales$scales
for (i in seq_along(scales)) {
  scale <- scales[[i]]
  cat(sprintf("   Scale %d: %s (%s)\n", i, class(scale)[1], scale$aesthetics[1]))
  if (!is.null(scale$name)) cat(sprintf("      name: %s\n", scale$name))
}

cat("\n5. Guides/Legend info:\n")
cat("   Legend position:", 
    if (!is.null(theme$legend.position)) as.character(theme$legend.position) else "right (default)", 
    "\n")

cat("\n6. Coord info:\n")
coord <- built$plot$coordinates
cat("   Coord class:", class(coord)[1], "\n")

cat("\n=== Summary of Extractable Elements ===\n")
cat("
From ggplot_build() we can extract:
- Title, subtitle, caption
- X and Y axis labels
- Legend title and position
- Scale ranges and breaks
- Theme settings (though complex)
- Facet information

For terminal plots, we could add options for:
1. Box/border around plot area
2. Grid lines (major/minor)
3. Axis labels (from ggplot or custom)
4. Title position (center/left)
5. Legend display (on/off, position)
6. Axis tick marks
7. Plot margins
")

# Test with different themes
cat("\n=== Theme Comparison ===\n")
themes <- list(
  "theme_gray" = theme_gray(),
  "theme_bw" = theme_bw(),
  "theme_minimal" = theme_minimal(),
  "theme_classic" = theme_classic(),
  "theme_void" = theme_void()
)

for (name in names(themes)) {
  p_themed <- p + themes[[name]]
  built_themed <- ggplot_build(p_themed)
  
  # Check for panel border
  has_border <- !is.null(built_themed$plot$theme$panel.border) && 
                !inherits(built_themed$plot$theme$panel.border, "element_blank")
  
  # Check for axis lines
  has_axis_line <- !is.null(built_themed$plot$theme$axis.line) && 
                   !inherits(built_themed$plot$theme$axis.line, "element_blank")
  
  cat(sprintf("  %s: border=%s, axis_line=%s\n", 
              name, has_border, has_axis_line))
}

