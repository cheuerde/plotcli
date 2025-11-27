#' Canvas Classes for Terminal Plotting
#'
#' Abstract canvas system that provides different rendering backends:
#' - AsciiCanvas: Basic ASCII characters (*, -, |, +)
#' - BrailleCanvas: Unicode Braille patterns (2x4 dots per cell = 8x resolution)
#' - BlockCanvas: Unicode block elements (half-blocks for 2x vertical resolution)
#'
#' @name Canvas
NULL

#' Base Canvas Class
#'
#' Abstract base class for all canvas types. Provides the interface
#' that all canvas implementations must follow.
#'
#' @export
Canvas <- R6Class("Canvas",
  public = list(
    #' @field width Character width of the canvas
    width = NULL,
    #' @field height Character height of the canvas
    height = NULL,
    #' @field pixel_width Pixel width (may be higher than char width for Braille/Block)
    pixel_width = NULL,
    #' @field pixel_height Pixel height (may be higher than char height for Braille/Block)
    pixel_height = NULL,
    #' @field matrix Character matrix for rendering
    matrix = NULL,
    #' @field color_matrix Parallel matrix tracking colors per cell
    color_matrix = NULL,
    #' @field x_mult Horizontal multiplier (pixels per character)
    x_mult = 1,
    #' @field y_mult Vertical multiplier (pixels per character)
    y_mult = 1,

    #' @description Initialize the canvas
    #' @param width Character width
    #' @param height Character height
    initialize = function(width, height) {
      self$width <- width
      self$height <- height
      self$pixel_width <- width * self$x_mult
      self$pixel_height <- height * self$y_mult
      self$matrix <- matrix(" ", nrow = height, ncol = width)
      self$color_matrix <- matrix(NA_character_, nrow = height, ncol = width)
    },

    #' @description Set a pixel at the given coordinates

    #' @param x X coordinate (1-based, in pixel space)
    #' @param y Y coordinate (1-based, in pixel space, 1 = top)
    #' @param color Optional color name
    set_pixel = function(x, y, color = NULL) {
      stop("set_pixel must be implemented by subclass")
    },

    #' @description Draw a line between two points
    #' @param x0 Start X coordinate
    #' @param y0 Start Y coordinate
    #' @param x1 End X coordinate
    #' @param y1 End Y coordinate
    #' @param color Optional color name
    draw_line = function(x0, y0, x1, y1, color = NULL) {
      # Use Bresenham's algorithm at pixel resolution
      points <- bresenham(
        round(x0), round(y0),
        round(x1), round(y1)
      )
      for (pt in points) {
        self$set_pixel(pt[1], pt[2], color)
      }
    },

    #' @description Draw multiple connected line segments
    #' @param xs Vector of X coordinates
    #' @param ys Vector of Y coordinates
    #' @param color Optional color name
    draw_polyline = function(xs, ys, color = NULL) {
      if (length(xs) != length(ys)) stop("xs and ys must have same length")
      if (length(xs) < 2) return()
      
      for (i in 1:(length(xs) - 1)) {
        self$draw_line(xs[i], ys[i], xs[i + 1], ys[i + 1], color)
      }
    },

    #' @description Draw points (scatter)
    #' @param xs Vector of X coordinates
    #' @param ys Vector of Y coordinates
    #' @param color Optional color name
    draw_points = function(xs, ys, color = NULL) {
      if (length(xs) != length(ys)) stop("xs and ys must have same length")
      
      for (i in seq_along(xs)) {
        self$set_pixel(round(xs[i]), round(ys[i]), color)
      }
    },

    #' @description Fill a rectangle
    #' @param x0 Left X coordinate
    #' @param y0 Top Y coordinate
    #' @param x1 Right X coordinate
    #' @param y1 Bottom Y coordinate
    #' @param color Optional color name
    fill_rect = function(x0, y0, x1, y1, color = NULL) {
      x0 <- round(x0); x1 <- round(x1)
      y0 <- round(y0); y1 <- round(y1)
      
      for (x in x0:x1) {
        for (y in y0:y1) {
          self$set_pixel(x, y, color)
        }
      }
    },

    #' @description Fill a vertical bar from bottom up to a height
    #' @param x X coordinate (center of bar in pixel space)
    #' @param height Height in pixels from bottom
    #' @param bar_width Width of bar in pixels (default 2)
    #' @param color Optional color name
    fill_bar = function(x, height, bar_width = 2, color = NULL) {
      x <- round(x)
      height <- round(height)
      half_width <- floor(bar_width / 2)
      
      for (dx in (-half_width):(half_width - 1 + bar_width %% 2)) {
        for (y in (self$pixel_height - height + 1):self$pixel_height) {
          self$set_pixel(x + dx, y, color)
        }
      }
    },

    #' @description Place text at a position
    #' @param x X coordinate (character position)
    #' @param y Y coordinate (character position)
    #' @param text Text string to place
    #' @param color Optional color name
    draw_text = function(x, y, text, color = NULL) {
      chars <- strsplit(text, "")[[1]]
      for (i in seq_along(chars)) {
        col <- x + i - 1
        if (col >= 1 && col <= self$width && y >= 1 && y <= self$height) {
          self$matrix[y, col] <- chars[i]
          if (!is.null(color)) {
            self$color_matrix[y, col] <- color
          }
        }
      }
    },

    #' @description Apply colors to the matrix
    #' @return Matrix with ANSI color codes applied
    apply_colors = function() {
      result <- self$matrix
      for (i in 1:nrow(result)) {
        for (j in 1:ncol(result)) {
          if (!is.na(self$color_matrix[i, j])) {
            result[i, j] <- make_colored(result[i, j], self$color_matrix[i, j])
          }
        }
      }
      return(result)
    },

    #' @description Get the rendered matrix (with colors)
    #' @return Character matrix
    render = function() {
      self$apply_colors()
    },

    #' @description Print the canvas to console
    print = function() {
      rendered <- self$render()
      cat("\n")
      for (i in 1:nrow(rendered)) {
        cat(paste(rendered[i, ], collapse = ""), "\n")
      }
      invisible(self)
    },

    #' @description Clear the canvas
    clear = function() {
      self$matrix <- matrix(" ", nrow = self$height, ncol = self$width)
      self$color_matrix <- matrix(NA_character_, nrow = self$height, ncol = self$width)
    },

    #' @description Draw a rectangle outline
    #' @param x0 Left X coordinate (pixel space)
    #' @param y0 Top Y coordinate (pixel space)
    #' @param x1 Right X coordinate (pixel space)
    #' @param y1 Bottom Y coordinate (pixel space)
    #' @param color Optional color name
    draw_rect = function(x0, y0, x1, y1, color = NULL) {
      x0 <- round(x0); x1 <- round(x1)
      y0 <- round(y0); y1 <- round(y1)
      
      # Draw four sides
      self$draw_line(x0, y0, x1, y0, color)  # Top
      self$draw_line(x0, y1, x1, y1, color)  # Bottom
      self$draw_line(x0, y0, x0, y1, color)  # Left
      self$draw_line(x1, y0, x1, y1, color)  # Right
    },

    #' @description Fill an area between a polyline and the bottom
    #' @param xs Vector of X coordinates
    #' @param ys Vector of Y coordinates
    #' @param color Optional color name
    fill_area = function(xs, ys, color = NULL) {
      if (length(xs) != length(ys)) stop("xs and ys must have same length")
      if (length(xs) < 2) return()
      
      # For each x, find the y value and fill from there to bottom
      for (i in seq_along(xs)) {
        x <- round(xs[i])
        y <- round(ys[i])
        
        if (x >= 1 && x <= self$pixel_width) {
          # Fill from y to bottom (pixel_height)
          for (py in y:self$pixel_height) {
            self$set_pixel(x, py, color)
          }
        }
      }
    },

    #' @description Draw a segment (line with optional arrowhead)
    #' @param x0 Start X coordinate
    #' @param y0 Start Y coordinate
    #' @param x1 End X coordinate
    #' @param y1 End Y coordinate
    #' @param arrow_end Add arrowhead at end (default FALSE)
    #' @param color Optional color name
    draw_segment = function(x0, y0, x1, y1, arrow_end = FALSE, color = NULL) {
      # Draw the main line
      self$draw_line(x0, y0, x1, y1, color)
      
      # Add arrowhead if requested
      if (arrow_end) {
        # Calculate arrow direction
        dx <- x1 - x0
        dy <- y1 - y0
        len <- sqrt(dx^2 + dy^2)
        
        if (len > 0) {
          # Normalize and scale for arrow size
          arrow_len <- min(len * 0.2, 4)  # Arrow is 20% of line or max 4 pixels
          dx <- dx / len * arrow_len
          dy <- dy / len * arrow_len
          
          # Arrow wings at 30 degrees
          wing_x1 <- x1 - dx + dy * 0.5
          wing_y1 <- y1 - dy - dx * 0.5
          wing_x2 <- x1 - dx - dy * 0.5
          wing_y2 <- y1 - dy + dx * 0.5
          
          self$draw_line(x1, y1, wing_x1, wing_y1, color)
          self$draw_line(x1, y1, wing_x2, wing_y2, color)
        }
      }
    },

    #' @description Draw a horizontal line
    #' @param y Y coordinate
    #' @param x0 Start X (default 1)
    #' @param x1 End X (default pixel_width)
    #' @param color Optional color name
    draw_hline = function(y, x0 = 1, x1 = NULL, color = NULL) {
      if (is.null(x1)) x1 <- self$pixel_width
      self$draw_line(x0, y, x1, y, color)
    },

    #' @description Draw a vertical line
    #' @param x X coordinate
    #' @param y0 Start Y (default 1)
    #' @param y1 End Y (default pixel_height)
    #' @param color Optional color name
    draw_vline = function(x, y0 = 1, y1 = NULL, color = NULL) {
      if (is.null(y1)) y1 <- self$pixel_height
      self$draw_line(x, y0, x, y1, color)
    },

    #' @description Draw a circle outline
    #' @param cx Center X coordinate
    #' @param cy Center Y coordinate
    #' @param r Radius in pixels
    #' @param color Optional color name
    draw_circle = function(cx, cy, r, color = NULL) {
      # Midpoint circle algorithm
      x <- r
      y <- 0
      err <- 0
      
      while (x >= y) {
        self$set_pixel(cx + x, cy + y, color)
        self$set_pixel(cx + y, cy + x, color)
        self$set_pixel(cx - y, cy + x, color)
        self$set_pixel(cx - x, cy + y, color)
        self$set_pixel(cx - x, cy - y, color)
        self$set_pixel(cx - y, cy - x, color)
        self$set_pixel(cx + y, cy - x, color)
        self$set_pixel(cx + x, cy - y, color)
        
        y <- y + 1
        err <- err + 1 + 2 * y
        
        if (2 * (err - x) + 1 > 0) {
          x <- x - 1
          err <- err + 1 - 2 * x
        }
      }
    },

    #' @description Fill a circle
    #' @param cx Center X coordinate
    #' @param cy Center Y coordinate
    #' @param r Radius in pixels
    #' @param color Optional color name
    fill_circle = function(cx, cy, r, color = NULL) {
      for (y in (cy - r):(cy + r)) {
        for (x in (cx - r):(cx + r)) {
          if ((x - cx)^2 + (y - cy)^2 <= r^2) {
            self$set_pixel(x, y, color)
          }
        }
      }
    },

    #' @description Draw a polygon outline
    #' @param xs Vector of X coordinates
    #' @param ys Vector of Y coordinates
    #' @param closed Whether to close the polygon (default TRUE)
    #' @param color Optional color name
    draw_polygon = function(xs, ys, closed = TRUE, color = NULL) {
      if (length(xs) != length(ys)) stop("xs and ys must have same length")
      if (length(xs) < 2) return()
      
      # Draw all edges
      for (i in 1:(length(xs) - 1)) {
        self$draw_line(xs[i], ys[i], xs[i + 1], ys[i + 1], color)
      }
      
      # Close the polygon if requested
      if (closed && length(xs) >= 3) {
        self$draw_line(xs[length(xs)], ys[length(ys)], xs[1], ys[1], color)
      }
    }
  )
)


#' ASCII Canvas Class
#'
#' Simple canvas using basic ASCII characters.
#' Resolution: 1x1 (one pixel per character)
#'
#' @export
AsciiCanvas <- R6Class("AsciiCanvas",
  inherit = Canvas,
  public = list(
    #' @field point_char Character used for points
    point_char = "*",
    
    #' @description Initialize ASCII canvas
    #' @param width Character width
    #' @param height Character height
    #' @param point_char Character to use for points (default "*")
    initialize = function(width, height, point_char = "*") {
      self$x_mult <- 1
      self$y_mult <- 1
      self$point_char <- point_char
      super$initialize(width, height)
    },

    #' @description Set a pixel
    #' @param x X coordinate (1-based)
    #' @param y Y coordinate (1-based, 1 = top)
    #' @param color Optional color name
    set_pixel = function(x, y, color = NULL) {
      x <- round(x)
      y <- round(y)
      
      if (x < 1 || x > self$width || y < 1 || y > self$height) return()
      
      self$matrix[y, x] <- self$point_char
      if (!is.null(color)) {
        self$color_matrix[y, x] <- color
      }
    }
  )
)


#' Braille Canvas Class
#'
#' High-resolution canvas using Unicode Braille patterns.
#' Resolution: 2x4 (2 horizontal, 4 vertical dots per character = 8x resolution)
#'
#' Braille dot layout (dot numbers and bit values):
#' \preformatted{
#'        Col 0  Col 1     Bit values
#' Row 0:   1      4        0x01  0x08
#' Row 1:   2      5        0x02  0x10
#' Row 2:   3      6        0x04  0x20
#' Row 3:   7      8        0x40  0x80
#' }
#'
#' @export
BrailleCanvas <- R6Class("BrailleCanvas",
  inherit = Canvas,
  public = list(
    #' @field braille_base Unicode code point for empty Braille character
    braille_base = 0x2800L,

    #' @description Initialize Braille canvas
    #' @param width Character width
    #' @param height Character height
    initialize = function(width, height) {
      self$x_mult <- 2
      self$y_mult <- 4
      super$initialize(width, height)
    },

    #' @description Get the bit value for a dot position
    #' @param dot_row Row within Braille cell (0-3)
    #' @param dot_col Column within Braille cell (0-1)
    #' @return Bit value
    get_dot_bit = function(dot_row, dot_col) {
      if (dot_col == 0) {
        if (dot_row < 3) {
          return(bitwShiftL(1L, dot_row))
        } else {
          return(0x40L)
        }
      } else {
        if (dot_row < 3) {
          return(bitwShiftL(1L, dot_row + 3))
        } else {
          return(0x80L)
        }
      }
    },

    #' @description Set a pixel in Braille space
    #' @param x X coordinate (1-based, in pixel space: 1 to width*2)
    #' @param y Y coordinate (1-based, in pixel space: 1 to height*4, 1 = top)
    #' @param color Optional color name
    set_pixel = function(x, y, color = NULL) {
      x <- round(x)
      y <- round(y)
      
      # Clamp to valid range
      if (x < 1) x <- 1
      if (x > self$pixel_width) x <- self$pixel_width
      if (y < 1) y <- 1
      if (y > self$pixel_height) y <- self$pixel_height
      
      # Convert pixel coords to cell coords
      cell_col <- ((x - 1) %/% 2) + 1
      cell_row <- ((y - 1) %/% 4) + 1
      
      # Get dot position within cell
      dot_col <- (x - 1) %% 2
      dot_row <- (y - 1) %% 4
      
      # Get current character
      current_char <- self$matrix[cell_row, cell_col]
      if (current_char == " " || !is_braille(current_char)) {
        current_code <- self$braille_base
      } else {
        current_code <- utf8ToInt(current_char)
      }
      
      # Set the dot bit
      dot_bit <- self$get_dot_bit(dot_row, dot_col)
      new_code <- bitwOr(current_code, dot_bit)
      
      self$matrix[cell_row, cell_col] <- intToUtf8(new_code)
      
      if (!is.null(color)) {
        self$color_matrix[cell_row, cell_col] <- color
      }
    }
  )
)


#' Block Canvas Class
#'
#' Canvas using Unicode block elements for 2x vertical resolution.
#' Uses half-block characters: upper half (U+2580), lower half (U+2584), full block (U+2588).
#'
#' Resolution: 1x2 (1 horizontal, 2 vertical pixels per character)
#'
#' @export
BlockCanvas <- R6Class("BlockCanvas",
  inherit = Canvas,
  public = list(
    #' @field upper_block Upper half block character
    upper_block = "\u2580",  # ▀
    #' @field lower_block Lower half block character
    lower_block = "\u2584",  # ▄
    #' @field full_block Full block character
    full_block = "\u2588",   # █
    #' @field pixel_state Matrix tracking which half-pixels are set (0=none, 1=upper, 2=lower, 3=both)
    pixel_state = NULL,

    #' @description Initialize Block canvas
    #' @param width Character width
    #' @param height Character height
    initialize = function(width, height) {
      self$x_mult <- 1
      self$y_mult <- 2
      super$initialize(width, height)
      self$pixel_state <- matrix(0L, nrow = height, ncol = width)
    },

    #' @description Set a pixel in Block space
    #' @param x X coordinate (1-based, in pixel space: 1 to width)
    #' @param y Y coordinate (1-based, in pixel space: 1 to height*2, 1 = top)
    #' @param color Optional color name
    set_pixel = function(x, y, color = NULL) {
      x <- round(x)
      y <- round(y)
      
      # Clamp to valid range
      if (x < 1) x <- 1
      if (x > self$pixel_width) x <- self$pixel_width
      if (y < 1) y <- 1
      if (y > self$pixel_height) y <- self$pixel_height
      
      # Convert pixel coords to cell coords
      cell_col <- x
      cell_row <- ((y - 1) %/% 2) + 1
      
      # Determine if upper or lower half
      is_lower <- ((y - 1) %% 2) == 1
      
      # Update pixel state
      current_state <- self$pixel_state[cell_row, cell_col]
      if (is_lower) {
        new_state <- bitwOr(current_state, 2L)  # Set lower bit
      } else {
        new_state <- bitwOr(current_state, 1L)  # Set upper bit
      }
      self$pixel_state[cell_row, cell_col] <- new_state
      
      # Update character based on state
      char <- switch(as.character(new_state),
        "0" = " ",
        "1" = self$upper_block,
        "2" = self$lower_block,
        "3" = self$full_block,
        " "
      )
      self$matrix[cell_row, cell_col] <- char
      
      if (!is.null(color)) {
        self$color_matrix[cell_row, cell_col] <- color
      }
    },

    #' @description Clear the canvas
    clear = function() {
      super$clear()
      self$pixel_state <- matrix(0L, nrow = self$height, ncol = self$width)
    }
  )
)


#' Create a canvas of the specified type
#'
#' @param width Character width
#' @param height Character height
#' @param type Canvas type: "ascii", "braille", or "block"
#' @return A Canvas object
#' @export
create_canvas <- function(width, height, type = "braille") {
  switch(type,
    "ascii" = AsciiCanvas$new(width, height),
    "braille" = BrailleCanvas$new(width, height),
    "block" = BlockCanvas$new(width, height),
    stop("Unknown canvas type: ", type)
  )
}

