.onLoad <- function(libname, pkgname) {
  # Enable crayon colors even in non-interactive sessions (e.g., Rscript)
  # This ensures colored output works when running scripts
  if (is.null(getOption("crayon.enabled"))) {
    options(crayon.enabled = TRUE)
  }
}

.onAttach <- function(libname, pkgname) {
  # Set default options
  plotcli_options(plot_width = 60, plot_height = 20, braille = FALSE)

  # Display a startup message
  packageStartupMessage("plotcli loaded. Use plotcli_options() to set global options.")
}