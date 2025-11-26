#!/usr/bin/env Rscript
#' Run visual tests for plotcli
#' 
#' This script generates HTML previews of all plot types for visual verification.
#' Run from the package root directory.

# Set working directory to package root if needed
if (!file.exists("DESCRIPTION")) {
  stop("Please run this script from the plotcli package root directory")
}

# Load required packages
suppressPackageStartupMessages({
  library(R6)
  library(crayon)
  library(stringr)
  library(ggplot2)
  library(rlang)
})

# Force crayon to output colors even in non-interactive mode
options(crayon.enabled = TRUE)
Sys.setenv(TERM = "xterm-256color")

# Source all R files in order
source("R/ascii_escape.r")
source("R/helper_functions.r")
source("R/plotcli.r")
source("R/class_functions.r")
source("R/plotcli_wrappers.r")
source("R/ggplotcli.r")

# Source the preview utilities
source("tools/preview_plot.R")

# Generate all test plots
files <- generate_test_plots("tools/test_output")

# Open the index in browser
browseURL(files$index)

message("\n✓ Visual test suite ready!")
message("  Check the browser for rendered plots with colors and Unicode.")

