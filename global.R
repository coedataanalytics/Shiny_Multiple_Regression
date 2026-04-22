# global.R
# Purpose: Load packages, set global options, source helper functions
# Why: Centralizes package management and makes dependencies explicit

# =============================================================================
# Package Management with pacman
# =============================================================================
# pacman handles both installation and loading, making the app more portable
# It won't re-install packages that are already present

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  # Data manipulation and piping
  tidyverse,      # Includes dplyr, tidyr, ggplot2, readr, purrr, etc.
  magrittr,       # Additional pipe operators (%<>%, %T>%)
  janitor,        # Data cleaning utilities (clean_names, etc.)
  
  # File reading
  readxl,         # Excel files (.xlsx, .xls)
  haven,          # SPSS files (.sav)
  
  # Regression and modeling
  broom,          # Tidy model outputs
  car,            # VIF, influence plots, assumption tests
  lmtest,         # Breusch-Pagan test, etc.
  sandwich,       # Robust standard errors
  moments,        # Skewness and kurtosis calculations
  
  # Interactive visualization
  plotly,         # Interactive ggplot2 graphics
  patchwork,      # Combine multiple plots
  
  # Shiny ecosystem
  shiny,          # Core Shiny
  shinyjs,        # For show/hide functionality
  shinydashboard, # Dashboard layout
  shinycssloaders,# Loading spinners
  DT,             # Interactive data tables
  kableExtra,     # Enhanced table formatting
  
  # Output formatting
  knitr,          # For nice tables
  clipr           # Clipboard functionality
)

# =============================================================================
# Source Helper Functions
# =============================================================================
# Each file handles a single responsibility (separation of concerns)

source("R/utils.R")
source("R/data_processing.R")
source("R/descriptive_stats.R")
source("R/assumption_checks.R")
source("R/regression_analysis.R")
source("R/plotting_functions.R")
source("R/apa_formatting.R")

# =============================================================================
# Global Options
# =============================================================================
options(
  # Use plotly for interactive plots by default
  shiny.useplotly = TRUE,
  # Scientific notation threshold
  scipen = 999,
  # Significant digits for output
  digits = 3
)

# Florida Tech COE color palette
ft_colors <- c(
  primary = "#8B0000",    # Burgundy
  secondary = "#D8D4D0",  # Light gray
  accent = "#C5A95F",     # Gold
  dark = "#2C2C2C",       # Charcoal
  light = "#F5F3F0",      # Off-white
  success = "#2E7D32",    # Green
  error = "#C62828"       # Red
)

# For backward compatibility
apa_colors <- ft_colors