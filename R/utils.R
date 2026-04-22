# R/utils.R
# Purpose: General utility functions that don't fit elsewhere
# Why: Avoids code duplication and keeps other files focused

#' Safely check if a package is available
#' @param pkg Package name as string
#' @return Logical
is_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Round to specified decimals with trailing zeros preserved
#' @param x Numeric value or vector
#' @param digits Number of decimal places
#' @return Formatted string
round_pretty <- function(x, digits = 3) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

#' Create a status badge for UI
#' @param text Badge text
#' @param color Bootstrap color (success, warning, danger, info, primary)
#' @return HTML span element
status_badge <- function(text, color = "info") {
  span(
    class = paste0("badge badge-", color),
    text
  )
}

#' Check if variable is numeric (and not just stored as numeric)
#' @param x Vector to check
#' @param min_unique Minimum unique values required (to filter IDs coded as numeric)
#' @return Logical
is_truly_numeric <- function(x, min_unique = 3) {
  is.numeric(x) && length(unique(x[!is.na(x)])) >= min_unique
}

#' Check if variable is categorical
#' @param x Vector to check
#' @param max_unique Maximum unique values for categorical
#' @return Logical
is_categorical <- function(x, max_unique = 10) {
  is.factor(x) || is.character(x) || 
    (is.numeric(x) && length(unique(x[!is.na(x)])) <= max_unique)
}

#' Format p-value with APA style
#' @param p p-value
#' @return Formatted string
format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < .001) return("< .001")
  if (p < .01) return(paste0("= ", round_pretty(p, 3)))
  paste0("= ", round_pretty(p, 2))
}

#' Null-coalescing operator (like SQL COALESCE)
#' @param x Value to check
#' @param default Default if NULL
#' @return x if not NULL, else default
`%||%` <- function(x, default) {
  if (is.null(x)) default else x
}