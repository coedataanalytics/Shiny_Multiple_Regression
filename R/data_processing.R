# R/data_processing.R
# Purpose: All functions related to loading, cleaning, and preparing data
# Why: Isolates data I/O logic from analysis logic

#' Import data from various file formats
#' @param file_path Path to the data file
#' @param sheet Sheet name for Excel files (optional)
#' @return A tibble with the data
#' @export
import_data <- function(file_path, sheet = NULL) {
  
  # Get file extension
  ext <- tolower(tools::file_ext(file_path))
  
  # Import based on extension
  data <- switch(
    ext,
    "csv" = readr::read_csv(file_path, show_col_types = FALSE),
    "xlsx" = if (!is.null(sheet)) {
      readxl::read_excel(file_path, sheet = sheet)
    } else {
      readxl::read_excel(file_path)
    },
    "xls" = readxl::read_excel(file_path),
    "sav" = haven::read_sav(file_path),
    "dta" = haven::read_dta(file_path),
    stop(paste("Unsupported file format:", ext))
  )
  
  # Convert to tibble and clean names
  data <- data %>%
    tibble::as_tibble() %>%
    janitor::clean_names()  # If you add janitor to packages; otherwise:
  
  # Clean column names manually (alternative to janitor)
  names(data) <- names(data) %>%
    tolower() %>%
    gsub("[^a-z0-9_]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  return(data)
}

#' Detect variable types automatically
#' @param data A data frame or tibble
#' @return A tibble with variable names, types, and metadata
#' @export
detect_variable_types <- function(data) {
  
  # Guard clause
  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(tibble(
      variable = character(0),
      class = character(0),
      n_missing = integer(0),
      pct_missing = numeric(0),
      n_unique = integer(0),
      suggested_type = character(0)
    ))
  }
  
  tryCatch({
    var_info <- tibble(
      variable = names(data),
      class = map_chr(data, ~ paste(class(.), collapse = ", ")),
      n_missing = map_int(data, ~ sum(is.na(.))),
      pct_missing = map_dbl(data, ~ mean(is.na(.)) * 100),
      n_unique = map_int(data, ~ length(unique(.))),
      suggested_type = map_chr(data, suggest_type)
    )
    
    return(var_info)
  }, error = function(e) {
    warning("detect_variable_types failed: ", e$message)
    # Return basic info even if full detection fails
    return(tibble(
      variable = names(data),
      class = map_chr(data, ~ paste(class(.), collapse = ", ")),
      n_missing = map_int(data, ~ sum(is.na(.))),
      pct_missing = map_dbl(data, ~ mean(is.na(.)) * 100),
      n_unique = map_int(data, ~ length(unique(.))),
      suggested_type = map_chr(data, ~ ifelse(is.numeric(.), "continuous", "categorical"))
    ))
  })
}

#' Suggest variable type for analysis
#' @param x A vector
#' @return String: "continuous", "categorical", or "identifier"
suggest_type <- function(x) {
  n_unique <- length(unique(x[!is.na(x)]))
  n_total <- sum(!is.na(x))
  
  # Likely an identifier
  if (n_unique == n_total) {
    return("identifier")
  }
  
  # Categorical: few unique values or non-numeric
  if (is.factor(x) || is.character(x)) {
    return("categorical")
  }
  
  if (is.numeric(x)) {
    # Binary or small number of categories
    if (n_unique <= 2) {
      return("categorical")
    }
    return("continuous")
  }
  
  return("unknown")
}

#' Clean and prepare data for regression
#' @param data Raw data frame
#' @param dv_name Name of dependent variable
#' @param iv_names Names of independent variables
#' @param convert_factors Named vector of variables to convert to factors
#' @return List with cleaned data and preprocessing report
#' @export
prepare_regression_data <- function(data, dv_name, iv_names, 
                                    convert_factors = NULL) {
  
  report <- list(
    original_rows = nrow(data),
    removed_rows = 0,
    conversions = list()
  )
  
  # Create a copy
  clean_data <- data
  
  # Convert specified variables to factors
  if (!is.null(convert_factors)) {
    for (var in names(convert_factors)) {
      if (var %in% names(clean_data)) {
        clean_data[[var]] <- factor(clean_data[[var]])
        report$conversions[[var]] <- "converted to factor"
      }
    }
  }
  
  # Select only relevant variables (DV and IVs)
  vars_needed <- c(dv_name, iv_names)
  clean_data <- clean_data[, vars_needed, drop = FALSE]
  
  # Remove rows with missing values on analysis variables
  complete_cases <- complete.cases(clean_data)
  report$removed_rows <- sum(!complete_cases)
  clean_data <- clean_data[complete_cases, ]
  
  report$final_rows <- nrow(clean_data)
  report$complete_case_rate <- report$final_rows / report$original_rows
  
  return(list(
    data = clean_data,
    report = report
  ))
}