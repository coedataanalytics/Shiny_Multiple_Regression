# R/descriptive_stats.R
# Purpose: Calculate descriptive statistics for the dataset
# Why: Provides foundation for understanding data before modeling

#' Calculate descriptive statistics for continuous variables
#' @param data A data frame
#' @param vars Character vector of variable names
#' @return Tibble of descriptive statistics
#' @export
descriptive_continuous <- function(data, vars) {
  
  if (length(vars) == 0) return(NULL)
  
  results <- map_dfr(vars, function(var) {
    x <- data[[var]]
    
    tibble(
      variable = var,
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      se = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
      min = min(x, na.rm = TRUE),
      q25 = quantile(x, .25, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      q75 = quantile(x, .75, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      skewness = moments::skewness(x, na.rm = TRUE),
      kurtosis = moments::kurtosis(x, na.rm = TRUE) - 3  # Excess kurtosis
    )
  })
  
  return(results)
}

#' Calculate frequency tables for categorical variables
#' @param data A data frame
#' @param vars Character vector of variable names
#' @return List of frequency tables
#' @export
descriptive_categorical <- function(data, vars) {
  
  if (length(vars) == 0) return(NULL)
  
  results <- map(vars, function(var) {
    x <- data[[var]]
    freq_table <- table(x, useNA = "ifany") %>%
      as.data.frame() %>%
      mutate(
        percent = Freq / sum(Freq) * 100,
        cum_percent = cumsum(percent)
      ) %>%
      rename(!!var := x, frequency = Freq)
    
    return(freq_table)
  })
  
  names(results) <- vars
  return(results)
}

#' Calculate correlation matrix with significance
#' @param data A data frame
#' @param vars Character vector of variable names
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return List with correlation matrix and p-value matrix
#' @export
correlation_matrix <- function(data, vars, method = "pearson") {
  
  if (length(vars) < 2) return(NULL)
  
  # Select only numeric variables
  numeric_vars <- vars[sapply(data[vars], is.numeric)]
  
  if (length(numeric_vars) < 2) {
    return(list(
      correlations = NULL,
      p_values = NULL,
      n = 0,
      message = "Need at least 2 numeric variables"
    ))
  }
  
  # Create correlation matrix
  cor_data <- data[numeric_vars] %>% drop_na()
  n <- nrow(cor_data)
  
  cor_matrix <- cor(cor_data, method = method)
  
  # Calculate p-values
  p_matrix <- matrix(NA, nrow = length(numeric_vars), 
                     ncol = length(numeric_vars),
                     dimnames = dimnames(cor_matrix))
  
  for (i in 1:(length(numeric_vars) - 1)) {
    for (j in (i + 1):length(numeric_vars)) {
      test <- cor.test(cor_data[[i]], cor_data[[j]], method = method)
      p_matrix[i, j] <- test$p.value
      p_matrix[j, i] <- test$p.value
    }
  }
  diag(p_matrix) <- 0
  
  return(list(
    correlations = cor_matrix,
    p_values = p_matrix,
    n = n,
    variables = numeric_vars
  ))
}

#' Generate comprehensive descriptive report
#' @param data A data frame
#' @param dv Name of dependent variable
#' @param ivs Names of independent variables
#' @return List with all descriptive statistics
#' @export
generate_descriptive_report <- function(data, dv, ivs) {
  
  all_vars <- c(dv, ivs)
  
  # Separate continuous and categorical
  var_types <- detect_variable_types(data[all_vars])
  
  continuous_vars <- var_types %>%
    filter(suggested_type == "continuous") %>%
    pull(variable)
  
  categorical_vars <- var_types %>%
    filter(suggested_type == "categorical") %>%
    pull(variable)
  
  list(
    continuous = descriptive_continuous(data, continuous_vars),
    categorical = descriptive_categorical(data, categorical_vars),
    correlations = correlation_matrix(data, continuous_vars),
    variable_types = var_types,
    missing_summary = map_dfr(all_vars, ~ tibble(
      variable = .x,
      n_missing = sum(is.na(data[[.x]])),
      pct_missing = mean(is.na(data[[.x]])) * 100
    ))
  )
}