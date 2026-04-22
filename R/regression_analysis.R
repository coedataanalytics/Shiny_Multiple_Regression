# R/regression_analysis.R
# Purpose: Core regression fitting and calculations
# Why: Centralizes all regression logic, including hierarchical models

#' Fit a simple or multiple regression model
#' @param data Data frame
#' @param dv Name of dependent variable
#' @param ivs Character vector of independent variable names
#' @param covariates Optional covariates to control for
#' @return List with model and summary statistics
#' @export
fit_regression <- function(data, dv, ivs, covariates = NULL) {
  
  # Build formula
  predictors <- c(covariates, ivs)
  formula_str <- paste(dv, "~", paste(predictors, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Fit model
  model <- lm(formula, data = data)
  
  # Get summary
  model_summary <- summary(model)
  
  # Tidy output
  tidy_output <- broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01 ~ "**",
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE ~ ""
      )
    )
  
  # Glance (model-level statistics)
  glance_output <- broom::glance(model)
  
  # Augment (observation-level statistics)
  augment_output <- broom::augment(model) %>%
    as_tibble()
  
  return(list(
    model = model,
    formula = formula_str,
    coefficients = tidy_output,
    model_stats = glance_output,
    augmented_data = augment_output,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    f_statistic = model_summary$fstatistic,
    f_pvalue = pf(model_summary$fstatistic[1], 
                  model_summary$fstatistic[2], 
                  model_summary$fstatistic[3], 
                  lower.tail = FALSE),
    n = nrow(data),
    dv = dv,
    ivs = ivs,
    covariates = covariates
  ))
}

#' Fit hierarchical regression models
#' @param data Data frame
#' @param dv Name of dependent variable
#' @param blocks List of character vectors, each containing predictors for that block
#' @param block_names Optional names for each block
#' @return List with all models and change statistics
#' @export
fit_hierarchical_regression <- function(data, dv, blocks, 
                                        block_names = NULL) {
  
  # Validate inputs
  if (!is.list(blocks) || length(blocks) < 2) {
    stop("blocks must be a list with at least 2 elements for hierarchical regression")
  }
  
  # Name blocks if not provided
  if (is.null(block_names)) {
    block_names <- paste0("Block ", 1:length(blocks))
  }
  
  # Fit models progressively
  models <- list()
  cumulative_predictors <- c()
  
  for (i in seq_along(blocks)) {
    cumulative_predictors <- c(cumulative_predictors, blocks[[i]])
    
    formula_str <- paste(dv, "~", paste(cumulative_predictors, collapse = " + "))
    
    models[[i]] <- list(
      name = block_names[i],
      formula = formula_str,
      predictors = cumulative_predictors,
      new_predictors = blocks[[i]],
      model = lm(as.formula(formula_str), data = data)
    )
    
    # Add summary statistics
    models[[i]]$summary <- summary(models[[i]]$model)
    models[[i]]$r_squared <- models[[i]]$summary$r.squared
    models[[i]]$adj_r_squared <- models[[i]]$summary$adj.r.squared
    models[[i]]$tidy <- broom::tidy(models[[i]]$model, conf.int = TRUE)
    models[[i]]$glance <- broom::glance(models[[i]]$model)
  }
  
  # Calculate change statistics
  change_stats <- tibble(
    block = block_names,
    r_squared = map_dbl(models, ~ .$r_squared),
    adj_r_squared = map_dbl(models, ~ .$adj_r_squared),
    r_squared_change = c(models[[1]]$r_squared, 
                         diff(map_dbl(models, ~ .$r_squared))),
    df_model = map_dbl(models, ~ .$summary$df[1]),
    df_residual = map_dbl(models, ~ .$summary$df[2])
  )
  
  # Calculate F-change and p-values for each step
  f_change <- c()
  p_change <- c()
  
  for (i in seq_along(models)) {
    if (i == 1) {
      # First model - just the overall F
      f_change <- c(f_change, models[[i]]$summary$fstatistic[1])
      p_change <- c(p_change, 
                    pf(models[[i]]$summary$fstatistic[1],
                       models[[i]]$summary$fstatistic[2],
                       models[[i]]$summary$fstatistic[3],
                       lower.tail = FALSE))
    } else {
      # Compare to previous model
      anova_result <- anova(models[[i-1]]$model, models[[i]]$model)
      f_change <- c(f_change, anova_result$F[2])
      p_change <- c(p_change, anova_result$`Pr(>F)`[2])
    }
  }
  
  change_stats <- change_stats %>%
    mutate(
      f_change = f_change,
      p_change = p_change,
      sig = case_when(
        p_change < .001 ~ "***",
        p_change < .01 ~ "**",
        p_change < .05 ~ "*",
        p_change < .10 ~ ".",
        TRUE ~ ""
      )
    )
  
  # Create ANOVA table for model comparison
  anova_table <- map(models, ~ .$model) %>%
    do.call(anova, .) %>%
    broom::tidy()
  
  # Effect sizes (Cohen's f²) for each step
  cohen_f2 <- c()
  for (i in seq_along(models)) {
    if (i == 1) {
      r2 <- models[[i]]$r_squared
    } else {
      r2 <- models[[i]]$r_squared - models[[i-1]]$r_squared
    }
    f2 <- r2 / (1 - models[[i]]$r_squared)  # Using current model R²
    cohen_f2 <- c(cohen_f2, f2)
  }
  
  change_stats <- change_stats %>%
    mutate(cohen_f2 = cohen_f2)
  
  # Compile results
  results <- list(
    models = models,
    change_statistics = change_stats,
    anova_table = anova_table,
    final_model = models[[length(models)]],
    n_predictors_final = length(cumulative_predictors),
    dv = dv,
    blocks = blocks,
    block_names = block_names,
    n = nrow(data)
  )
  
  class(results) <- c("hierarchical_regression", "list")
  
  return(results)
}

#' Print method for hierarchical regression
#' @export
print.hierarchical_regression <- function(x, ...) {
  cat("\n========================================\n")
  cat("HIERARCHICAL REGRESSION RESULTS\n")
  cat("========================================\n\n")
  
  cat("Dependent Variable:", x$dv, "\n")
  cat("Sample Size:", x$n, "\n")
  cat("Number of Blocks:", length(x$models), "\n\n")
  
  cat("--- Model Change Statistics ---\n\n")
  print(x$change_statistics)
  
  cat("\n--- Final Model Coefficients ---\n\n")
  print(x$final_model$tidy)
  
  invisible(x)
}

#' Get standardized coefficients (beta weights)
#' @param model An lm object
#' @param data Original data
#' @return Tibble with standardized coefficients
#' @export
standardized_coefficients <- function(model, data) {
  
  # Get model terms
  mf <- model.frame(model)
  dv_name <- names(mf)[1]
  iv_names <- attr(terms(model), "term.labels")
  
  # Standardize variables
  std_data <- mf %>%
    mutate(across(where(is.numeric), ~ scale(.)[,1]))
  
  # Fit standardized model
  std_formula <- as.formula(paste(dv_name, "~", paste(iv_names, collapse = " + ")))
  std_model <- lm(std_formula, data = std_data)
  
  # Extract standardized coefficients
  std_coefs <- broom::tidy(std_model) %>%
    filter(term != "(Intercept)") %>%
    rename(beta = estimate,
           std_error = std.error) %>%
    select(term, beta, std_error, statistic, p.value)
  
  return(std_coefs)
}

#' Calculate semi-partial (part) correlations
#' @param model An lm object
#' @param data Original data
#' @return Tibble with semi-partial correlations
#' @export
semipartial_correlations <- function(model, data) {
  
  iv_names <- attr(terms(model), "term.labels")
  dv_name <- names(model.frame(model))[1]
  
  # Get R² for full model
  full_r2 <- summary(model)$r.squared
  
  # Calculate semi-partial for each predictor
  sp_corrs <- map_dfr(iv_names, function(iv) {
    # Fit model without this predictor
    other_ivs <- setdiff(iv_names, iv)
    
    if (length(other_ivs) == 0) {
      # Only one predictor - semi-partial equals correlation
      reduced_r2 <- 0
    } else {
      reduced_formula <- as.formula(paste(dv_name, "~", paste(other_ivs, collapse = " + ")))
      reduced_model <- lm(reduced_formula, data = data)
      reduced_r2 <- summary(reduced_model)$r.squared
    }
    
    sr <- sqrt(full_r2 - reduced_r2)  # Semi-partial correlation
    sr2 <- full_r2 - reduced_r2       # Semi-partial R² (unique variance)
    
    tibble(
      term = iv,
      semi_partial_r = sr,
      semi_partial_r2 = sr2
    )
  })
  
  return(sp_corrs)
}

#' Calculate standardized coefficients (beta weights)
#' @param model An lm object
#' @param data Original data frame
#' @return Tibble with standardized coefficients
#' @export
standardized_coefficients <- function(model, data) {
  
  # Get model terms
  mf <- model.frame(model)
  dv_name <- names(mf)[1]
  iv_names <- attr(terms(model), "term.labels")
  
  # Guard: need at least one predictor
  if (length(iv_names) == 0) {
    return(tibble(
      term = character(0),
      beta = numeric(0),
      std_error = numeric(0)
    ))
  }
  
  # Standardize all numeric variables in model frame
  std_data <- mf %>%
    mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
  
  # Fit standardized model
  std_formula <- as.formula(paste(dv_name, "~", paste(iv_names, collapse = " + ")))
  std_model <- tryCatch({
    lm(std_formula, data = std_data)
  }, error = function(e) {
    warning("Could not fit standardized model: ", e$message)
    return(NULL)
  })
  
  if (is.null(std_model)) {
    return(NULL)
  }
  
  # Extract standardized coefficients
  std_coefs <- broom::tidy(std_model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    rename(beta = estimate,
           std_error = std.error,
           conf.low = conf.low,
           conf.high = conf.high) %>%
    select(term, beta, std_error, conf.low, conf.high)
  
  return(std_coefs)
}

#' Calculate partial correlations for each predictor
#' @param model An lm object
#' @param data Original data frame
#' @return Tibble with partial correlations
#' @export
partial_correlations <- function(model, data) {
  
  iv_names <- attr(terms(model), "term.labels")
  dv_name <- all.vars(formula(model))[1]
  
  if (length(iv_names) == 0) {
    return(tibble(
      term = character(0),
      partial_r = numeric(0)
    ))
  }
  
  # Get t-values from model summary
  model_summary <- summary(model)
  coef_table <- model_summary$coefficients
  
  # Calculate partial r from t-statistic
  # Formula: partial_r = t / sqrt(t² + df_residual)
  df_residual <- model_summary$df[2]
  
  partial_r <- map_dfr(iv_names, function(iv) {
    t_val <- coef_table[iv, "t value"]
    partial_r_val <- t_val / sqrt(t_val^2 + df_residual)
    
    tibble(
      term = iv,
      partial_r = partial_r_val
    )
  })
  
  return(partial_r)
}

#' Calculate semi-partial (part) correlations
#' @param model An lm object
#' @param data Original data frame
#' @return Tibble with semi-partial correlations and sr²
#' @export
semipartial_correlations <- function(model, data) {
  
  iv_names <- attr(terms(model), "term.labels")
  dv_name <- all.vars(formula(model))[1]
  
  if (length(iv_names) == 0) {
    return(tibble(
      term = character(0),
      semi_partial_r = numeric(0),
      sr_squared = numeric(0)
    ))
  }
  
  # Get R² for full model
  full_r2 <- summary(model)$r.squared
  
  # Calculate semi-partial for each predictor
  sp_corrs <- map_dfr(iv_names, function(iv) {
    # Fit model without this predictor
    other_ivs <- setdiff(iv_names, iv)
    
    if (length(other_ivs) == 0) {
      # Only one predictor - semi-partial equals zero-order correlation
      reduced_r2 <- 0
    } else {
      reduced_formula <- as.formula(paste(dv_name, "~", 
                                          paste(other_ivs, collapse = " + ")))
      reduced_model <- tryCatch({
        lm(reduced_formula, data = data)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(reduced_model)) {
        reduced_r2 <- 0
      } else {
        reduced_r2 <- summary(reduced_model)$r.squared
      }
    }
    
    # Semi-partial correlation (sr)
    sr <- sqrt(max(0, full_r2 - reduced_r2))  # Protect against negative due to rounding
    
    # Determine sign from coefficient
    coef_sign <- sign(coef(model)[iv])
    sr <- sr * coef_sign
    
    # Unique variance (sr²)
    sr2 <- full_r2 - reduced_r2
    
    tibble(
      term = iv,
      semi_partial_r = sr,
      sr_squared = sr2
    )
  })
  
  return(sp_corrs)
}

#' Get comprehensive coefficient table with all statistics
#' @param model An lm object
#' @param data Original data frame
#' @return Tibble with B, SE, β, t, p, CI, pr, sr, sr²
#' @export
comprehensive_coefficient_table <- function(model, data) {
  
  # Get standard coefficient table
  std_coef <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    rename(b = estimate,
           se = std.error,
           t = statistic,
           p = p.value) %>%
    select(term, b, se, t, p, conf.low, conf.high)
  
  # Get standardized coefficients
  beta_coefs <- standardized_coefficients(model, data)
  
  # Get partial correlations
  partial_r <- partial_correlations(model, data)
  
  # Get semi-partial correlations
  semi_partial <- semipartial_correlations(model, data)
  
  # Combine all statistics
  if (!is.null(beta_coefs) && nrow(beta_coefs) > 0) {
    std_coef <- std_coef %>%
      left_join(beta_coefs %>% select(term, beta), by = "term")
  } else {
    std_coef$beta <- NA
  }
  
  if (!is.null(partial_r) && nrow(partial_r) > 0) {
    std_coef <- std_coef %>%
      left_join(partial_r, by = "term")
  } else {
    std_coef$partial_r <- NA
  }
  
  if (!is.null(semi_partial) && nrow(semi_partial) > 0) {
    std_coef <- std_coef %>%
      left_join(semi_partial, by = "term")
  } else {
    std_coef$semi_partial_r <- NA
    std_coef$sr_squared <- NA
  }
  
  # Add significance stars
  std_coef <- std_coef %>%
    mutate(
      sig = case_when(
        p < .001 ~ "***",
        p < .01 ~ "**",
        p < .05 ~ "*",
        p < .10 ~ ".",
        TRUE ~ ""
      )
    )
  
  return(std_coef)
}