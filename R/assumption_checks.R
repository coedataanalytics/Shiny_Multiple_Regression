# R/assumption_checks.R
# Purpose: Test all regression assumptions
# Why: Valid regression inferences require meeting assumptions
# Each assumption gets its own function for modularity and testing

#' Test Linearity Assumption
#' @param model An lm object
#' @param data The original data frame
#' @return List with test results and diagnostic plots
#' @export
check_linearity <- function(model, data = NULL) {
  
  # Method 1: Component + Residual Plots (Partial Residual Plots)
  # This is the gold standard for linearity in multiple regression
  
  # Get model frame
  mf <- model.frame(model)
  dv_name <- names(mf)[1]
  
  # For simple regression, use simple scatter
  iv_names <- attr(terms(model), "term.labels")
  
  # Create component + residual plot data
  cr_plots <- map(iv_names, function(iv) {
    
    # Get the coefficient
    coef_val <- coef(model)[iv]
    
    # Get residuals
    resid <- residuals(model)
    
    # Component
    component <- coef_val * mf[[iv]]
    
    # Partial residual = residual + component
    partial_resid <- resid + component
    
    tibble(
      iv = iv,
      iv_value = mf[[iv]],
      partial_residual = partial_resid,
      component = component
    )
  })
  
  cr_data <- bind_rows(cr_plots)
  
  # Create plot
  cr_plot <- ggplot(cr_data, aes(x = iv_value, y = partial_residual)) +
    geom_point(alpha = 0.5, color = "#1B365D") +
    geom_smooth(method = "loess", se = TRUE, color = "#C41E3A", 
                fill = "#C41E3A", alpha = 0.2) +
    geom_abline(slope = coef(model)[1], intercept = 0, 
                linetype = "dashed", color = "#6B8E8E") +
    facet_wrap(~ iv, scales = "free") +
    labs(
      title = "Component + Residual Plots (Linearity Check)",
      subtitle = "Red line should be approximately linear if assumption is met",
      x = "Predictor Value",
      y = "Partial Residual"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
  
  # Rainbow test (nonlinearity test)
  # Requires lmtest package
  rainbow_test <- tryCatch({
    lmtest::raintest(model)
  }, error = function(e) {
    list(statistic = NA, p.value = NA, method = "Rainbow test failed")
  })
  
  return(list(
    plot = cr_plot,
    rainbow_test = list(
      statistic = rainbow_test$statistic,
      p_value = rainbow_test$p.value,
      interpretation = ifelse(
        rainbow_test$p.value > .05,
        "No significant nonlinearity detected (p > .05)",
        "Significant nonlinearity detected (p < .05) - consider transformations or polynomial terms"
      )
    ),
    passed = rainbow_test$p.value > .05
  ))
}

#' Test Normality of Residuals
#' @param model An lm object
#' @return List with test results and plots
#' @export
check_normality <- function(model) {
  
  residuals <- residuals(model)
  standardized <- rstandard(model)
  
  # Remove NA values
  residuals <- residuals[!is.na(residuals)]
  standardized <- standardized[!is.na(standardized)]
  
  n <- length(residuals)
  
  # Shapiro-Wilk test (for n < 5000)
  shapiro_test <- if (n >= 3 && n <= 5000) {
    tryCatch({
      shapiro.test(residuals)
    }, error = function(e) {
      list(statistic = NA, p.value = NA, method = "Shapiro-Wilk failed")
    })
  } else {
    list(statistic = NA, p.value = NA, 
         method = "Shapiro-Wilk not applicable (n > 5000)")
  }
  
  # Kolmogorov-Smirnov test (with warning about ties)
  ks_test <- tryCatch({
    ks.test(residuals, "pnorm", 
            mean = mean(residuals), 
            sd = sd(residuals))
  }, warning = function(w) {
    # Ties present - still return result but note the warning
    list(statistic = NA, p.value = NA, 
         method = "K-S test: ties present in data")
  }, error = function(e) {
    list(statistic = NA, p.value = NA, method = "K-S test failed")
  })
  
  # Q-Q Plot data (return data, not plot object, to avoid conflicts)
  qq_data <- data.frame(
    theoretical = qnorm(ppoints(n)),
    sample = sort(standardized)
  )
  
  # Histogram data
  hist_data <- data.frame(residuals = residuals)
  
  return(list(
    qq_data = qq_data,
    hist_data = hist_data,
    shapiro_wilk = list(
      statistic = shapiro_test$statistic,
      p_value = shapiro_test$p.value,
      interpretation = ifelse(
        !is.na(shapiro_test$p.value) && shapiro_test$p.value > .05,
        "Residuals appear normally distributed (p > .05)",
        "Residuals significantly deviate from normal (p < .05)"
      )
    ),
    kolmogorov_smirnov = list(
      statistic = ks_test$statistic,
      p_value = ks_test$p.value,
      interpretation = ifelse(
        !is.na(ks_test$p.value) && ks_test$p.value > .05,
        "K-S test: no significant deviation from normal (p > .05)",
        "K-S test: significant deviation (p < .05)"
      )
    ),
    skewness = tryCatch(moments::skewness(residuals), error = function(e) NA),
    kurtosis = tryCatch(moments::kurtosis(residuals) - 3, error = function(e) NA),
    passed = !is.na(shapiro_test$p.value) && shapiro_test$p.value > .05
  ))
}

#' Test Homoscedasticity (Constant Variance)
#' @param model An lm object
#' @return List with test results and plots
#' @export
check_homoscedasticity <- function(model) {
  
  fitted_vals <- fitted(model)
  residuals <- residuals(model)
  standardized <- rstandard(model)
  
  # Breusch-Pagan test
  bp_test <- lmtest::bptest(model)
  
  # NCV test (Non-Constant Variance)
  ncv_test <- car::ncvTest(model)
  
  # Residuals vs Fitted plot
  rvf_plot <- ggplot(data.frame(fitted = fitted_vals, 
                                resid = standardized), 
                     aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.5, color = "#1B365D") +
    geom_smooth(method = "loess", se = FALSE, color = "#C41E3A", 
                linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#6B8E8E") +
    labs(
      title = "Residuals vs. Fitted Values",
      subtitle = "Red line should be flat and horizontal if homoscedasticity holds",
      x = "Fitted Values",
      y = "Standardized Residuals"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  # Scale-Location plot (Spread-Location)
  sl_plot <- ggplot(data.frame(fitted = fitted_vals, 
                               sqrt_resid = sqrt(abs(standardized))),
                    aes(x = fitted, y = sqrt_resid)) +
    geom_point(alpha = 0.5, color = "#1B365D") +
    geom_smooth(method = "loess", se = FALSE, color = "#C41E3A", 
                linewidth = 1) +
    labs(
      title = "Scale-Location Plot",
      subtitle = "Flat line indicates constant variance",
      x = "Fitted Values",
      y = expression(sqrt("|Standardized Residuals|"))
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  return(list(
    rvf_plot = rvf_plot,
    scale_location_plot = sl_plot,
    breusch_pagan = list(
      statistic = bp_test$statistic,
      p_value = bp_test$p.value,
      interpretation = ifelse(
        bp_test$p.value > .05,
        "Homoscedasticity assumption met (p > .05)",
        "Heteroscedasticity detected (p < .05) - consider robust standard errors"
      )
    ),
    ncv_test = list(
      statistic = ncv_test$test,
      p_value = ncv_test$p,
      interpretation = ifelse(
        ncv_test$p > .05,
        "Non-constant variance test: assumption met (p > .05)",
        "Non-constant variance detected (p < .05)"
      )
    ),
    passed = bp_test$p.value > .05
  ))
}

#' Test Independence of Residuals (Durbin-Watson)
#' @param model An lm object
#' @return List with test results
#' @export
check_independence <- function(model) {
  
  # Durbin-Watson test
  dw_test <- lmtest::dwtest(model)
  
  # Run test for negative autocorrelation too
  dw_test_alt <- lmtest::dwtest(model, alternative = "two.sided")
  
  return(list(
    durbin_watson = list(
      statistic = dw_test$statistic,
      p_value = dw_test$p.value,
      interpretation = ifelse(
        dw_test$p.value > .05,
        "No autocorrelation detected (p > .05) - independence assumption met",
        "Autocorrelation detected (p < .05) - consider time series methods"
      )
    ),
    passed = dw_test$p.value > .05
  ))
}

#' Check Multicollinearity
#' @param model An lm object
#' @return List with VIF values and interpretation
#' @export
check_multicollinearity <- function(model) {
  
  # Get VIF values
  vif_values <- tryCatch({
    car::vif(model)
  }, error = function(e) {
    # VIF can't be calculated for simple regression
    return(NULL)
  })
  
  if (is.null(vif_values)) {
    return(list(
      vif = NULL,
      message = "VIF not applicable for simple regression (only one predictor)",
      passed = TRUE
    ))
  }
  
  # Handle different VIF output formats
  if (is.matrix(vif_values)) {
    vif_df <- as.data.frame(vif_values) %>%
      rownames_to_column("variable") %>%
      rename(vif = GVIF)
  } else {
    vif_df <- tibble(
      variable = names(vif_values),
      vif = as.numeric(vif_values)
    )
  }
  
  # Add interpretation
  vif_df <- vif_df %>%
    mutate(
      severity = case_when(
        vif < 5 ~ "Low",
        vif >= 5 & vif < 10 ~ "Moderate",
        vif >= 10 ~ "High",
        TRUE ~ "Unknown"
      ),
      interpretation = case_when(
        vif < 5 ~ "Acceptable",
        vif >= 5 & vif < 10 ~ "Consider remediation",
        vif >= 10 ~ "Problematic - consider removing or combining variables",
        TRUE ~ ""
      )
    )
  
  # Overall assessment
  max_vif <- max(vif_df$vif, na.rm = TRUE)
  
  return(list(
    vif_table = vif_df,
    max_vif = max_vif,
    overall_interpretation = ifelse(
      max_vif < 5,
      "No multicollinearity concerns (all VIF < 5)",
      ifelse(
        max_vif < 10,
        "Moderate multicollinearity detected (some VIF >= 5)",
        "Severe multicollinearity detected (VIF >= 10)"
      )
    ),
    passed = max_vif < 10
  ))
}

#' Check for Influential Observations
#' @param model An lm object
#' @param threshold Criterion for flagging ("standard" or "conservative")
#' @return List with influential observation indices and plots
#' @export
check_influential <- function(model, threshold = "standard") {
  
  # Get influence measures
  influence_df <- influence.measures(model)$infmat %>%
    as.data.frame() %>%
    rownames_to_column("row_num") %>%
    mutate(row_num = as.integer(row_num))
  
  # Cook's distance
  cooks_d <- cooks.distance(model)
  n <- length(cooks_d)
  
  # Threshold for Cook's D: 4/n or 4/(n-k-1)
  k <- length(coef(model)) - 1
  cooks_threshold <- if (threshold == "conservative") {
    4 / (n - k - 1)
  } else {
    4 / n
  }
  
  # Leverage (hat values)
  hat_vals <- hatvalues(model)
  leverage_threshold <- 2 * (k + 1) / n
  
  # DFBETAS
  dfbetas_vals <- dfbetas(model)
  
  # Combine into summary
  influence_summary <- tibble(
    row_num = 1:n,
    cooks_d = cooks_d,
    leverage = hat_vals,
    standardized_residual = rstandard(model),
    studentized_residual = rstudent(model)
  ) %>%
    mutate(
      influential_cooks = cooks_d > cooks_threshold,
      influential_leverage = leverage > leverage_threshold,
      influential_residual = abs(standardized_residual) > 3,
      is_influential = influential_cooks | influential_leverage | influential_residual
    )
  
  # Cook's distance plot
  cooks_plot <- ggplot(influence_summary, aes(x = row_num, y = cooks_d)) +
    geom_col(fill = "#1B365D", alpha = 0.7) +
    geom_hline(yintercept = cooks_threshold, color = "#C41E3A", 
               linetype = "dashed", linewidth = 1) +
    geom_text(
      data = filter(influence_summary, influential_cooks),
      aes(label = row_num), 
      vjust = -0.5, color = "#C41E3A", size = 3
    ) +
    labs(
      title = "Cook's Distance",
      subtitle = paste0("Threshold = ", round(cooks_threshold, 4), 
                        " (red dashed line)"),
      x = "Observation Number",
      y = "Cook's Distance"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  # Leverage vs Residuals squared plot
  leverage_plot <- ggplot(
    influence_summary, 
    aes(x = leverage, y = standardized_residual^2)
  ) +
    geom_point(alpha = 0.5, color = "#1B365D") +
    geom_vline(xintercept = leverage_threshold, color = "#C41E3A", 
               linetype = "dashed") +
    geom_hline(yintercept = 4, color = "#C41E3A", linetype = "dashed") +
    labs(
      title = "Leverage vs. Squared Residuals",
      subtitle = "Points in upper-right corner are most influential",
      x = "Leverage (Hat Value)",
      y = "Squared Standardized Residual"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  # Return results
  influential_rows <- influence_summary %>%
    filter(is_influential) %>%
    pull(row_num)
  
  return(list(
    cooks_plot = cooks_plot,
    leverage_plot = leverage_plot,
    influence_summary = influence_summary,
    influential_rows = influential_rows,
    n_influential = length(influential_rows),
    cooks_threshold = cooks_threshold,
    leverage_threshold = leverage_threshold,
    passed = length(influential_rows) == 0
  ))
}

#' Run all assumption checks
#' @param model An lm object
#' @param data Original data (for linearity check)
#' @return List with all assumption check results
#' @export
check_all_assumptions <- function(model, data = NULL) {
  
  message("Checking linearity...")
  linearity <- check_linearity(model, data)
  
  message("Checking normality...")
  normality <- check_normality(model)
  
  message("Checking homoscedasticity...")
  homoscedasticity <- check_homoscedasticity(model)
  
  message("Checking independence...")
  independence <- check_independence(model)
  
  message("Checking multicollinearity...")
  multicollinearity <- check_multicollinearity(model)
  
  message("Checking influential observations...")
  influential <- check_influential(model)
  
  # Summary table
  summary_table <- tibble(
    Assumption = c("Linearity", "Normality of Residuals", 
                   "Homoscedasticity", "Independence",
                   "Multicollinearity", "No Influential Observations"),
    Status = c(
      ifelse(linearity$passed, "✓ Passed", "✗ Failed"),
      ifelse(normality$passed, "✓ Passed", "✗ Failed"),
      ifelse(homoscedasticity$passed, "✓ Passed", "✗ Failed"),
      ifelse(independence$passed, "✓ Passed", "✗ Failed"),
      ifelse(multicollinearity$passed, "✓ Passed", "✗ Failed"),
      ifelse(influential$passed, "✓ Passed", "✗ Warning")
    ),
    Key_Statistic = c(
      paste0("Rainbow test p = ", round_pretty(linearity$rainbow_test$p_value)),
      paste0("Shapiro-Wilk p = ", round_pretty(normality$shapiro_wilk$p_value)),
      paste0("Breusch-Pagan p = ", round_pretty(homoscedasticity$breusch_pagan$p_value)),
      paste0("Durbin-Watson = ", round_pretty(independence$durbin_watson$statistic)),
      paste0("Max VIF = ", round_pretty(multicollinearity$max_vif)),
      paste0(influential$n_influential, " flagged")
    )
  )
  
  return(list(
    linearity = linearity,
    normality = normality,
    homoscedasticity = homoscedasticity,
    independence = independence,
    multicollinearity = multicollinearity,
    influential = influential,
    summary = summary_table,
    all_passed = all(c(
      linearity$passed,
      normality$passed,
      homoscedasticity$passed,
      independence$passed
    ))
  ))
}