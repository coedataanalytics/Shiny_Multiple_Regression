# R/plotting_functions.R
# Purpose: All visualization functions
# Why: Centralizes plotting logic, enables consistent APA styling

#' Florida Tech-themed theme for ggplot2
#' @return ggplot2 theme object
#' @export
theme_ft <- function() {
  theme_minimal(base_size = 12) +
    theme(
      # Title and labels
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, 
                                color = "#8B0000"),  # Burgundy titles
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#2C2C2C"),
      
      # Axis
      axis.title = element_text(face = "bold", size = 11, color = "#2C2C2C"),
      axis.text = element_text(size = 10, color = "#2C2C2C"),
      axis.line = element_line(color = "#8B0000", linewidth = 0.5),
      axis.ticks = element_line(color = "#8B0000"),
      
      # Legend
      legend.position = "right",
      legend.title = element_text(face = "bold", color = "#2C2C2C"),
      legend.background = element_rect(fill = "#F5F3F0", color = "#D8D4D0"),
      
      # Panel
      panel.grid.major = element_line(color = "#E8E4E0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.border = element_rect(fill = NA, color = "#8B0000", linewidth = 0.5),
      
      # Facets
      strip.text = element_text(face = "bold", size = 10, color = "#FFFFFF"),
      strip.background = element_rect(fill = "#8B0000", color = "#6D0000")
    )
}

# Keep theme_apa as alias for backward compatibility
theme_apa <- theme_ft

#' Create scatter plot with regression line
#' @param data Data frame
#' @param x_var Name of x variable
#' @param y_var Name of y variable
#' @param show_ci Logical, show confidence band
#' @param show_pi Logical, show prediction band
#' @param interactive Logical, return plotly object
#' @return ggplot or plotly object
#' @export
scatter_with_regression <- function(data, x_var, y_var, 
                                    show_ci = TRUE, 
                                    show_pi = FALSE,
                                    interactive = TRUE) {
  
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.6, color = "#8B0000", size = 2) +
    geom_smooth(method = "lm", 
                se = show_ci, 
                color = "#C5A95F", 
                fill = "#C5A95F", 
                alpha = 0.2) +
    labs(
      title = paste0(y_var, " vs. ", x_var),
      x = x_var,
      y = y_var
    ) +
    theme_apa()
  
  # Add prediction interval if requested
  if (show_pi) {
    model <- lm(as.formula(paste(y_var, "~", x_var)), data = data)
    pred_data <- data[[x_var]]
    pred_interval <- predict(model, 
                             newdata = data.frame(x_var = pred_data),
                             interval = "prediction")
    pred_df <- data.frame(
      x = pred_data,
      lwr = pred_interval[, "lwr"],
      upr = pred_interval[, "upr"]
    )
    
    p <- p +
      geom_ribbon(data = pred_df, 
                  aes(x = x, ymin = lwr, ymax = upr),
                  inherit.aes = FALSE,
                  alpha = 0.1, fill = "#6B8E8E")
  }
  
  if (interactive) {
    return(ggplotly(p, tooltip = c("x", "y")))
  }
  
  return(p)
}

#' Create partial regression plots (Added Variable Plots)
#' @param model An lm object
#' @param interactive Logical, return plotly object
#' @return ggplot or plotly object
#' @export
partial_regression_plots <- function(model, interactive = TRUE) {
  
  # Use car's avPlots data
  av_data <- car::avPlots(model, plot = FALSE)
  
  plots <- map(names(av_data), function(var_name) {
    
    df <- as.data.frame(av_data[[var_name]])
    names(df) <- c("partial_x", "partial_y")
    
    # Calculate correlation for annotation
    r <- cor(df$partial_x, df$partial_y)
    
    p <- ggplot(df, aes(x = partial_x, y = partial_y)) +
      geom_point(alpha = 0.6, color = "#8B0000", size = 2) +
      geom_smooth(method = "lm", se = TRUE, 
                  color = "#C5A95F", fill = "#C5A95F", alpha = 0.2) +
      labs(
        title = var_name,
        subtitle = paste0("Partial r = ", round(r, 3)),
        x = paste("Partial", var_name),
        y = "Partial Y"
      ) +
      theme_apa()
    
    if (interactive) {
      return(ggplotly(p))
    }
    return(p)
  })
  
  names(plots) <- names(av_data)
  return(plots)
}

#' Create residual plots for model diagnostics
#' @param model An lm object
#' @param interactive Logical
#' @return List of plots
#' @export
residual_diagnostic_plots <- function(model, interactive = TRUE) {
  
  augmented <- broom::augment(model) %>%
    as_tibble()
  
  # 1. Residuals vs Fitted
  p1 <- ggplot(augmented, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6, color = "#8B0000") +
    geom_smooth(method = "loess", se = FALSE, color = "#C5A95F") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#6B8E8E") +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_apa()
  
  # 2. Q-Q Plot
  p2 <- ggplot(augmented, aes(sample = .resid)) +
    stat_qq(alpha = 0.6, color = "#1B365D") +
    stat_qq_line(color = "#C41E3A") +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Residuals"
    ) +
    theme_apa()
  
  # 3. Scale-Location
  p3 <- ggplot(augmented, aes(x = .fitted, y = sqrt(abs(.resid)))) +
    geom_point(alpha = 0.6, color = "#1B365D") +
    geom_smooth(method = "loess", se = FALSE, color = "#C41E3A") +
    labs(
      title = "Scale-Location",
      x = "Fitted Values",
      y = expression(sqrt("|Residuals|"))
    ) +
    theme_apa()
  
  # 4. Residuals vs Leverage
  p4 <- ggplot(augmented, aes(x = .hat, y = .resid)) +
    geom_point(alpha = 0.6, color = "#1B365D") +
    geom_smooth(method = "loess", se = FALSE, color = "#C41E3A") +
    labs(
      title = "Residuals vs Leverage",
      x = "Leverage (Hat)",
      y = "Residuals"
    ) +
    theme_apa()
  
  plots <- list(
    residuals_vs_fitted = p1,
    qq_plot = p2,
    scale_location = p3,
    residuals_vs_leverage = p4
  )
  
  if (interactive) {
    plots <- map(plots, ggplotly)
  }
  
  return(plots)
}

#' Create coefficient plot with option for standardized or unstandardized
#' @param model An lm object
#' @param data Original data frame
#' @param type "unstandardized" or "standardized"
#' @param interactive Logical
#' @return ggplot or plotly object
#' @export
coefficient_plot <- function(model, data, type = "unstandardized", 
                             interactive = TRUE) {
  
  # Get comprehensive coefficients
  coef_data <- comprehensive_coefficient_table(model, data)
  
  # Guard: check we have data
  if (is.null(coef_data) || nrow(coef_data) == 0) {
    stop("No coefficient data available for plotting")
  }
  
  # Set coefficient type label FIRST (before mutating)
  coef_type_label <- ifelse(type == "standardized", 
                            "Standardized (β)", 
                            "Unstandardized (B)")
  
  # Select appropriate coefficient based on type
  if (type == "standardized" && "beta" %in% names(coef_data)) {
    coef_data <- coef_data %>%
      mutate(
        coef_value = beta
      )
  } else {
    coef_data <- coef_data %>%
      mutate(
        coef_value = b
      )
  }
  
  # Remove rows with NA coefficient values
  coef_data <- coef_data %>%
    filter(!is.na(coef_value))
  
  # Guard: check we still have data after filtering
  if (nrow(coef_data) == 0) {
    stop("No valid coefficient values for plotting after removing NA values")
  }
  
  # Order by coefficient magnitude
  coef_data <- coef_data %>%
    mutate(Predictor = fct_reorder(term, coef_value))
  
  # Build plot with explicit aes mapping
  p <- ggplot(coef_data, aes(x = coef_value, y = Predictor)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.3, color = "#6B8E8E", linewidth = 0.8) +
    geom_point(size = 4, color = "#1B365D") +
    labs(
      title = "Regression Coefficients",
      subtitle = paste0(coef_type_label, " | Points = estimates, lines = 95% CI"),
      x = ifelse(type == "standardized", 
                 "Standardized Coefficient (β)", 
                 "Unstandardized Coefficient (B)"),
      y = "Predictor"
    ) +
    theme_apa() +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    )
  
  # Add significance stars only if we have significant predictors
  coef_data_sig <- coef_data %>%
    filter(!is.na(p) & p < .05)
  
  if (nrow(coef_data_sig) > 0) {
    p <- p +
      geom_text(data = coef_data_sig,
                aes(label = "*", x = coef_value),
                vjust = 0.5, hjust = -0.8,
                size = 6, color = "#C41E3A")
  }
  
  if (interactive) {
    return(ggplotly(p, tooltip = c("x", "y")))
  }
  
  return(p)
}

#' Create hierarchical regression comparison plot
#' @param hier_result Result from fit_hierarchical_regression
#' @param interactive Logical
#' @return ggplot or plotly object
#' @export
hierarchical_comparison_plot <- function(hier_result, interactive = TRUE) {
  
  plot_data <- hier_result$change_statistics %>%
    mutate(
      block = factor(block, levels = unique(block)),
      r_squared_label = paste0("R² = ", round(r_squared, 3))
    )
  
  p <- ggplot(plot_data, aes(x = block, y = r_squared_change)) +
    geom_col(aes(fill = !is.na(p_change) & p_change < .05), 
             alpha = 0.8, na.rm = TRUE) +
    geom_text(
      aes(label = paste0("ΔR² = ", round(r_squared_change, 3))),
      vjust = -0.5, size = 4, na.rm = TRUE
    ) +
    scale_fill_manual(values = c("TRUE" = "#228B22", "FALSE" = "#6B8E8E"),
                      guide = "none", na.value = "#999999") +
    labs(
      title = "R² Change by Block",
      subtitle = "Green = significant change (p < .05)",
      x = "Model Block",
      y = expression(Delta~R^2)
    ) +
    theme_apa() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (interactive) {
    return(ggplotly(p))
  }
  
  return(p)
}

#' Create correlation matrix heatmap
#' @param cor_matrix Correlation matrix from correlation_matrix()
#' @param interactive Logical
#' @return ggplot or plotly object
#' @export
correlation_heatmap <- function(cor_result, interactive = TRUE) {
  
  if (is.null(cor_result$correlations)) {
    return(NULL)
  }
  
  # Melt correlation matrix
  cor_melt <- cor_result$correlations %>%
    as.data.frame() %>%
    rownames_to_column("var1") %>%
    pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation")
  
  # Create p-value matrix
  p_melt <- cor_result$p_values %>%
    as.data.frame() %>%
    rownames_to_column("var1") %>%
    pivot_longer(cols = -var1, names_to = "var2", values_to = "p_value")
  
  # Combine
  plot_data <- cor_melt %>%
    left_join(p_melt, by = c("var1", "var2")) %>%
    mutate(
      sig = case_when(
        p_value < .001 ~ "***",
        p_value < .01 ~ "**",
        p_value < .05 ~ "*",
        TRUE ~ ""
      ),
      label = paste0(round(correlation, 2), sig)
    )
  
  p <- ggplot(plot_data, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(
      low = "#C41E3A", mid = "white", high = "#1B365D",
      midpoint = 0, limits = c(-1, 1),
      name = "Correlation"
    ) +
    labs(
      title = "Correlation Matrix",
      subtitle = "* p < .05, ** p < .01, *** p < .001",
      x = "",
      y = ""
    ) +
    theme_apa() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(angle = 0),
      legend.position = "right"
    )
  
  if (interactive) {
    return(ggplotly(p, tooltip = c("correlation", "p_value")))
  }
  
  return(p)
}

#' Create partial regression plots (Added Variable Plots)
#' @param model An lm object
#' @param data Original data frame
#' @param interactive Logical, return plotly object
#' @return List of ggplot or plotly objects
#' @export
partial_regression_plots <- function(model, data = NULL, interactive = TRUE) {
  
  # Get predictor names
  iv_names <- attr(terms(model), "term.labels")
  dv_name <- all.vars(formula(model))[1]
  
  if (length(iv_names) == 0) {
    return(list(message = "No predictors in model"))
  }
  
  # Use car package's avPlots data if available
  tryCatch({
    av_data <- car::avPlots(model, plot = FALSE)
    
    plots <- lapply(names(av_data), function(var_name) {
      
      df <- as.data.frame(av_data[[var_name]])
      names(df) <- c("partial_x", "partial_y")
      
      # Calculate partial correlation for annotation
      r <- cor(df$partial_x, df$partial_y, use = "complete.obs")
      
      # Get coefficient significance
      coef_summary <- summary(model)$coefficients
      p_val <- coef_summary[var_name, "Pr(>|t|)"]
      sig_stars <- ifelse(p_val < .001, "***",
                          ifelse(p_val < .01, "**",
                                 ifelse(p_val < .05, "*", "")))
      
      p <- ggplot(df, aes(x = partial_x, y = partial_y)) +
        geom_point(alpha = 0.6, color = "#1B365D", size = 2) +
        geom_smooth(method = "lm", se = TRUE, 
                    color = "#C41E3A", fill = "#C41E3A", alpha = 0.2) +
        geom_abline(slope = 0, intercept = 0, linetype = "dashed", 
                    color = "#6B8E8E", alpha = 0.5) +
        labs(
          title = var_name,
          subtitle = paste0("Partial r = ", round(r, 3), sig_stars),
          x = paste("Partial", var_name),
          y = "Partial Y"
        ) +
        theme_apa()
      
      if (interactive) {
        return(ggplotly(p, tooltip = c("x", "y")))
      }
      return(p)
    })
    
    names(plots) <- iv_names
    return(plots)
    
  }, error = function(e) {
    # Fallback: manual calculation of partial residuals
    message("Using fallback partial residual calculation: ", e$message)
    
    mf <- model.frame(model)
    residuals_full <- residuals(model)
    
    plots <- lapply(iv_names, function(iv) {
      # Fit model without this predictor
      other_ivs <- setdiff(iv_names, iv)
      
      if (length(other_ivs) > 0) {
        reduced_formula <- as.formula(paste(iv, "~", paste(other_ivs, collapse = " + ")))
        reduced_model <- lm(reduced_formula, data = mf)
        partial_x <- residuals(reduced_model)
        
        reduced_dv_formula <- as.formula(paste(dv_name, "~", paste(other_ivs, collapse = " + ")))
        reduced_dv_model <- lm(reduced_dv_formula, data = mf)
        partial_y <- residuals(reduced_dv_model)
      } else {
        partial_x <- mf[[iv]]
        partial_y <- mf[[dv_name]]
      }
      
      df <- data.frame(partial_x = partial_x, partial_y = partial_y)
      r <- cor(df$partial_x, df$partial_y, use = "complete.obs")
      
      p <- ggplot(df, aes(x = partial_x, y = partial_y)) +
        geom_point(alpha = 0.6, color = "#1B365D", size = 2) +
        geom_smooth(method = "lm", se = TRUE, 
                    color = "#C41E3A", fill = "#C41E3A", alpha = 0.2) +
        labs(
          title = iv,
          subtitle = paste0("Partial r = ", round(r, 3)),
          x = paste("Partial", iv),
          y = "Partial Y"
        ) +
        theme_apa()
      
      if (interactive) {
        return(ggplotly(p))
      }
      return(p)
    })
    
    names(plots) <- iv_names
    return(plots)
  })
}

#' Create overall regression fit plot
#' @param model An lm object
#' @param data Original data frame
#' @param dv_name Name of dependent variable
#' @param interactive Logical, return plotly object
#' @return ggplot or plotly object
#' @export
overall_regression_plot <- function(model, data, dv_name, interactive = TRUE) {
  
  # Get fitted values
  fitted_vals <- fitted(model)
  
  # Check number of predictors
  n_predictors <- length(attr(terms(model), "term.labels"))
  
  if (n_predictors == 1) {
    # SIMPLE REGRESSION: Scatter plot with regression line
    predictor_name <- attr(terms(model), "term.labels")[1]
    
    # Guard: check predictor exists in data
    if (!predictor_name %in% names(data)) {
      # Try to find it in model frame
      mf <- model.frame(model)
      if (predictor_name %in% names(mf)) {
        plot_data <- data.frame(
          x = mf[[predictor_name]],
          y = mf[[dv_name]],
          fitted = fitted_vals
        )
      } else {
        stop("Predictor variable not found in data")
      }
    } else {
      plot_data <- data.frame(
        x = data[[predictor_name]],
        y = data[[dv_name]],
        fitted = fitted_vals
      )
    }
    
    # Remove any NA values
    plot_data <- plot_data[complete.cases(plot_data), ]
    
    # Ensure numeric
    plot_data$x <- as.numeric(plot_data$x)
    plot_data$y <- as.numeric(plot_data$y)
    plot_data$fitted <- as.numeric(plot_data$fitted)
    
    # Fit line for plotting
    line_model <- lm(y ~ x, data = plot_data)
    line_data <- data.frame(
      x = seq(min(plot_data$x), max(plot_data$x), length.out = 100)
    )
    line_data$y <- predict(line_model, newdata = line_data)
    
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(alpha = 0.6, color = "#1B365D", size = 2) +
      geom_line(data = line_data, aes(x = x, y = y), 
                color = "#C41E3A", linewidth = 1) +
      labs(
        title = "Overall Regression Fit",
        subtitle = paste0("Simple Linear Regression: ", dv_name, " ~ ", predictor_name),
        x = predictor_name,
        y = dv_name
      ) +
      theme_apa()
    
  } else {
    # MULTIPLE REGRESSION: Fitted vs Actual plot
    plot_data <- data.frame(
      fitted = fitted_vals,
      actual = data[[dv_name]]
    )
    
    # Remove NA values
    plot_data <- plot_data[complete.cases(plot_data), ]
    
    # Ensure numeric
    plot_data$fitted <- as.numeric(plot_data$fitted)
    plot_data$actual <- as.numeric(plot_data$actual)
    
    p <- ggplot(plot_data, aes(x = fitted, y = actual)) +
      geom_point(alpha = 0.6, color = "#1B365D", size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "#C41E3A", linewidth = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "#228B22", fill = "#228B22", alpha = 0.2) +
      labs(
        title = "Overall Regression Fit",
        subtitle = paste0("Multiple Regression (", n_predictors, " predictors): Fitted vs Actual"),
        x = "Fitted Values",
        y = paste("Actual", dv_name)
      ) +
      theme_apa()
  }
  
  if (interactive) {
    return(ggplotly(p, tooltip = c("x", "y")))
  }
  
  return(p)
}

#' Create residual vs fitted plot
#' @param model An lm object
#' @param interactive Logical
#' @return ggplot or plotly object
#' @export
residuals_vs_fitted_plot <- function(model, interactive = TRUE) {
  
  augmented <- broom::augment(model) %>%
    as_tibble()
  
  p <- ggplot(augmented, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6, color = "#1B365D", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#6B8E8E", linewidth = 1) +
    geom_smooth(method = "loess", se = TRUE, color = "#C41E3A", fill = "#C41E3A", alpha = 0.2) +
    labs(
      title = "Residuals vs Fitted Values",
      subtitle = "Check for patterns (should be random scatter around 0)",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_apa()
  
  if (interactive) {
    return(ggplotly(p, tooltip = c("x", "y")))
  }
  
  return(p)
}