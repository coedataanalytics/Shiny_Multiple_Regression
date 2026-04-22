# R/apa_formatting.R
# Purpose: Generate APA-formatted output
# Why: Researchers need properly formatted results for manuscripts

#' Format regression table in APA style
#' @param model_result Result from fit_regression() or fit_hierarchical_regression()
#' @param format Output format ("html", "latex", or "dataframe")
#' @return Formatted table
#' @export
apa_regression_table <- function(model_result, format = "html") {
  
  # Handle hierarchical regression
  if (inherits(model_result, "hierarchical_regression")) {
    return(apa_hierarchical_table(model_result, format))
  }
  
  # Single model
  coef_table <- model_result$coefficients %>%
    mutate(
      # Format b
      b = round_pretty(estimate, 2),
      # Format SE
      se = round_pretty(std.error, 2),
      # Format t
      t = round_pretty(statistic, 2),
      # Format p
      p = format_p_value(p.value),
      # Format CI
      ci = paste0("[", round_pretty(conf.low, 2), ", ", 
                  round_pretty(conf.high, 2), "]"),
      # Significance
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01 ~ "**",
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(term, b, se, t, p, ci, sig) %>%
    rename(
      Predictor = term,
      b = b,
      SE = se,
      t = t,
      p = p,
      `95% CI` = ci,
      Sig = sig  # FIXED: Changed from empty name to "Sig"
    )
  
  # Add model statistics
  model_stats <- tibble(
    Predictor = c("", "R²", "Adjusted R²", "F", "n"),
    b = c("", round_pretty(model_result$r_squared, 3), 
          round_pretty(model_result$adj_r_squared, 3), 
          round_pretty(model_result$f_statistic[1], 2),
          as.character(model_result$n)),
    SE = c("", "", "", "", ""),
    t = c("", "", "", "", ""),
    p = c("", "", "", format_p_value(model_result$f_pvalue), ""),
    `95% CI` = c("", "", "", "", ""),
    Sig = c("", "", "", "", "")
  )
  
  coef_table <- bind_rows(coef_table, model_stats)
  
  if (format == "html") {
    return(knitr::kable(coef_table, format = "html", 
                        align = c("l", "r", "r", "r", "r", "c", "l"),
                        escape = FALSE) %>%
             kableExtra::kable_styling(bootstrap_options = c("striped", "hover")))
  }
  
  return(coef_table)
}

#' Format hierarchical regression table in APA style
#' @param hier_result Result from fit_hierarchical_regression()
#' @param format Output format
#' @return Formatted table
#' @export
apa_hierarchical_table <- function(hier_result, format = "html") {
  
  # Build coefficient table across models
  all_predictors <- unique(unlist(hier_result$blocks))
  
  # Get coefficients from each model
  coef_list <- map(hier_result$models, function(m) {
    m$tidy %>%
      select(term, estimate, std.error, p.value) %>%
      mutate(
        coef_str = paste0(round_pretty(estimate, 2), 
                          " (", round_pretty(std.error, 2), ")"),
        sig = case_when(
          p.value < .001 ~ "***",
          p.value < .01 ~ "**",
          p.value < .05 ~ "*",
          p.value < .10 ~ ".",
          TRUE ~ ""
        )
      ) %>%
      select(term, coef_str, sig) %>%
      unite("full_coef", coef_str, sig, sep = "")
  })
  
  # Join all models
  coef_table <- tibble(term = all_predictors)
  
  for (i in seq_along(coef_list)) {
    coef_table <- coef_table %>%
      left_join(coef_list[[i]], by = "term")
    names(coef_table)[ncol(coef_table)] <- hier_result$block_names[i]
  }
  
  # Add model statistics
  stats_rows <- tibble(
    term = c("R²", "ΔR²", "F", "ΔF")
  )
  
  # Add first block stats
  stats_rows[[hier_result$block_names[1]]] <- c(
    round_pretty(hier_result$models[[1]]$r_squared, 3),
    "-",
    round_pretty(hier_result$models[[1]]$summary$fstatistic[1], 2),
    "-"
  )
  
  # Add subsequent block stats
  for (i in 2:length(hier_result$models)) {
    stats_rows[[hier_result$block_names[i]]] <- c(
      round_pretty(hier_result$models[[i]]$r_squared, 3),
      round_pretty(hier_result$change_statistics$r_squared_change[i], 3),
      round_pretty(hier_result$models[[i]]$summary$fstatistic[1], 2),
      round_pretty(hier_result$change_statistics$f_change[i], 2)
    )
  }
  
  coef_table <- bind_rows(coef_table, stats_rows)
  names(coef_table)[1] <- "Predictor"
  
  if (format == "html") {
    return(knitr::kable(coef_table, format = "html",
                        align = c("l", rep("r", length(hier_result$models)))) %>%
             kableExtra::kable_styling(bootstrap_options = c("striped", "hover")))
  }
  
  return(coef_table)
}

#' Format assumption check summary for reporting
#' @param assumption_result Result from check_all_assumptions()
#' @return Character string with APA-formatted summary
#' @export
apa_assumption_summary <- function(assumption_result) {
  
  # Build narrative summary
  lines <- c(
    "Assumption Checks:",
    "",
    paste0("1. Linearity: ", assumption_result$linearity$rainbow_test$interpretation),
    paste0("2. Normality: ", assumption_result$normality$shapiro_wilk$interpretation),
    paste0("3. Homoscedasticity: ", assumption_result$homoscedasticity$breusch_pagan$interpretation),
    paste0("4. Independence: ", assumption_result$independence$durbin_watson$interpretation),
    paste0("5. Multicollinearity: ", assumption_result$multicollinearity$overall_interpretation)
  )
  
  if (assumption_result$influential$n_influential > 0) {
    lines <- c(lines, 
               paste0("6. Influential observations: ", 
                      assumption_result$influential$n_influential,
                      " cases flagged (Cook's D > ", 
                      round(assumption_result$influential$cooks_threshold, 3), ")"))
  } else {
    lines <- c(lines, "6. Influential observations: None detected")
  }
  
  return(paste(lines, collapse = "\n"))
}

#' Write results to clipboard (for easy pasting into documents)
#' @param content Content to copy
#' @return Invisible NULL, side effect of copying to clipboard
#' @export
copy_to_clipboard <- function(content) {
  
  tryCatch({
    # Newer clipr versions don't require availability check
    # Just attempt the write and catch any errors
    clipr::write_clip(content, clipboard = "clipboard")
    message("Content copied to clipboard!")
    
  }, error = function(e) {
    # Provide helpful error message if clipboard fails
    warning(
      "Clipboard access failed: ", e$message, "\n",
      "Possible solutions:\n",
      "1. On Linux: Install xclip or xsel (sudo apt install xclip)\n",
      "2. On macOS: Ensure terminal has accessibility permissions\n",
      "3. On Windows: Check if another app is blocking clipboard\n",
      "4. Manually select and copy the table from the Export tab"
    )
  })
  
  invisible(NULL)
}