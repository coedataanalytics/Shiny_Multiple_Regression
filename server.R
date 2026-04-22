# server.R
# Purpose: Server logic and reactive orchestration
# Why: Handles all reactive data flow and user interactions

server <- function(input, output, session) {
  
  # ===========================================================================
  # Reactive Values - Initialize FIRST before any observers
  # ===========================================================================
  
  rv <- reactiveValues(
    data = NULL,
    var_info = NULL,
    dv = NULL,
    blocks = list(block1 = character(0)),
    block_counter = 1,
    regression_result = NULL,
    assumption_result = NULL,
    descriptive_result = NULL,
    last_block_update = Sys.time()
  )
  
  # ===========================================================================
  # Data Import
  # ===========================================================================
  
  # Load sample data
  observeEvent(input$load_sample, {
    set.seed(123)
    n <- 200
    
    rv$data <- tibble(
      id = 1:n,
      job_satisfaction = round(rnorm(n, 3.5, 0.8) + 
                                 0.3 * rnorm(n, 3.5, 0.9) + 
                                 0.25 * rnorm(n, 3.2, 0.85) - 
                                 0.2 * rnorm(n, 3.0, 1.0) + 
                                 rnorm(n, 0, 0.5), 2),
      workload = round(rnorm(n, 3.0, 1.0), 2),
      autonomy = round(rnorm(n, 3.5, 0.9), 2),
      supervisor_support = round(rnorm(n, 3.2, 0.85), 2),
      tenure_years = round(runif(n, 0, 20), 1),
      age = round(rnorm(n, 35, 10)),
      gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE),
      department = sample(c("Sales", "Marketing", "Engineering", "HR"), n, replace = TRUE)
    )
    
    rv$var_info <- detect_variable_types(rv$data)
    
    showNotification("Sample data loaded!", type = "message")
    updateTabItems(session, "sidebar_menu", "variables")
  })
  
  # Handle file upload
  observeEvent(input$data_file, {
    req(input$data_file)
    
    tryCatch({
      rv$data <- import_data(input$data_file$datapath)
      rv$var_info <- detect_variable_types(rv$data)
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Sheet selector for Excel files
  output$sheet_selector <- renderUI({
    req(input$data_file)
    
    if (grepl("\\.xlsx?$", input$data_file$name, ignore.case = TRUE)) {
      sheets <- readxl::excel_sheets(input$data_file$datapath)
      selectInput("excel_sheet", "Select Sheet:", choices = sheets)
    }
  })
  
  # Re-load data when sheet changes
  observeEvent(input$excel_sheet, {
    req(input$data_file, input$excel_sheet)
    
    tryCatch({
      rv$data <- import_data(input$data_file$datapath, sheet = input$excel_sheet)
      rv$var_info <- detect_variable_types(rv$data)
    }, error = function(e) {
      showNotification(paste("Error loading sheet:", e$message), type = "error")
    })
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(rv$data)
    
    DT::datatable(
      head(rv$data, 100),
      options = list(scrollX = TRUE, pageLength = 10, dom = 'Bfrtip'),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  # Data summary UI
  output$data_summary_ui <- renderUI({
    req(rv$data)
    
    tagList(
      infoBox("Rows", nrow(rv$data), icon = icon("table"), color = "blue", width = 6),
      infoBox("Columns", ncol(rv$data), icon = icon("columns"), color = "blue", width = 6),
      br(),
      h5("Missing Data:"),
      verbatimTextOutput("missing_summary")
    )
  })
  
  output$missing_summary <- renderText({
    req(rv$data)
    missing_count <- sum(is.na(rv$data))
    missing_pct <- mean(is.na(rv$data)) * 100
    paste0("Total missing cells: ", missing_count, " (", round(missing_pct, 2), "%)")
  })
  
  # ===========================================================================
  # Variable Selection (STATIC INPUTS)
  # ===========================================================================
  
  # Variable info table
  output$variable_info <- DT::renderDataTable({
    req(rv$var_info)
    
    DT::datatable(
      rv$var_info,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE,
      selection = 'none'
    ) %>%
      DT::formatPercentage('pct_missing', digits = 1)
  })
  
  # DV selector
  output$dv_selector <- renderUI({
    req(rv$data)
    
    continuous_vars <- character(0)
    if (!is.null(rv$var_info)) {
      continuous_vars <- rv$var_info %>%
        filter(suggested_type == "continuous") %>%
        pull(variable)
    }
    
    if (length(continuous_vars) == 0) {
      continuous_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
    }
    
    if (length(continuous_vars) == 0) {
      continuous_vars <- names(rv$data)
    }
    
    selectInput(
      "dv",
      "Select Dependent Variable (DV):",
      choices = c("-- Select a DV --" = "", continuous_vars),
      selected = ""
    )
  })
  
  # Reactive expression for available predictors
  available_predictors <- reactive({
    req(rv$data)
    
    all_vars <- names(rv$data)
    excluded <- c("id", "ID", "Id")
    
    if (!is.null(input$dv) && input$dv != "") {
      excluded <- c(excluded, input$dv)
    }
    
    setdiff(all_vars, excluded)
  })
  
  # Update ALL block selectInputs when predictors change
  observe({
    req(available_predictors())
    
    preds <- available_predictors()
    
    updateSelectInput(session, "block1_vars", choices = preds)
    updateSelectInput(session, "block2_vars", choices = preds)
    updateSelectInput(session, "block3_vars", choices = preds)
    updateSelectInput(session, "block4_vars", choices = preds)
    updateSelectInput(session, "block5_vars", choices = preds)
  })
  
  # Track active blocks
  rv$active_block_count <- 1
  
  # Add block button - show next hidden block container
  observeEvent(input$add_block, {
    req(rv$active_block_count < 5)
    
    rv$active_block_count <- rv$active_block_count + 1
    block_id <- paste0("block", rv$active_block_count, "_container")
    
    # Use JavaScript to show the next block
    shinyjs::show(block_id)
    
    showNotification(paste("Added Block", rv$active_block_count), type = "message")
  })
  
  # Reset blocks - hide all except block 1
  observeEvent(input$reset_blocks, {
    rv$active_block_count <- 1
    
    # Hide blocks 2-5
    shinyjs::hide("block2_container")
    shinyjs::hide("block3_container")
    shinyjs::hide("block4_container")
    shinyjs::hide("block5_container")
    
    # Clear selections
    updateSelectInput(session, "block1_vars", selected = character(0))
    updateSelectInput(session, "block2_vars", selected = character(0))
    updateSelectInput(session, "block3_vars", selected = character(0))
    updateSelectInput(session, "block4_vars", selected = character(0))
    updateSelectInput(session, "block5_vars", selected = character(0))
    
    showNotification("Reset to single block", type = "message")
  })
  
  # Capture block selections into rv$blocks
  observeEvent(input$block1_vars, {
    rv$blocks$block1 <- input$block1_vars %||% character(0)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  observeEvent(input$block2_vars, {
    rv$blocks$block2 <- input$block2_vars %||% character(0)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  observeEvent(input$block3_vars, {
    rv$blocks$block3 <- input$block3_vars %||% character(0)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  observeEvent(input$block4_vars, {
    rv$blocks$block4 <- input$block4_vars %||% character(0)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  observeEvent(input$block5_vars, {
    rv$blocks$block5 <- input$block5_vars %||% character(0)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # Remove DV from block selections when DV changes
  observeEvent(input$dv, {
    req(rv$data)
    
    if (!is.null(input$dv) && input$dv != "") {
      for (i in 1:5) {
        block_name <- paste0("block", i)
        current_selection <- rv$blocks[[block_name]]
        if (!is.null(current_selection) && input$dv %in% current_selection) {
          rv$blocks[[block_name]] <- setdiff(current_selection, input$dv)
          # Also update the UI
          updateSelectInput(session, paste0("block", i, "_vars"), 
                            selected = rv$blocks[[block_name]])
        }
      }
    }
  })
  
  # Model formula display
  output$model_formula_display <- renderUI({
    req(rv$data)
    
    dv_selected <- !is.null(input$dv) && input$dv != ""
    dv_label <- if (dv_selected) input$dv else "[Select DV]"
    
    # Get active blocks only
    active_blocks <- list()
    for (i in 1:rv$active_block_count) {
      block_name <- paste0("block", i)
      predictors <- rv$blocks[[block_name]]
      if (is.null(predictors)) predictors <- character(0)
      predictors <- predictors[!is.na(predictors) & predictors != ""]
      active_blocks[[block_name]] <- predictors
    }
    
    all_predictors <- unlist(active_blocks)
    all_predictors <- all_predictors[!is.na(all_predictors) & all_predictors != ""]
    
    # Build formula display for each active block
    block_formulas <- lapply(names(active_blocks), function(block_name) {
      predictors <- active_blocks[[block_name]]
      block_num <- gsub("block", "", block_name)
      
      if (length(predictors) > 0) {
        div(
          style = "margin-bottom: 10px;",
          strong(paste0("Block ", block_num, ":")),
          br(),
          code(paste(dv_label, "~", paste(predictors, collapse = " + ")))
        )
      } else {
        div(
          style = "margin-bottom: 10px; color: #888;",
          strong(paste0("Block ", block_num, ":")),
          br(),
          em("(no predictors selected)")
        )
      }
    })
    
    # Full model formula
    if (length(all_predictors) > 0 && dv_selected) {
      full_formula <- div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4e8; border-radius: 5px;",
        strong("Full Model:"),
        br(),
        code(paste(input$dv, "~", paste(all_predictors, collapse = " + ")))
      )
    } else if (length(all_predictors) > 0 && !dv_selected) {
      full_formula <- div(
        style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 5px;",
        strong("Full Model (select DV to complete):"),
        br(),
        code(paste("[Select DV] ~", paste(all_predictors, collapse = " + ")))
      )
    } else {
      full_formula <- div(
        style = "margin-top: 15px; padding: 10px; background-color: #f8f8f8; border-radius: 5px; color: #888;",
        em("Select predictors above to build your model")
      )
    }
    
    tagList(
      h4("Model Specification:"),
      hr(),
      do.call(tagList, block_formulas),
      full_formula
    )
  })
  # ===========================================================================
  # Descriptive Statistics
  # ===========================================================================
  
  observeEvent(input$sidebar_menu, {
    if (input$sidebar_menu == "descriptives" && !is.null(rv$data)) {
      all_vars <- c(input$dv, unlist(rv$blocks))
      all_vars <- all_vars[all_vars != ""]
      
      if (length(all_vars) > 0) {
        rv$descriptive_result <- generate_descriptive_report(
          rv$data, 
          input$dv, 
          setdiff(all_vars, input$dv)
        )
      }
    }
  })
  
  output$descriptive_continuous <- DT::renderDataTable({
    req(rv$descriptive_result)
    
    DT::datatable(
      rv$descriptive_result$continuous,
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatRound(c('mean', 'sd', 'se', 'min', 'q25', 'median', 'q75', 'max'), 2) %>%
      DT::formatRound(c('skewness', 'kurtosis'), 2)
  })
  
  output$descriptive_categorical <- renderUI({
    req(rv$descriptive_result)
    
    if (is.null(rv$descriptive_result$categorical)) {
      return(p("No categorical variables selected."))
    }
    
    tables <- lapply(names(rv$descriptive_result$categorical), function(var_name) {
      tagList(
        h5(paste0("Variable: ", var_name)),
        DT::datatable(
          rv$descriptive_result$categorical[[var_name]],
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    })
    
    do.call(tagList, tables)
  })
  
  output$correlation_plot <- renderPlotly({
    req(rv$descriptive_result)
    
    if (is.null(rv$descriptive_result$correlations)) {
      return(NULL)
    }
    
    correlation_heatmap(rv$descriptive_result$correlations, interactive = TRUE)
  })
  
  # ===========================================================================
  # Regression Analysis
  # ===========================================================================
  
  observeEvent(input$run_regression, {
    req(rv$data)
    
    if (is.null(input$dv) || input$dv == "") {
      showNotification("Please select a Dependent Variable (DV).", type = "error")
      return()
    }
    
    all_predictors <- unlist(rv$blocks)
    all_predictors <- all_predictors[all_predictors != ""]
    
    if (length(all_predictors) == 0) {
      showNotification("Please select at least one predictor.", type = "error")
      return()
    }
    
    n_blocks <- sum(sapply(rv$blocks, function(x) length(x) > 0))
    
    tryCatch({
      withProgress(message = "Running regression analysis...", value = 0, {
        
        incProgress(0.1, detail = "Preparing data...")
        prep_result <- prepare_regression_data(rv$data, input$dv, all_predictors)
        clean_data <- prep_result$data
        
        if (nrow(clean_data) < 10) {
          stop("Not enough complete cases for analysis.")
        }
        
        incProgress(0.3, detail = "Fitting models...")
        
        if (n_blocks > 1) {
          non_empty_blocks <- rv$blocks[sapply(rv$blocks, function(x) length(x) > 0)]
          block_names <- paste0("Block ", 1:length(non_empty_blocks))
          
          rv$regression_result <- fit_hierarchical_regression(
            clean_data, input$dv, non_empty_blocks, block_names
          )
        } else {
          rv$regression_result <- fit_regression(clean_data, input$dv, all_predictors)
        }
        
        incProgress(0.5, detail = "Checking assumptions...")
        
        if (inherits(rv$regression_result, "hierarchical_regression")) {
          model_for_assumptions <- rv$regression_result$final_model$model
        } else {
          model_for_assumptions <- rv$regression_result$model
        }
        
        rv$assumption_result <- check_all_assumptions(model_for_assumptions, clean_data)
        
        incProgress(1, detail = "Complete!")
      })
      
      showNotification("Analysis complete!", type = "message")
      updateTabItems(session, "sidebar_menu", "regression")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$hierarchical_table <- DT::renderDataTable({
    req(rv$regression_result)
    
    if (inherits(rv$regression_result, "hierarchical_regression")) {
      # Return as data frame for DT::datatable instead of HTML
      apa_regression_table(rv$regression_result, format = "dataframe")
    }
  }, options = list(dom = 't', pageLength = 10))
  
  # Final coefficients table with comprehensive statistics
  output$final_coefficients <- DT::renderDataTable({
    req(rv$regression_result, rv$data)
    
    # Get model and data
    if (inherits(rv$regression_result, "hierarchical_regression")) {
      model <- rv$regression_result$final_model$model
      dv_name <- rv$regression_result$dv
    } else {
      model <- rv$regression_result$model
      dv_name <- rv$regression_result$dv
    }
    
    # Prepare clean data
    all_predictors <- unlist(rv$blocks)
    all_predictors <- all_predictors[all_predictors != ""]
    prep_result <- prepare_regression_data(rv$data, dv_name, all_predictors)
    clean_data <- prep_result$data
    
    # Generate comprehensive coefficient table
    coef_table <- comprehensive_coefficient_table(model, clean_data)
    
    # Format for display
    display_table <- coef_table %>%
      mutate(
        b = sprintf("%.3f", b),
        se = sprintf("(%.3f)", se),
        beta = sprintf("%.3f", beta),
        t = sprintf("%.2f", t),
        p = case_when(
          p < .001 ~ "< .001***",
          p < .01 ~ sprintf("%.3f**", p),
          p < .05 ~ sprintf("%.3f*", p),
          p < .10 ~ sprintf("%.3f.", p),
          TRUE ~ sprintf("%.3f", p)
        ),
        conf.low = sprintf("%.3f", conf.low),
        conf.high = sprintf("%.3f", conf.high),
        partial_r = sprintf("%.3f", partial_r),
        semi_partial_r = sprintf("%.3f", semi_partial_r),
        sr_squared = sprintf("%.3f", sr_squared)
      ) %>%
      select(term, b, beta, se, t, p, conf.low, conf.high, 
             partial_r, semi_partial_r, sr_squared) %>%
      rename(
        Predictor = term,
        `B (Unstd.)` = b,
        `β (Std.)` = beta,
        `SE` = se,
        `t` = t,
        `p` = p,
        `95% CI Low` = conf.low,
        `95% CI High` = conf.high,
        `pr` = partial_r,
        `sr` = semi_partial_r,
        `sr²` = sr_squared
      )
    
    DT::datatable(
      display_table,
      options = list(
        scrollX = TRUE,
        dom = 't',
        pageLength = 10
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle('Predictor', fontWeight = 'bold')
  })
  
  output$model_summary_ui <- renderUI({
    req(rv$regression_result)
    
    if (inherits(rv$regression_result, "hierarchical_regression")) {
      final <- rv$regression_result$final_model
      
      tagList(
        infoBox("R²", round(final$r_squared, 3), icon = icon("chart-line"), color = "blue"),
        infoBox("Adj. R²", round(final$adj_r_squared, 3), icon = icon("chart-line"), color = "blue"),
        infoBox("n", rv$regression_result$n, icon = icon("users"), color = "green"),
        br(),
        h5("F-statistic:"),
        verbatimTextOutput("f_stat_output")
      )
    } else {
      tagList(
        infoBox("R²", round(rv$regression_result$r_squared, 3), icon = icon("chart-line"), color = "blue"),
        infoBox("Adj. R²", round(rv$regression_result$adj_r_squared, 3), icon = icon("chart-line"), color = "blue"),
        infoBox("n", rv$regression_result$n, icon = icon("users"), color = "green")
      )
    }
  })
  
  output$f_stat_output <- renderText({
    req(rv$regression_result)
    
    if (inherits(rv$regression_result, "hierarchical_regression")) {
      f <- rv$regression_result$final_model$summary$fstatistic
      p_val <- pf(f[1], f[2], f[3], lower.tail = FALSE)
      paste0("F(", f[2], ", ", f[3], ") = ", round(f[1], 2), ", p ", format_p_value(p_val))
    }
  })
  
  output$rsquared_plot <- renderPlotly({
    req(rv$regression_result)
    
    # Check if hierarchical regression was performed
    if (!inherits(rv$regression_result, "hierarchical_regression")) {
      # Single model - show simple R² display
      return(plot_ly() %>%
               add_text(
                 x = 1, y = 1,
                 text = paste0("R² = ", round(rv$regression_result$r_squared, 3)),
                 mode = "text",
                 textfont = list(size = 24)
               ) %>%
               layout(
                 title = "Model R²",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }
    
    # Hierarchical regression - create change plot
    tryCatch({
      change_data <- rv$regression_result$change_statistics
      
      # Validate data exists
      if (is.null(change_data) || nrow(change_data) == 0) {
        return(plot_ly() %>%
                 add_text(text = "No change statistics available") %>%
                 layout(title = "R² Change Plot"))
      }
      
      # Create plot directly instead of calling helper function
      p <- ggplot(change_data, aes(x = block, y = r_squared_change)) +
        geom_col(aes(fill = !is.na(p_change) & p_change < .05), 
                 alpha = 0.8, na.rm = TRUE) +
        geom_text(
          aes(label = paste0("ΔR² = ", round(r_squared_change, 3))),
          vjust = -0.5, size = 4, na.rm = TRUE
        ) +
        scale_fill_manual(values = c("TRUE" = "#228B22", "FALSE" = "#6B8E8E"),
                          guide = "none") +
        labs(
          title = "R² Change by Block",
          subtitle = "Green = significant change (p < .05)",
          x = "Model Block",
          y = expression(Delta~R^2)
        ) +
        theme_apa() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
      
    }, error = function(e) {
      # Show error message in plot area instead of infinite loading
      plot_ly() %>%
        add_text(
          text = paste("Error:", e$message),
          mode = "text",
          textfont = list(color = "red")
        ) %>%
        layout(title = "R² Change Plot Error")
    })
  })
  
  # Coefficient plot
  output$coef_plot <- renderPlotly({
    req(rv$regression_result, rv$data)
    
    tryCatch({
      # Get model and data
      if (inherits(rv$regression_result, "hierarchical_regression")) {
        model <- rv$regression_result$final_model$model
      } else {
        model <- rv$regression_result$model
      }
      
      all_predictors <- unlist(rv$blocks)
      all_predictors <- all_predictors[all_predictors != ""]
      prep_result <- prepare_regression_data(rv$data, 
                                             rv$regression_result$dv, 
                                             all_predictors)
      clean_data <- prep_result$data
      
      # Validate we have predictors
      if (length(all_predictors) == 0) {
        return(plot_ly(type = 'scatter', mode = 'text',
                       text = ~"No predictors in model",
                       x = 0, y = 0))
      }
      
      # Create the plot
      coef_plot_obj <- coefficient_plot(model, clean_data, 
                                        type = "unstandardized", 
                                        interactive = FALSE)
      
      # Convert to plotly with explicit tooltip specification
      ggplotly(coef_plot_obj, tooltip = c("x", "y"))
      
    }, error = function(e) {
      # FIXED: Provide x, y coordinates for error text
      plot_ly(type = 'scatter', mode = 'text',
              x = 0, y = 0,
              text = ~paste("Error:", e$message),
              textfont = list(color = "red", size = 14)) %>%
        layout(
          title = "Coefficient Plot Error",
          xaxis = list(visible = FALSE, range = c(-1, 1)),
          yaxis = list(visible = FALSE, range = c(-1, 1)),
          showlegend = FALSE
        )
    })
  })
  
  # Partial regression plots
  output$partial_plots_ui <- renderUI({
    req(rv$regression_result)
    
    # Get the final model
    if (inherits(rv$regression_result, "hierarchical_regression")) {
      model <- rv$regression_result$final_model$model
    } else {
      model <- rv$regression_result$model
    }
    
    # Check if multiple predictors exist
    n_predictors <- length(attr(terms(model), "term.labels"))
    
    if (n_predictors < 2) {
      return(p("Partial regression plots require at least 2 predictors."))
    }
    
    tryCatch({
      plots <- partial_regression_plots(model, rv$data, interactive = TRUE)
      
      # Create plot cards
      plot_cards <- lapply(names(plots), function(plot_name) {
        div(
          style = "display: inline-block; width: 48%; margin: 1%; vertical-align: top;",
          withSpinner(plotlyOutput(paste0("partial_plot_", plot_name), height = "350px"))
        )
      })
      
      # Create individual plot outputs
      lapply(names(plots), function(plot_name) {
        output[[paste0("partial_plot_", plot_name)]] <- renderPlotly({
          plots[[plot_name]]
        })
      })
      
      do.call(tagList, plot_cards)
      
    }, error = function(e) {
      div(
        style = "padding: 20px; background-color: #fff3cd; border-radius: 5px;",
        h5("⚠️ Unable to Generate Partial Regression Plots"),
        p(paste("Error:", e$message))
      )
    })
  })
  
  # ===========================================================================
  # Assumption Checks
  # ===========================================================================
  
  output$assumption_summary <- DT::renderDataTable({
    req(rv$assumption_result)
    
    DT::datatable(
      rv$assumption_result$summary,
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
  
  output$assumption_narrative <- renderText({
    req(rv$assumption_result)
    apa_assumption_summary(rv$assumption_result)
  })
  
  output$linearity_plot <- renderPlotly({
    req(rv$assumption_result)
    ggplotly(rv$assumption_result$linearity$plot)
  })
  
  # Normality Q-Q Plot
  output$normality_qq <- renderPlotly({
    req(rv$assumption_result)
    
    tryCatch({
      qq_data <- rv$assumption_result$normality$qq_data
      
      if (is.null(qq_data) || nrow(qq_data) == 0) {
        return(plot_ly() %>% add_text(text = "No Q-Q data available"))
      }
      
      p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.6, color = "#1B365D", size = 2) +
        geom_abline(slope = 1, intercept = 0, color = "#C41E3A", linewidth = 1) +
        labs(
          title = "Normal Q-Q Plot",
          x = "Theoretical Quantiles",
          y = "Standardized Residuals"
        ) +
        theme_apa()
      
      ggplotly(p)
      
    }, error = function(e) {
      plot_ly() %>%
        add_text(text = paste("Error:", e$message), mode = "text") %>%
        layout(title = "Q-Q Plot Error")
    })
  })
  
  # Normality Histogram
  output$normality_hist <- renderPlotly({
    req(rv$assumption_result)
    
    tryCatch({
      hist_data <- rv$assumption_result$normality$hist_data
      
      if (is.null(hist_data) || nrow(hist_data) == 0) {
        return(plot_ly() %>% add_text(text = "No histogram data available"))
      }
      
      p <- ggplot(hist_data, aes(x = residuals)) +
        geom_histogram(aes(y = after_stat(density)), 
                       bins = 30, fill = "#1B365D", alpha = 0.7,
                       color = "white", na.rm = TRUE) +
        geom_density(color = "#C41E3A", linewidth = 1) +
        labs(
          title = "Distribution of Residuals",
          x = "Residuals",
          y = "Density"
        ) +
        theme_apa()
      
      ggplotly(p)
      
    }, error = function(e) {
      plot_ly() %>%
        add_text(text = paste("Error:", e$message), mode = "text") %>%
        layout(title = "Histogram Error")
    })
  })
  
  output$homoscedasticity_rvf <- renderPlotly({
    req(rv$assumption_result)
    ggplotly(rv$assumption_result$homoscedasticity$rvf_plot)
  })
  
  output$homoscedasticity_sl <- renderPlotly({
    req(rv$assumption_result)
    ggplotly(rv$assumption_result$homoscedasticity$scale_location_plot)
  })
  
  output$cooks_plot <- renderPlotly({
    req(rv$assumption_result)
    ggplotly(rv$assumption_result$influential$cooks_plot)
  })
  
  output$influential_cases <- DT::renderDataTable({
    req(rv$assumption_result)
    
    influential_df <- rv$assumption_result$influential$influence_summary %>%
      filter(is_influential)
    
    if (nrow(influential_df) == 0) {
      DT::datatable(
        tibble(Note = "No influential cases detected."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    } else {
      DT::datatable(
        influential_df,
        options = list(scrollX = TRUE, pageLength = 5),
        rownames = FALSE
      ) %>%
        DT::formatRound(c('cooks_d', 'leverage', 'standardized_residual', 'studentized_residual'), 3)
    }
  })
  
  # Debug output - add temporarily
  output$debug_regression <- renderText({
    req(rv$regression_result)
    
    if (inherits(rv$regression_result, "hierarchical_regression")) {
      paste(
        "Type: hierarchical_regression\n",
        "Number of blocks:", length(rv$regression_result$models), "\n",
        "Change stats rows:", nrow(rv$regression_result$change_statistics), "\n",
        "Change stats columns:", paste(names(rv$regression_result$change_statistics), collapse = ", ")
      )
    } else {
      paste("Type: simple regression\n", "R²:", rv$regression_result$r_squared)
    }
  })
  
  # Overall Regression Plot
  output$overall_regression_plot <- renderPlotly({
    req(rv$regression_result, rv$data)
    
    tryCatch({
      # Get model and DV name
      if (inherits(rv$regression_result, "hierarchical_regression")) {
        model <- rv$regression_result$final_model$model
        dv_name <- rv$regression_result$dv
      } else {
        model <- rv$regression_result$model
        dv_name <- rv$regression_result$dv
      }
      
      # Get clean data for plotting
      all_predictors <- unlist(rv$blocks)
      all_predictors <- all_predictors[all_predictors != ""]
      
      prep_result <- prepare_regression_data(rv$data, dv_name, all_predictors)
      plot_data <- prep_result$data
      
      # Validate data is numeric
      if (!is.numeric(plot_data[[dv_name]])) {
        stop(paste("DV", dv_name, "is not numeric"))
      }
      
      # Create the plot
      overall_regression_plot(model, plot_data, dv_name, interactive = TRUE)
      
    }, error = function(e) {
      plot_ly() %>%
        add_text(
          text = paste("Error:", e$message),
          mode = "text",
          textfont = list(color = "red", size = 14)
        ) %>%
        layout(
          title = "Overall Regression Plot Error",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    })
  })
  
  # Residuals vs Fitted Plot (add to Assumptions tab if not already there)
  output$residuals_fitted_plot <- renderPlotly({
    req(rv$assumption_result)
    
    tryCatch({
      if (inherits(rv$regression_result, "hierarchical_regression")) {
        model <- rv$regression_result$final_model$model
      } else {
        model <- rv$regression_result$model
      }
      
      residuals_vs_fitted_plot(model, interactive = TRUE)
      
    }, error = function(e) {
      plot_ly() %>%
        add_text(text = paste("Error:", e$message), mode = "text") %>%
        layout(title = "Residuals Plot Error")
    })
  })
  






  


  
}