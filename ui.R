# ui.R
# Purpose: Define the user interface
# Why: Separates UI from server logic for maintainability

# =============================================================================
# Dashboard Header
# =============================================================================
header <- dashboardHeader(
  title = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(src = "logo.png", height = "35", style = "margin-right: 10px;"),
    tags$span("Hierarchical Regression", style = "white-space: nowrap;")
  ),
  titleWidth = 400  # Increased from 280 to prevent cutoff
)

# =============================================================================
# Dashboard Sidebar
# =============================================================================
sidebar <- dashboardSidebar(
  width = 280,
  
  sidebarMenu(
    id = "sidebar_menu",
    
    # Data Input Section
    menuItem("1. Data Import", tabName = "data_import", icon = icon("upload")),
    
    # Variable Selection
    menuItem("2. Variables", tabName = "variables", icon = icon("list")),
    
    # Descriptive Statistics
    menuItem("3. Descriptives", tabName = "descriptives", icon = icon("chart-bar")),
    
    # Regression Analysis
    menuItem("4. Regression", tabName = "regression", icon = icon("calculator")),
    
    # Assumption Checks
    menuItem("5. Assumptions", tabName = "assumptions", icon = icon("check-circle"))
  ),
  
  # Sample data button
  div(style = "padding: 15px;",
      actionButton("load_sample", "Load Sample Data", 
                   icon = icon("database"),
                   class = "btn-primary",
                   width = "100%")
  )
)

# =============================================================================
# Dashboard Body
# =============================================================================
body <- dashboardBody(
  
  useShinyjs(),  
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .loading-spinner {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
      }
      .info-box-content {
        text-align: center;
      }
      .btn-primary {
        background-color: #1B365D !important;
        border-color: #1B365D !important;
      }
      .btn-primary:hover {
        background-color: #2a4a7a !important;
      }
    "))
  ),
  
  tabItems(
    
    # =========================================================================
    # Tab 1: Data Import
    # =========================================================================
    tabItem(
      tabName = "data_import",
      
      fluidRow(
        box(
          title = "Upload Data File",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          
          fileInput(
            "data_file",
            "Choose CSV, Excel, or SPSS File",
            accept = c(
              ".csv",
              ".xlsx", ".xls",
              ".sav", ".dta"
            ),
            multiple = FALSE
          ),
          
          conditionalPanel(
            condition = "input.data_file.name.endsWith('.xlsx') || input.data_file.name.endsWith('.xls')",
            uiOutput("sheet_selector")
          ),
          
          hr(),
          
          h5("File Requirements:"),
          tags$ul(
            tags$li("First row should contain column headers"),
            tags$li("Missing values coded as NA or blank cells"),
            tags$li("Numeric variables for regression analysis")
          )
        ),
        
        box(
          title = "Data Summary",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          
          uiOutput("data_summary_ui")
        )
      ),
      
      fluidRow(
        box(
          title = "Data Preview (First 100 Rows)",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          withSpinner(DT::dataTableOutput("data_preview"))
        )
      )
    ),
    
    # =========================================================================
    # Tab 2: Variable Selection
    # =========================================================================
    tabItem(
      tabName = "variables",
      
      fluidRow(
        box(
          title = "Select Variables",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          
          # DV selector (static)
          uiOutput("dv_selector"),
          
          hr(),
          
          h5("Hierarchical Blocks:"),
          p("Add predictors in blocks for hierarchical regression."),
          
          # Static block inputs (up to 5 blocks for MVP)
          div(
            id = "blocks_container",
            
            div(
              style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px; background-color: #f9f9f9;",
              strong("Block 1:"),
              selectInput(
                inputId = "block1_vars",
                label = "Select Predictors (IVs):",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            
            div(
              id = "block2_container",
              style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px; background-color: #f9f9f9; display: none;",
              strong("Block 2:"),
              selectInput(
                inputId = "block2_vars",
                label = "Select Predictors (IVs):",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            
            div(
              id = "block3_container",
              style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px; background-color: #f9f9f9; display: none;",
              strong("Block 3:"),
              selectInput(
                inputId = "block3_vars",
                label = "Select Predictors (IVs):",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            
            div(
              id = "block4_container",
              style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px; background-color: #f9f9f9; display: none;",
              strong("Block 4:"),
              selectInput(
                inputId = "block4_vars",
                label = "Select Predictors (IVs):",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            
            div(
              id = "block5_container",
              style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px; background-color: #f9f9f9; display: none;",
              strong("Block 5:"),
              selectInput(
                inputId = "block5_vars",
                label = "Select Predictors (IVs):",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100%"
              )
            )
          ),
          
          actionButton("add_block", "Add Block", icon = icon("plus"), 
                       class = "btn-primary"),
          actionButton("reset_blocks", "Reset", icon = icon("undo"))
        ),
        
        box(
          title = "Variable Information",
          status = "info",
          solidHeader = TRUE,
          width = 8,
          
          DT::dataTableOutput("variable_info")
        )
      ),
      
      fluidRow(
        box(
          title = "Selected Model Specification",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          
          uiOutput("model_formula_display")
        )
      )
    ),
    # =========================================================================
    # Tab 3: Descriptive Statistics
    # =========================================================================
    tabItem(
      tabName = "descriptives",
      
      fluidRow(
        box(
          title = "Continuous Variables",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          
          withSpinner(DT::dataTableOutput("descriptive_continuous"))
        ),
        
        box(
          title = "Categorical Variables",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          
          uiOutput("descriptive_categorical")
        )
      ),
      
      fluidRow(
        box(
          title = "Correlation Matrix",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          withSpinner(plotlyOutput("correlation_plot", height = "500px"))
        )
      )
    ),
    
    # =========================================================================
    # Tab 4: Regression Analysis
    # =========================================================================
    tabItem(
      tabName = "regression",
      
      fluidRow(
        column(12,
               actionButton("run_regression", "Run Analysis", 
                            icon = icon("play"), 
                            class = "btn-primary btn-lg"),
               br(), br()
        )
      ),
      
      fluidRow(
        box(
          title = "Overall Regression Fit",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          p("This plot shows how well the model predictions match the actual values."),
          
          withSpinner(plotlyOutput("overall_regression_plot", height = "450px"))
        )
      ),
      
      fluidRow(
        # Model Comparison Table
        box(
          title = "Model Comparison (Hierarchical)",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          withSpinner(uiOutput("hierarchical_table"))
        )
      ),
      
      fluidRow(
        # Final Model Coefficients
        box(
          title = "Final Model Coefficients",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          
          withSpinner(DT::dataTableOutput("final_coefficients"))
        ),
        
        # Model Summary
        box(
          title = "Model Summary",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          
          uiOutput("model_summary_ui")
        )
      ),
      
      fluidRow(
        # R² Change Plot
        box(
          title = "R² Change Visualization",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          
          withSpinner(plotlyOutput("rsquared_plot"))
        ),
        
        # Coefficient Plot
        box(
          title = "Coefficient Plot",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          
          withSpinner(plotlyOutput("coef_plot"))
        )
      ),
      
      fluidRow(
        # ... existing R² and Coefficient plots ...
      ),
      
      fluidRow(
        # Partial Regression Plots
        box(
          title = "Partial Regression Plots",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          p("These plots show the relationship between each predictor and the outcome after controlling for all other predictors."),
          
          uiOutput("partial_plots_ui")
        )
      )
    ),
    
    # =========================================================================
    # Tab 5: Assumption Checks
    # =========================================================================
    tabItem(
      tabName = "assumptions",
      
      fluidRow(
        box(
          title = "Assumption Summary",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          withSpinner(DT::dataTableOutput("assumption_summary")),
          
          br(),
          verbatimTextOutput("assumption_narrative")
        )
      ),
      
      fluidRow(
        tabBox(
          title = "Diagnostic Plots",
          id = "assumption_plots",
          width = 12,
          
          tabPanel(
            "Linearity",
            withSpinner(plotlyOutput("linearity_plot", height = "400px"))
          ),
          tabPanel(
            "Normality",
            withSpinner(plotlyOutput("normality_qq", height = "400px")),
            br(),
            withSpinner(plotlyOutput("normality_hist", height = "400px"))
          ),
          tabPanel(
            "Homoscedasticity",
            withSpinner(plotlyOutput("homoscedasticity_rvf", height = "400px")),
            br(),
            withSpinner(plotlyOutput("homoscedasticity_sl", height = "400px"))
          ),
          tabPanel(
            "Influential Cases",
            withSpinner(plotlyOutput("cooks_plot", height = "400px")),
            br(),
            DT::dataTableOutput("influential_cases")
          )
        )
      )
    )
  )
)
# =============================================================================
# Assemble UI
# =============================================================================
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)