# app.R
# Purpose: Main entry point that launches the Shiny application
# Why: Single file to run the entire application

# Source all components
source("global.R")

# Run the application
shinyApp(ui = ui, server = server)