# NSMO Survey Analysis Dashboard - Main Application File
# Modular Version 2.0
# 
# INSTRUCTIONS:
# 1. Place this file and all supporting files in the same directory as nsmo.rds
# 2. Run: library(shiny); runApp("app.R")
#
# Required files in same directory:
# - app.R (this file)
# - global.R (data preprocessing and setup)
# - ui.R (user interface)
# - server.R (server logic)
# - nsmo.rds (your data file)

# Source the modular components
source("global.R")
source("ui.R") 
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
