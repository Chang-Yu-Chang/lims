# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# ENABLE AUTO-RELOAD: Browser will auto-refresh when you save files
options(shiny.autoreload = TRUE)
options(shiny.autoreload.r = TRUE) # Crucial: watches the R/ folder
options(shiny.autoload.js.version = TRUE)
options(shiny.minified = TRUE)
options(sass.cache = FALSE)

# Comment this if you don't want the app to be served on a random port
options(shiny.host = '0.0.0.0') 
options(shiny.port = 1234) # A standard port for Shiny

# Print development instructions
cat("\n")
cat("====================================\n")
cat("DEVELOPMENT MODE - LIVE RELOAD ACTIVE\n")
cat("====================================\n")
cat("1. Make changes to R/ files\n")
cat("2. Press Ctrl+Shift+L (or Cmd+Shift+L on Mac)\n")
cat("3. Browser will auto-refresh\n")
cat("====================================\n\n")

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
