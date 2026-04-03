# 1. SET DEVELOPMENT OPTIONS
# ---------------------------------------------------------
options(golem.app.prod = FALSE) 
options(shiny.autoreload = TRUE)
# This pattern tells Shiny to watch your Module and Function files specifically
options(shiny.autoreload.pattern = glob2rx("R/*.R"))
options(shiny.autoreload.legacy_warning = FALSE)

# Fixed port for consistent testing
options(shiny.host = '0.0.0.0') 
options(shiny.port = 1234)

# 2. CLEAN & PREP ENVIRONMENT
# ---------------------------------------------------------
# Detach packages to prevent 'masked object' conflicts
golem::detach_all_attached()

# Document and load the package into memory
# This makes functions like db_site_add() available
golem::document_and_reload()

# 4. PRINT STATUS
# ---------------------------------------------------------
cat("\n============================================\n")
cat("🚀 CHANG LAB LIMS: LIVE REFRESH ACTIVE\n")
cat("============================================\n")
cat("• Edit files in /R to trigger auto-reload.\n")
cat("• If the app crashes, check the terminal below.\n")
cat("• URL: http://127.0.0.1:1234\n")
cat("============================================\n\n")

# 5. START APP
# ---------------------------------------------------------
run_app()