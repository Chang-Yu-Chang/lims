#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # When the title is clicked, navigate to the home page
  observeEvent(input$go_home, {
    bslib::nav_select(id = "main_nav", selected = "home")
  })
  
  # 1. Run the core database initialization first
  initialize_database()

  # Initialize database connection pool
  pool <- pool::dbPool(
    RSQLite::SQLite(),
    dbname = "lims_data.db"
  )
  
  session$onSessionEnded(function() {
    pool::poolClose(pool)
  })
  
  # Keep this so app refresh stays at the same page
  # 1. Update URL when tab changes
  observeEvent(input$main_nav, {
    # This changes the URL to match the 'value' in the UI
    shiny::updateQueryString(paste0("#", input$main_nav), mode = "replace")
  })

  # 2. On Startup: Check the URL and jump to that tab
  # We use 'priority = 10' to ensure this happens before other observers
  observe({
    query <- shiny::getUrlHash() 
    if (nchar(query) > 1) {
      tab_name <- substring(query, 2)
      # nav_select tells the UI: "Go to the tab with this value"
      bslib::nav_select("main_nav", selected = tab_name)
    }
  }, priority = 10)

  
  # Initialize modules
  mod_home_server("home", pool)
  mod_sites_server("sites", pool)
  mod_labels_server("labels", pool)
}
