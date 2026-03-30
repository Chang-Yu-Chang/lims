#' Sites Management Module UI
#'
#' @param id Module ID
#'
#' @return Shiny UI
#'
#' @export

mod_sites_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Add New Site"),
       bslib::card_body(
        min_height = 200,
        bslib::layout_column_wrap(
          width = 1/2,
          fixed_width = TRUE,
          textInput(ns("site_name"), "Site Name"),
          plotOutput("p2")
        )
      )
      # bslib::card_body(
      #   textInput(ns("site_name"), "Site Name"),
      #   textAreaInput(ns("description"), "Description"),
      #   actionButton(ns("add_site_btn"), "Save Site", class = "btn-primary")
      # )
    )
  )
}

#' Sites Management Module Server
#'
#' @param id Module ID
#' @param pool Database pool connection
#'
#' @return Module server logic
#'
#' @export
mod_sites_server <- function(id, pool) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive trigger for database updates
    sites_refresh <- shiny::reactiveVal(0)
    
    # 1. Display next site ID
    output$next_site_id <- shiny::renderText({
      sites_refresh()
      get_next_site_id(pool)
    })
    
    # 2. Add site button handler
    shiny::observeEvent(input$add_site_btn, {
      site_name <- shiny::trimws(input$site_name)
      description <- shiny::trimws(input$description)
      
      # Validation
      if (site_name == "") {
        shiny::showNotification("Please enter a site name", type = "error")
        return()
      }
      
      # Execute Database Logic
      tryCatch({
        add_site(pool, site_name, description)
        
        # Reset UI
        shiny::updateTextInput(session, "site_name", value = "")
        shiny::updateTextAreaInput(session, "description", value = "")
        
        shiny::showNotification("Site added successfully!", type = "message")
        
        # Trigger UI refresh
        sites_refresh(sites_refresh() + 1)
        
      }, error = function(e) {
        shiny::showNotification(paste("Database Error:", e$message), type = "error")
      })
    })
    
    # 3. Clear form button
    shiny::observeEvent(input$clear_form_btn, {
      shiny::updateTextInput(session, "site_name", value = "")
      shiny::updateTextAreaInput(session, "description", value = "")
    })
    
    # 4. Display sites table
    output$sites_table <- DT::renderDataTable({
      sites_refresh()
      fetch_all_sites(pool)
    }, options = list(pageLength = 10, scrollX = TRUE))
    
  })
}