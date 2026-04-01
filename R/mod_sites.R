#' Sites Management Module UI
#'
#' @param id Module ID
#'
#' @return Shiny UI
#'
#' @importFrom DT renderDataTable
#'
#' @export

# mod_sites_ui
mod_sites_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Add New Site"),
        bslib::card_body(
          shiny::textInput(ns("site_id"), "Site ID"),
          shiny::textAreaInput(ns("site_desc"), "Site Description"),
          # shiny::actionButton(ns("save_btn"), "SAVE THIS")
          bslib::input_task_button(ns("save_btn"), "Save Site")
          # bslib::input_task_button(ns("save_btn"), "Save")
        )
      ),
      bslib::card(
        bslib::card_header("Existing Sites"),
        DT::dataTableOutput(ns("sites_table"))
      )
    )
  )
}


#' Sites Management Module Server
#'
#' @param id Module ID
#' @param pool Database pool connection
#'
#' @importFrom DT renderDataTable
#'
#' @return Module server logic
#'
#' @export

mod_sites_server <- function(id, pool) {
  shiny::moduleServer(id, function(input, output, session) {
    sites_refresh <- shiny::reactiveVal(0)

    # Listen for the button click
    shiny::observeEvent(input$save_btn, {
      s_id <- trimws(input$site_id)
      s_desc <- trimws(input$site_desc)

      # Validation 1. Empty ID
      if (s_id == "") {
        shiny::showNotification("Error: Site ID cannot be empty", type = "error")
        return()
      }

      # Validation 2. Pattern Check (ST + 4 digits)
      if (!grepl("^ST[0-9]{4}$", s_id)) {
        shiny::showNotification("Format Error: Use ST0001", type = "error")
        return()
      }

      # 3. Validation: Duplicate Check
      existing <- db_site_fetch_all(pool) 
      if (s_id %in% existing$site_id) {
        shiny::showNotification("Error: This Site ID already exists", type = "error")
        return()
      }
      

      # Save to Database
      tryCatch(
        {
          # Ensure db_site_add is defined in fct_sites.R
          db_site_add(pool, s_id, s_desc)

          # 3. Reset the UI and notify user
          shiny::updateTextInput(session, "site_id", value = "")
          shiny::updateTextAreaInput(session, "site_desc", value = "")
          shiny::showNotification(paste("Site", s_id, "saved!"), type = "message")

          # Trigger refresh
          sites_refresh(sites_refresh() + 1)
        },
        error = function(e) {
          shiny::showNotification(paste("Database Error", e$message), type = "error")
        }
      )
    })

    # Display the table of all sites, refresh when sites_refresh changes
    output$sites_table <- DT::renderDataTable({
      sites_refresh()
      data <- db_site_fetch_all(pool)

      DT::datatable(
        data,
        colnames = c("Site ID", "Site Description", "Date Created"),
        options = list(pageLength = 10)
      )
    })
  })
}
