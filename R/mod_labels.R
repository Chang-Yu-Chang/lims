#' labels UI Function
#' @export
mod_labels_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Create Labels"),
        bslib::card_body(
          shiny::selectizeInput(
            ns("site_id"), 
            "Select Site ID",
            choices = NULL,
            options = list(placeholder = "Search for site...")
          ),
          
          shiny::textInput(
            ns("plant_range"),
            "Plant IDs (e.g., 1-10 or 1,3,5-7)",
            value = "1-10"
          ),

          shiny::actionButton(
            ns("create_btn"),
            "Create Labels",
            class = "btn-primary w-100"
          )
        )
      ),
      bslib::card(
        bslib::card_header("Created Labels"),
        DT::dataTableOutput(ns("labels_table"))
      )
    )
  )
}

#' labels Server Functions
#' @export
mod_labels_server <- function(id, pool) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    labels_refresh <- shiny::reactiveVal(0)
    generated_labels <- shiny::reactiveVal(data.frame(
      plant_id = character(),
      site_id = character(),
      created_at = character(),
      stringsAsFactors = FALSE
    ))

    # Update site choices from DB
    shiny::observe({
      tryCatch({
        sites <- db_site_fetch_all(pool)
        if (nrow(sites) > 0) {
          shiny::updateSelectizeInput(session, "site_id", choices = sites$site_id, server = TRUE)
        }
      }, error = function(e) NULL)
    })

    # Handle Creation
    shiny::observeEvent(input$create_btn, {
      site_id <- input$site_id
      range_text <- trimws(input$plant_range)
      
      if (is.null(site_id) || site_id == "") {
        shiny::showNotification("Please select a Site ID", type = "error")
        return()
      }
      
      # Use the function from fct_labels.R
      plant_nums <- parse_range(range_text)
      
      if (is.null(plant_nums) || any(is.na(plant_nums))) {
        shiny::showNotification("Invalid range format. Use numbers, commas, and dashes (e.g. 1-5, 10)", type = "error")
        return()
      }

      new_ids <- sprintf("%s-P%03d", site_id, plant_nums)

      # Deduplication
      current_data <- generated_labels()
      final_ids <- new_ids[!(new_ids %in% current_data$plant_id)]
      duplicates <- new_ids[new_ids %in% current_data$plant_id]

      if (length(duplicates) > 0) {
        shiny::showNotification(paste("Skipped", length(duplicates), "duplicate labels."), type = "warning")
      }

      if (length(final_ids) == 0) return()

      tryCatch({
        new_rows <- data.frame(
          plant_id = final_ids,
          site_id = rep(site_id, length(final_ids)),
          created_at = rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), length(final_ids)),
          stringsAsFactors = FALSE
        )
        
        generated_labels(rbind(current_data, new_rows))
        shiny::showNotification(paste("Added", length(final_ids), "labels."), type = "message")
        labels_refresh(labels_refresh() + 1)
        
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    output$labels_table <- DT::renderDataTable({
      labels_refresh()
      DT::datatable(
        generated_labels(),
        colnames = c("Plant ID", "Site ID", "Created At"),
        options = list(pageLength = 10, order = list(list(2, 'desc'))),
        rownames = FALSE
      )
    })
  })
}