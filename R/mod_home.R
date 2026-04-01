#' Home Page Module UI
#'
#' Display lab logo and LIMS description
#'
#' @param id Module ID
#'
#' @return Shiny UI
#'
#' @export
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  # bslib::page_fillable or tagList works well here
  tagList(
    # 1. Hero Section (Centered Logo and Title)
    div(
      style = "text-align: center; padding: 3rem 1rem;",
      img(
        src = "www/logo.png",
        height = "220px",
        style = "margin-bottom: 2rem; filter: drop-shadow(0 4px 6px rgba(0,0,0,0.1));"
      ),
      h2("Chang Lab @BRCAS", class = "fw-bold"),
      h3("Laboratory Information Management System (LIMS)", class = "text-muted mb-4"),

      # Description constrained to a readable width
      div(
        style = "max-width: 700px; margin: 0 auto;",
        tags$ul(
          class = "list-unstyled fs-5 mt-4",
          style = "line-height: 2;",
          tags$li(
            bsicons::bs_icon("check2-circle", class = "text-primary me-2"), "Track lab samples from collection to storage and use."
          ),
          tags$li(
            bsicons::bs_icon("check2-circle", class = "text-primary me-2"), "Manage sample sites and generate unique QR codes."
          ),
          tags$li(bsicons::bs_icon("check2-circle", class = "text-primary me-2"), "Maintain complete records of storage locations and usage history.")
        )
      )
    ),

    # 2. Quick Stats Section (Modern Value Boxes)
    # width = 1/3 ensures 3 boxes per row on PC, but stacks on mobile
    bslib::layout_column_wrap(
      width = 1/3,
      gap = "1rem",
      style = "min-height: 250px; min-width: 300px;",
      
      bslib::value_box(
        title = "Total Sites",
        value = "1", # Pure hardcoded string for testing
        showcase = bsicons::bs_icon("geo-alt-fill", class = "text-primary"),
        theme = NULL,
        fill = FALSE,
        class = "border-0 shadow-sm"
      ),
      bslib::value_box(
        title = "Total Samples",
        value = "42", 
        showcase = bsicons::bs_icon("basket2-fill", class = "text-primary"),
        theme = NULL,
        fill = FALSE,
        class = "border-0 shadow-sm"
      ),
      bslib::value_box(
        title = "Equipments",
        value = "42", 
        showcase = bsicons::bs_icon("house-door-fill", class = "text-primary"),
        theme = NULL,
        fill = FALSE,
        class = "border-0 shadow-sm"
      ),
      bslib::value_box(
        title = "Total Scans",
        value = "12",
        showcase = bsicons::bs_icon("qr-code-scan", class = "text-primary"),
        theme = NULL,
        fill = FALSE,
        class = "border-0 shadow-sm"
      )
    ),

    # 3. Footer
    div(
      class = "text-center text-muted",
      style = "margin-top: 5rem; padding-bottom: 3rem;",
      p(bsicons::bs_icon("arrow-up"), "Use the navigation tabs above to get started")
    )
  )
}

#' Home Page Module Server
mod_home_server <- function(id, pool) {
  shiny::moduleServer(id, function(input, output, session) {
    # Example of pulling a real count for the Home Page Value Box
    # output$site_count <- renderText({
    #   # Uses the helper function we discussed earlier
    #   res <- DBI::dbGetQuery(pool, "SELECT COUNT(*) as n FROM sites")
    #   res$n
    # })
  })
}
