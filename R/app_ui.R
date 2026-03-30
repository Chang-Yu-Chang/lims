#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 


app_ui <- function(request) {
  # Define theme
  my_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly", 
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("JetBrains Mono"),
    primary = "#FFB6C1",
    "body-bg" = "#ffffff",
    success = "#73628A"
  )

  tagList(
    # External resources
    golem_add_external_resources(),
    
    # Main app with navbar
    bslib::page_navbar(
      id = "main_nav",
      title = "Chang Lab LIMS",
      theme = my_theme,

      # HOME TAB
      bslib::nav_panel(
        title = "Home",
        value = "home",
        mod_home_ui("home")
      ),
      
      # SITES TAB (placeholder)
      bslib::nav_panel(
        title = "Sites",
        value = "sites",
        mod_sites_ui("sites")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lims"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
