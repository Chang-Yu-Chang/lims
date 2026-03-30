# This file is for the VS Code Shiny Extension / Live Preview
pkgload::load_all(here::here())
options(shiny.autoreload = TRUE)
lims::run_app() # Replace 'lims' with the name in your DESCRIPTION file