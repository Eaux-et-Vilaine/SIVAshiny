#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  #bslib::bs_themer()
  mod_vilaine_aval_server("mod_vilaine_aval_1")
  mod_barrage_server("mod_barrage_1")
}
