#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  #bslib::bs_themer()
  output$plot_niveaux <- renderPlot({
    random_ggplot()
  })
  output$plot_debits <- renderPlot({
    random_ggplot()
  })
  output$data_table_bilan <- DT::renderDT({
    random_DT(5, 5)
  })
}
