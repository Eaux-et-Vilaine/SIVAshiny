#' mod_vilaine_aval UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vilaine_aval_ui <- function(id){
  ns <- NS(id)
  tabPanel(title="Vilaine aval",
           icon=icon("water"),
           sidebarLayout(
             sidebarPanel(width = 4, 
                          dateInput(ns("va_datedebut"),
                                    label = "date de début :",
                                    value = Sys.Date()-20,
                                    language = "fr",
                                    format = "dd/mm/yyyy"),
                          dateInput(ns("va_datefin"), 
                                    label = "date de fin :", 
                                    value =Sys.Date(),
                                    language = "fr",
                                    format = "dd/mm/yyyy"),
                          checkboxGroupInput(ns("va_choix_station"), 
                                             label = h5("Choisissez les stations :"),
                                             choices = list(
                                               "Niveau Vilaine barrage" = "vilaine_barrage", 
                                               "Niveau mer barrage" = "mer_barrage",
                                               "Niveau Redon ecluse"="redon_ecluse",
                                               "Niveau Aucfer"="aucfer",
                                               "Niveau Molac" ="molac",
                                               "Niveau Le Gueslin"="legueslin",
                                               "Niveau Sixt"="sixtsuraff",
                                               "Niveau Pont de Cran"="pontdecran",
                                               "Niveau Guerouet"="guerouet",
                                               "Debit Pont de Cran"="debitcran"),
                                             inline=FALSE,
                                             selected = c("vilaine_barrage","redon_ecluse","debitcran")),
                          shinyWidgets::actionBttn(
                            inputId = ns("bttn_va"),
                            label = "OK",
                            style = "fill", 
                            color = "primary"					
                          )
             ),
             mainPanel(width = 8,                       
                       h3("Niveaux Vilaine"),
                       shinycssloaders::withSpinner(rAmCharts::amChartsOutput(outputId = ns("va_amchart"))),
                       h3("Debit"),
                       shinycssloaders::withSpinner(plotOutput(ns("va_plot_debits"))),
                       h3("Bilan journalier"),
                       shinycssloaders::withSpinner(DT::DTOutput(ns("va_data_table_bilan")))
             )
           )
           
  )
}

#' mod_vilaine_aval Server Functions
#'
#' @noRd 
mod_vilaine_aval_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$bttn_va,{
      #browser()
      #shinyCatch({
      validate(need(exists("pool"), "Il faut une connexion vers la base"))
      shinybusy::show_modal_spinner(text="chargement base") # show the modal window
      # TODO develop load tag
      niveaux2 <-
        load_niveaux(
          debut = as.POSIXct(strptime("2021-01-01 00:00:00",
                                      format = "%Y-%m-%d %H:%M:%S")),
          fin = as.POSIXct(strptime("2021-01-10 00:00:00",
                                    format = "%Y-%m-%d %H:%M:%S")),
          tags = c(2507, 2508, 2100, 1000, 
                   1100,1300,1400,1902,2000),
          con = pool
        )
      shinybusy::remove_modal_spinner() # remove it when done
      output$va_amchart <- renderAmCharts({
          rAmCharts::amTimeSeries(
              niveaux2, 
            'horodate',
            c("vilaine_barrage",
              "mer_barrage",
              "redon_ecluse",
              "aucfer",
              "molac",
              "legueslin",
              "sixtsuraff",
              "pontdecran",
              "guerouet"
            ),
            bullet = "round",
            color =   randomcoloR::distinctColorPalette(9),
            #backgroundColor = "#40555E",
            #backgroundAlpha = 0.4,
            bulletSize =  4,
            aggregation = "Average",
            fillAlphas = 0.1,
            groupToPeriods = c('10mm', '30mm', 'hh', 'DD', 'MM', 'MAX'),
            #  c('hh', 'DD', '10DD','MM','MAX'),
            linewidth = 0.2,
            legend = TRUE,
            # maxSeries = 200,
            categoryAxesSettings.minPeriod = "30mm"
          ) %>%
          setExport(enabled = TRUE)  
      })
    })
  })
}

## To be copied in the UI
# mod_vilaine_aval_ui("mod_vilaine_aval_1")

## To be copied in the server
# mod_vilaine_aval_server("mod_vilaine_aval_1")
