#' mod_passe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_passe_ui <- function(id){
  ns <- NS(id)
  tabPanel(title="Passe",
      icon=icon("fish"),
      sidebarLayout(
          sidebarPanel(width = 4, 
              dateInput(ns("pa_datedebut"),
                  label = "date de début :",
                  value = Sys.Date()-20,
                  language = "fr",
                  format = "dd/mm/yyyy"),
              dateInput(ns("pa_datefin"), 
                  label = "date de fin :", 
                  value =Sys.Date(),
                  language = "fr",
                  format = "dd/mm/yyyy"),                  
              shinyWidgets::actionBttn(
                  inputId = ns("bttn_pa"),
                  label = "OK",
                  style = "fill", 
                  color = "primary"					
              )
          ),
          mainPanel(width = 8,                       
              h3("Fonctionnement passe"),
              fluidRow(
                  column(width = 1, offset = 5,
                      shinyWidgets::actionBttn(
                          inputId = ns("bttn_pa_inf_inf"),
                          label = "<<",
                          style = "fill", 
                          color = "royal"					
                      )
                  ),
                  column(width = 1, 
                      shinyWidgets::actionBttn(
                          inputId = ns("bttn_pa_inf"),
                          label = "<",
                          style = "fill", 
                          color = "primary"					
                      )
                  ),
                  column(width = 1, 
                      shinyWidgets::actionBttn(
                          inputId = ns("bttn_pa_sup"),
                          label = ">",
                          style = "fill", 
                          color = "primary"					
                      )
                  ),
                  column(width = 1, 
                      shinyWidgets::actionBttn(
                          inputId = ns("bttn_pa_sup_sup"),
                          label = ">>",
                          style = "fill", 
                          color = "royal"					
                      )
                  )                  
              ),
              shinycssloaders::withSpinner(
                  plotly::plotlyOutput(outputId = ns("pa_plotly_passe_detail"))),
              h3("Débits"),
              shinycssloaders::withSpinner(plotly::plotlyOutput(
                      outputId = ns("pa_plotly_passe_debit")))              
          )
      )
  
  )
  
  
  
}

#' mod_passe Server Functions
#'
#' @noRd 
mod_passe_server <- function(id){
  moduleServer( id, function(input, output, session){
        ns <- session$ns
        pa <- reactiveValues(
            passe = NULL, # jeu de donné chargé en base
            ll = NULL,  # jeu de données calculé de fonctionnement
            fct_datedebut = NULL,
            fct_datefin =NULL)
           
        
        observeEvent(input$bttn_pa_inf_inf,{ 
              pa$fct_datedebut <- input$pa_datedebut
              pa$fct_datefin <- pmin(input$pa_datedebut+7, input$pa_datefin)
            })
        
        observeEvent(input$bttn_pa_sup_sup,{ 
              pa$fct_datedebut <- pmax(input$pa_datefin-7,input$pa_datedebut)
              pa$fct_datefin <- input$pa_datefin
            })
        
        observeEvent(input$bttn_pa_inf,{ 
              if (pa$fct_datedebut -7 < input$pa_datedebut) {
                pa$fct_datedebut <- input$pa_datedebut
                pa$fct_datefin <- input$pa_datedebut + 7
              } else {
              pa$fct_datedebut <- pa$fct_datedebut -7 
              pa$fct_datefin <- pa$fct_datefin -7 
              }
            })
        
        observeEvent(input$bttn_pa_sup,{ 
              if (pa$fct_datefin +7 > input$pa_datefin) {
                pa$fct_datedebut <- input$pa_datefin -7
                pa$fct_datefin <- input$pa_datefin 
              } else {
                pa$fct_datedebut <- pa$fct_datedebut + 7 
                pa$fct_datefin <- pa$fct_datefin + 7 
              }
            })
        
        observeEvent(input$bttn_pa,{
              # reset if changed
              pa$fct_date_debut = NULL
              pa$fct_date_fin = NULL
              validate(need(exists("pool"), "Il faut une connexion vers la base"))
              shinybusy::show_modal_spinner(text="chargement base")
              pa$passe <- load_passe(debut = as.POSIXct(strptime(input$pa_datedebut, format = "%Y-%m-%d")),
                  fin = as.POSIXct(strptime(input$pa_datefin, format = "%Y-%m-%d")),
                  con = pool)          
              shinybusy::remove_modal_spinner() 
            })
        
        output$pa_plotly_passe_detail <- plotly::renderPlotly({
              validate(need(!is.null(pa$passe), "cliquez sur le bouton OK pour charger des valeurs"))
              if (is.null(pa$fct_datedebut))   pa$fct_datedebut <- input$pa_datedebut
              if (is.null(pa$fct_datefin))   pa$fct_datefin <- input$pa_datedebut+7
              pa$ll <- calcul_fonct_pass(dat = pa$passe)              
              g <- plot_passe_detail(data_DF=pa$ll$dat, debut=pa$fct_datedebut, fin=pa$fct_datefin)
              return(plotly::ggplotly(g))
            })
        
        output$pa_plotly_passe_debit <- plotly::renderPlotly({
              validate(need(!is.null(pa$ll), "cliquez sur le bouton OK pour charger des valeurs"))
              ll_debit <- calcul_debit_passe(pa$ll)
              g <- plot_debit_passe(ll_debit)      
              return(plotly::ggplotly(g))
            })
        
      })
}

## To be copied in the UI
# mod_passe_ui("mod_passe_1")

## To be copied in the server
# mod_passe_server("mod_passe_1")
