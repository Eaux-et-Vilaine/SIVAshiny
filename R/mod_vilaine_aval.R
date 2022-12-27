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
              checkboxGroupInput(ns("va_choix_niveau"), 
                  label = h5("Niveaux :"),
                  choices = list(
                      "Niveau Vilaine barrage" = "vilaine_barrage", 
                      "Niveau mer barrage" = "mer_barrage",
                      "Niveau Redon ecluse"= "redon_ecluse",
                      "Niveau Aucfer"="aucfer",
                      "Niveau Molac" ="molac",
                      "Niveau Le Gueslin"="legueslin",
                      "Niveau Sixt"="sixtsuraff",
                      "Niveau Pont de Cran"="pontdecran",
                      "Niveau Guerouet"="guerouet"
                  ),
                  inline=FALSE,
                  selected = c("vilaine_barrage","redon_ecluse","debitcran")),
              checkboxGroupInput(ns("va_choix_debit"), 
                  label = h5("Débits :"),
                  choices = list(
                      "Débit Oust (Ile aux pies)"= "ile_aux_pies_debit",
                      "Débit au Pont de Cran" = "pont_de_cran_debit",
                      "Débit Vilaine estimé" = "arzal_debit_vilaine_estime",
                      "Débit Vilaine à Langon" = "langon_debit",
                      "Débit recalculé SIVA *" = "barrage_debit_recacule",
                      "Débit total Arzal 1-5 *"= "debit_barQ"),
                  inline=FALSE,
                  selected = c("pont_de_cran_debit","arzal_debit_vilaine_estime")),
              p("* Attention plus long :  "),
              p("  demande le chargement de 34 variables"),
              shinyWidgets::actionBttn(
                  inputId = ns("bttn_va"),
                  label = "OK",
                  style = "fill", 
                  color = "primary"					
              )
          ),
          mainPanel(width = 8,                       
              h3("Niveaux Vilaine"),
              shinycssloaders::withSpinner(rAmCharts::amChartsOutput(outputId = ns("va_armchart_niveaux"))),
              h3("Débits"),
              shinycssloaders::withSpinner(rAmCharts::amChartsOutput(outputId = ns("va_armchart_debits"))),
           
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
        # codes <- 
        va <- reactiveValues(niveaux = NULL, debits=NULL)
        get_tags <- function(type=c("niveau", "debit")){
          type = match.arg(type)
          if (type == "niveau"){            
            codes <- input$va_choix_niveau
            if (!all(codes%in%SIVA::niveau$code)) stop(
                  sprintf("Erreur interne, code %s niveau pas dans la liste des codes", 
                      codes[!codes%in%SIVA::niveau$code]))
            tags <- SIVA::niveau[SIVA::niveau$code %in% codes,"tag"] %>% pull()
            return(tags)
          } else  if (type == "debit"){
            codes <- input$va_choix_debit  
            if (!all(codes%in%SIVA::debit$code)) stop(
                  sprintf("Erreur interne, code %s debit pas dans la liste des codes", 
                      codes[!codes%in%SIVA::debit$code]))
            tags <- SIVA::debit[SIVA::debit$code %in% codes,"tag"] %>% pull()
            return(tags)
          }    else {
            stop("type doit être soit niveau, soit debit")
          }
        }
        
        observeEvent(input$bttn_va,{
           
              #shinyCatch({
              validate(need(exists("pool"), "Il faut une connexion vers la base"))
              shinybusy::show_modal_spinner(text="chargement base") # show the modal window
       
              va$niveaux <-
                  load_niveaux(
                      debut = as.POSIXct(strptime(input$va_datedebut-1, format = "%Y-%m-%d")),
                      fin = as.POSIXct(strptime(input$va_datefin, format = "%Y-%m-%d")),
                      tags = get_tags(type="niveau") ,
                      con = pool
                  )
              va$debits <- load_debits(debut = as.POSIXct(strptime(input$va_datedebut-1, format = "%Y-%m-%d")),
                  fin = as.POSIXct(strptime(input$va_datefin, format = "%Y-%m-%d")),
                  tags = get_tags(type="debit") ,
                  con = pool)
             
              shinybusy::remove_modal_spinner() # remove it when done
              # TODO use attributes(niveaux)$libelle 
             
            })
        output$va_armchart_niveaux <- renderAmCharts({
              validate(need(!is.null(va$niveaux), "cliquez sur le bouton OK pour charger des valeurs"))
              rAmCharts::amTimeSeries(
                      va$niveaux, 
                      'horodate',
                      colnames(va$niveaux)[2:ncol(va$niveaux)],
                      bullet = "round",
                      color =   randomcoloR::distinctColorPalette(ncol(va$niveaux)-1),
                      #backgroundColor = "#40555E",
                      #backgroundAlpha = 0.4,
                      bulletSize =  4,
                      aggregation = "Average",
                      fillAlphas = 0.2,
                      groupToPeriods = c('10mm', '30mm', 'hh', 'DD', 'MAX'),
                      #  c('hh', 'DD', '10DD','MM','MAX'),
                      linewidth = 0.2,
                      legend = TRUE           
                  ) %>%
                  rAmCharts::setExport(enabled = TRUE)  
            })
        
        output$va_armchart_debits <- renderAmCharts({
              validate(need(!is.null(va$debits), "cliquez sur le bouton OK pour charger des valeurs"))
             rAmCharts::amTimeSeries(
                      va$debits,
                      'horodate',
                      colnames(va$debits)[2:ncol(va$debits)],
                      bullet = "round",
                      color =   randomcoloR::distinctColorPalette(ncol(va$debits)-1),
                      #backgroundColor = "#40555E",
                      #backgroundAlpha = 0.4,
                      bulletSize =  4,
                      aggregation = "Average",
                      fillAlphas = 0.2,
                      groupToPeriods = c('10mm', '30mm', 'hh', 'DD', 'MAX'),
                      #  c('hh', 'DD', '10DD','MM','MAX'),
                      linewidth = 0.2,
                      legend = TRUE           
                  ) %>%
                  rAmCharts::setExport(enabled = TRUE)  
            })
      })
}

## To be copied in the UI
# mod_vilaine_aval_ui("mod_vilaine_aval_1")

## To be copied in the server
# mod_vilaine_aval_server("mod_vilaine_aval_1")
