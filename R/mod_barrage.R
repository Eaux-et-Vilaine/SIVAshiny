#' mod_barrage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_barrage_ui <- function(id){
  ns <- NS(id)
  tabPanel(title="Barrage",
      icon=icon("poll"),
      sidebarLayout(
          sidebarPanel(width = 4, 
              dateInput(ns("bar_datedebut"),
                  label = "date de début :",
                  value = Sys.Date()-20,
                  language = "fr",
                  format = "dd/mm/yyyy"),
              dateInput(ns("bar_datefin"), 
                  label = "date de fin :", 
                  value =Sys.Date(),
                  language = "fr",
                  format = "dd/mm/yyyy"),
              shinyWidgets::actionBttn(
                  inputId = ns("bar_bttn"),
                  label = "OK",
                  style = "fill", 
                  color = "primary"					
              )
          ),
          mainPanel(width = 8,   
              verbatimTextOutput(ns("selected")),
              h3("Graphiques de niveaux"),
              shinycssloaders::withSpinner(plotOutput(ns("bar_plot_niveaux"))),
              h3("Graphiques de debit"),
              shinycssloaders::withSpinner(plotOutput(ns("bar_plot_debits"))),
              h3("Débits barrage"),
              shinycssloaders::withSpinner(DT::DTOutput(ns("bar_Qj")))
          )
      )
  
  )
}

#' mod_barrage Server Functions
#'
#' @noRd 
mod_barrage_server <- function(id, v){
  moduleServer( id, function(input, output, session){
        ns <- session$ns
        v <- reactiveValues(debits = NULL, niveaux = NULL, Qj=NULL)
        
        observeEvent(input$bar_bttn,{
              
              #shinyCatch({
              validate(need(exists("pool"), "Il faut une connexion vers la base"))
              shinybusy::show_modal_spinner(text="chargement base") # show the modal window
              debit_barrage <- SIVA::rawdata2020
#    debit_barrage <-
#      load_debit_barrage (debut = as.POSIXct(
#        strptime(input$bar_datedebut, format = "%Y-%m-%d")
#      ),
#      fin = as.POSIXct(
#        strptime(input$bar_datefin, format = "%Y-%m-%d")
#      ),
#      con=pool)
              shinybusy::remove_modal_spinner() # remove it when done
              shinybusy::show_modal_spinner(text="calcul")
              debit_barrage <-traitement_siva(debit_barrage)
              Q12345 <- debit_total(param, param0 = param, debit_barrage)
              Q12345$tot_vol <- debit_barrage$tot_vol # volume total au barrage d'Arzal
              
              
              Qj <- debit_journalier(debit_barrage=debit_barrage, type = "recalcule")
              Q2j <- debit_journalier(debit_barrage=debit_barrage, type = "barrage_volume")
              Q3j <- debit_journalier(debit_barrage=debit_barrage, type = "barrage_debit")
              stopifnot(nrow(Qj) == nrow(Q2j))
              stopifnot(nrow(Q2j) == nrow(Q3j))
              QV <- bind_cols(Qj, Q2j %>% select(-date), Q3j %>% select(-date))
              
              
              v$Qj <- QV
              niveaux <- debit_barrage %>% select(horodate, niveauvilaineb,
                      niveaumerb) %>%
                  rename(horodate = horodate) %>% # todo get rid of horodate in code
                  pivot_longer(
                      cols = c("niveauvilaineb", "niveaumerb"),
                      names_to = "source",
                      names_prefix = "niveau",
                      values_to = "niveau"
                  )
              v$niveaux <- niveaux
              debits_vannes <-
                  Q12345 %>% select(horodate, starts_with("Qvanne")) %>%
                  pivot_longer(
                      cols = starts_with("Qvanne"),
                      names_to = "vanne",
                      names_prefix = "qvanne",
                      values_to = "Q"
                  )
              debits_volets <-
                  Q12345 %>% select(horodate, starts_with("Qvolet")) %>%
                  pivot_longer(
                      cols = starts_with("Qvolet"),
                      names_to = "volet",
                      names_prefix = "Qvolet",
                      values_to = "Q"
                  )
              debits <- bind_rows(debits_vannes, debits_volets)
              v$debits <- debits
#    g1 <- ggplot()+ geom_line(aes(x=horodate, y=niveau, col=source), data=niveaux) 
#    g2 <- ggplot()+  geom_line(aes(x=horodate, y=Q, col=vanne), data=debits_vannes)+ 
#      geom_line(aes(x=horodate, y=Q, col=volet), data=debits_volets)
              shinybusy::remove_modal_spinner()
              # sorties graphiques 
            })
        output$bar_plotly_niveaux <- plotly::renderPlotly({
              if(is.null(v$niveaux)){
                return()
              } else {
                cat("v niveaux OK")
                height <- session$clientData$output_p_height
                width <- session$clientData$output_p_width
                
                p <- v$niveaux%>% plotly::plot_ly(
                        source = "niveaux", # ATTENTION C'EST LE LIEN
                        x= ~horodate,
                        y= ~niveau,
                        height = height, 
                        width = width
                    ) %>%
                    plotly::add_markers( color = ~source, colors = "Set1") %>%
                    plotly::layout(dragmode = "select", selectdirection = "h") %>%
                    event_register("plotly_selected")     
                return(p)
              }
            })
        
        
        
        output$bar_plotly_debits <- plotly::renderPlotly({
              if(is.null(v$debits)){
                return()
              } else {
                cat("v debits OK")
                eventSelected <- event_data("plotly_selected", source = "niveaux")
                height <- session$clientData$output_p_height
                width <- session$clientData$output_p_width                
                p <- v$debits%>% plotly::plot_ly(
                    x= ~horodate,
                    y= ~Q,
                    height = height, 
                    width = width,
                    color = ~source, 
                    colors = randomcoloR::distinctColorPalette(length(unique(v$debits$source))),
                    mode = "markers")
                if (is.null(eventSelected)) {
                  return(p) 
                } else {
                  #browser()
                  
                  p <- layout(p=p ,title = 'Données sélectionnées',
                      shapes = list(
                          list(type = "rect",
                              fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                              x0 =  min(eventSelected$x), x1 = max(eventSelected$x), xref = "x",
                              y0 = 0, y1 = max(v$debits$Q), yref = "y"))
                  )
                  
                  
                  return(p) 
                }
#                    p %>%
#                        dplyr::filter(between(horodate, sel$x[1], sel$x[2])) %>%
#                        plotly::add_lines() 
              }
            })
        output$bar_Qj <- DT::renderDT({
              if(is.null(v$Qj)){
                return()
              } else {
                Qj <- v$Qj 
                DT::datatable(Qj,
                    rownames=FALSE,          
                    extensions = "Buttons",
                    option=list(
                        scroller = TRUE,
                        scrollX = TRUE,
                        scrollY = "500px",                        
                        lengthMenu=list(c(-1,5,20,50),c("All","5","20","50")),
                        "pagelength"=-1,
                        dom= "Blfrtip",
                        scrollX = T, 
                        buttons=list(
                            list(extend="excel",
                                filename = "Qj"))))                    
                
                
              }
            })
        
        
        output$selected <- renderPrint({
              d <- event_data("plotly_selected")
              if (is.null(d)) "Brushed points appear here (double-click to clear)" else d
            })
        
        
        
        
      }) 
return(v)}
## To be copied in the UI
# mod_barrage_ui("mod_barrage_1")

## To be copied in the server
# mod_barrage_server("mod_barrage_1")
