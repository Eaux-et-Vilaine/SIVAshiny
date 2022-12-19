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
                  value = Sys.Date()-7,
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
              h3("Graphiques de niveaux"),
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("bar_plotly_niveaux"))),
              h3("Graphiques de debit"),
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("bar_plotly_debits"))),
              h3("Débits barrage"),
              shinycssloaders::withSpinner(DT::DTOutput(ns("bar_Qj")))
          )
      )
  
  )
}

#' mod_barrage Server Functions
#'
#' @noRd 
mod_barrage_server <- function(id){
  moduleServer( id, function(input, output, session){
        ns <- session$ns
        v <- reactiveValues(debits = NULL, niveaux = NULL, Qj=NULL)
     
        observeEvent(input$bar_bttn,{
              #browser()
              #shinyCatch({
              validate(need(exists("pool"), "Il faut une connexion vers la base"))
              shinybusy::show_modal_spinner(text="chargement base") # show the modal window
              # on rajoute un jour avant
              debit_barrage <-
                  load_debit_barrage (debut = as.POSIXct(
                          strptime(input$bar_datedebut-1, format = "%Y-%m-%d")
                      ),
                      fin = as.POSIXct(
                          strptime(input$bar_datefin, format = "%Y-%m-%d")
                      ),
                      con=pool)
              shinybusy::remove_modal_spinner() # remove it when done
              shinybusy::show_modal_spinner(text="calcul")
              debit_barrage <-traitement_siva(debit_barrage)
              Q12345 <- debit_total(param, param0 = param, debit_barrage)
              Q12345$tot_vol <- debit_barrage$tot_vol # volume total au barrage d'Arzal = somme des totaliseurs de volume
              Q12345$volet_vanne <-
                  debit_barrage$tot_vol_barrage + debit_barrage$tot_vol_volet # volume total toutes les dix minutes sur volets et vannes
              
              
              Qj <-
                  as.data.frame(
                      Q12345 %>%dplyr::select(                             ,
                              Q,
                              date,
                              volvoletcalcule,
                              debit_moyen_cran,
                              tot_vol,
                              volet_vanne,
                              tot_vol_siphon,
                              tot_vol_passe,
                              tot_vol_ecluse
                          ) %>%
                          dplyr::group_by(date) %>%
                          dplyr::summarize(
                              volume_recalcule = 
                                  sum(Q * 600, volvoletcalcule, tot_vol_passe, tot_vol_siphon, tot_vol_ecluse, na.rm =TRUE),
                              tot_vol_bar = sum(tot_vol, na.rm = TRUE),
                              volet_vanne_bar = sum(volet_vanne),
                              debit_moyen_cran = mean(debit_moyen_cran)
                              
                          ) %>%
                          dplyr::mutate(
                              debitvilainecalcule = volume_recalcule / (24 * 60 * 60),
                              debit_tot_vol_bar = tot_vol_bar / (24 * 60 * 60),
                              debit_volvan_bar = volet_vanne / (24 * 60 * 60)
                          )
                  )
              Qj <- Qj %>% 
                  mutate(across(where(is.numeric), round)) %>%
                  arrange(date) %>%
                  slice(-1)
              v$Qj <- Qj
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
                      names_to = "source",
                      names_prefix = "qvanne",
                      values_to = "Q"
                  )
              debits_volets <-
                  Q12345 %>% select(horodate, starts_with("Qvolet")) %>%
                  pivot_longer(
                      cols = starts_with("Qvolet"),
                      names_to = "source",
                      names_prefix = "Qvolet",
                      values_to = "Q"
                  )
              debits <- bind_rows(debits_vannes, debits_volets)
              v$debits <- debits
#              g1 <- ggplot()+ geom_line(aes(x=horodate, y=niveau, col=source), data=niveaux) + theme_bw()
#              g2 <- ggplot() +  geom_line(aes(x=horodate, y=Q, col=source), data=bind_rows(debits_vannes, debits_volets)) + 
#                  theme_bw()
              shinybusy::remove_modal_spinner()
              # sorties graphiques 
              
              
              
            },    ignoreInit = TRUE)
        
        output$bar_plotly_niveaux <- plotly::renderPlotly({
              if(is.null(v$niveaux)){
                return()
              } else {
                height <- session$clientData$output_p_height
                width <- session$clientData$output_p_width
                p <- v$niveaux%>% plotly::plot_ly(
                        x= ~horodate,
                        y= ~niveau,
                        height = height, 
                        width = width
                    ) %>%
                    plotly::add_markers( color = ~source, colors = "Set1") %>%
                    plotly::layout(dragmode = "select", selectdirection = "h") 

#                    brush <- plotly::event_data("plotly_brushing", source = "bar_plotly_debits")
#                    if (is.null(brush)) return(p)
#                    cat("brush activated")
#                    browser()
#                    p %>%
#                        dplyr::filter(between(horodate, brush$x[1], brush$x[2])) %>%
#                        plotly::add_lines() %>%                       
#                        plotly::event_register('plotly_brushing')
              }
            })
        
        
        
        output$bar_plotly_debits <- plotly::renderPlotly({
              if(is.null(v$debits)){
                return()
              } else {
                height <- session$clientData$output_p_height
                width <- session$clientData$output_p_width                
                p <- v$debits%>% plotly::plot_ly(
                    x= ~horodate,
                    y= ~Q,
                    height = height, 
                    width = width,
                    color = ~source, 
                    colors = randomcoloR::distinctColorPalette(length(unique(v$debits$source))),
                    mode = "markers") #%>%
               # plotly::layout(plot_bgcolor='black', paper_bgcolor='black', margin = list(b=30, l=0, r=10, t=30))
                #sel <- plotly::event_data("plotly_selected", source = "bar_plotly_niveaux")
                return(p) 
     
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
                DT::datatable(,
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
        
      
        eventSelected <- reactive(event_data("plotly_click", source = "bar_plotly_niveaux")) %>% debounce(500)
        observeEvent(eventSelected,
            {
              browser()
              d <-  eventSelected()
              if (rlang::is_empty(d)) {
                return()
              }
              if (!is.null(d) || rlang::is_empty(d)) {
                browser()
                new_ids <- c(ids(), evt %>% pull(key)) %>% unique()
                ids(new_ids) 
              }
            
              p <- plotly::plotlyProxy("bar_plotly_debits", session)
              # show full data if no brush exists
              if (is.null(d)) {
                plotly::plotlyProxyInvoke(p, "plotly", "restyle", list(v$debits$Q))
                return()
              }
              debits_filter <- v$debits %>% dplyr::filter(between(horodate, brush$x[1], brush$x[2]))
              if (nrow(d_filter) < 10) return()
              
              
              plotly::plotlyProxyInvoke(p, "restyle", "y", list(debits_filter$Q))
            })
        
        
        
        
      })
}

## To be copied in the UI
# mod_barrage_ui("mod_barrage_1")

## To be copied in the server
# mod_barrage_server("mod_barrage_1")
