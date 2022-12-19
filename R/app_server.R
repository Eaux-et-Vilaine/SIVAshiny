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
  v <- reactiveValues(debits = NULL, niveaux = NULL, Qj=NULL)
  
  observeEvent(input$bar_bttn,{
        #browser()
        #shinyCatch({
        validate(need(exists("pool"), "Il faut une connexion vers la base"))
        shinybusy::show_modal_spinner(text="chargement base") # show the modal window
        # on rajoute un jour avant
        debit_barrage <- SIVA::rawdata2020
#            load_debit_barrage (debut = as.POSIXct(
#                    strptime(input$bar_datedebut-1, format = "%Y-%m-%d")
#                ),
#                fin = as.POSIXct(
#                    strptime(input$bar_datefin, format = "%Y-%m-%d")
#                ),
#                con=pool)
        shinybusy::remove_modal_spinner() # remove it when done
        shinybusy::show_modal_spinner(text="calcul")
        debit_barrage <-traitement_siva(debit_barrage)
        Q12345 <- debit_total(param, param0 = param, debit_barrage)
        Q12345$tot_vol <- debit_barrage$tot_vol # volume total au barrage d'Arzal = somme des totaliseurs de volume
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
        niveaux$nms=str_c(niveaux$source,"_", 1:nrow(niveaux))
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
        debits$nms=str_c(debits$source,"_", 1:nrow(debits))
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
                  source = "niveaux",
                  x= ~horodate,
                  y= ~niveau,
                  height = height, 
                  width = width,
                  customdata = ~ nms
              ) %>%
              plotly::add_markers( color = ~source, colors = "Set1") %>%
              plotly::layout(dragmode = "select", selectdirection = "h") %>%
              event_register("plotly_selected")
          
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
              customdata = ~ nms,
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
  
  output$click <- renderPrint({
        d <- event_data("plotly_click")
        if (is.null(d)) "Click events appear here (double-click to clear)" else d
      })
  
  
  output$selecting <- renderPrint({
        d <- event_data("plotly_selecting")
        if (is.null(d)) "Brush points appear here (double-click to clear)" else d
      })
  
  
  output$selected <- renderPrint({
        d <- event_data("plotly_selected")
        if (is.null(d)) "Brushed points appear here (double-click to clear)" else d
      })
  
  eventSelecing <- reactive(event_data("plotly_selecting", source = "bar_plotly_niveaux")) #%>% debounce(500)
  observeEvent(eventSelecing,
      {
        #browser()
        d <-  eventSelecing()
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
  
}
