#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #bslib::bs_themer() à décommenter pour choisir les thèmes
  # Vilaine aval ---------------------------------
  mod_vilaine_aval_server("mod_vilaine_aval_1")
  mod_passe_server("mod_passe_1")
  mod_isac_server("mod_isac_1")
  
  # Barrage, calculs utilisant les fonctions SIVA.
  # plus données toutes les dix minutes
  # Ce graphe ne se trouve pas dans un module, parce que les plotly et les reactives 
  # ne fonctionnent plus
  v <- reactiveValues(debits = NULL, niveaux = NULL, Qj=NULL, Q12345=NULL)
  observeEvent(input$bar_bttn,{
        #browser()
        #shinyCatch({
        validate(need(input$bar_datefin>input$bar_datedebut, "Le temps s'écoulant de manière linéaire, la date de fin est toujours ultérieure à la date de début"))
                validate(need(exists("pool"), "Il faut une connexion vers la base"))
        shinybusy::show_modal_spinner(text="chargement base") # show the modal window
        # on rajoute un jour avant
        #debit_barrage <- SIVA::rawdata2020
             debit_barrage <- load_debit_barrage (debut = as.POSIXct(
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
        v$Q12345 <- Q12345
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
      
# Barrage, graphique des niveaux toutes les dix minutes.
  
  output$bar_plotly_niveau_10 <- plotly::renderPlotly({  
        validate(need(input$bar_bttn, "choisir des dates"))
        validate(need(input$bar_datefin>input$bar_datedebut, "Le temps s'écoulant de manière linéaire, la date de fin est toujours ultérieure à la date de début"))
        
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
                  width = width
              ) %>%
              plotly::add_markers( color = ~source, colors = "Set1") %>%
              plotly::layout(dragmode = "select", selectdirection = "h") %>%
              event_register("plotly_selected")
                }
        
      })
  
# barrage : graphiqueplotly des débits par 10 minutes
# Un évennement va déclencher la recomposition du graphique avec un rectangle
# indiquant la période sélectionnée dans plotly_niveau_10
  
  output$bar_plotly_debit_10 <- plotly::renderPlotly({
        validate(need(input$bar_bttn, "choisir des dates"))
        validate(need(input$bar_datefin>input$bar_datedebut, "Le temps s'écoulant de manière linéaire, la date de fin est toujours ultérieure à la date de début"))
        
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
              mode = "markers")
          if (is.null(eventSelected)) {
            return(p) 
          } else {
            #browser()
            
            p <- layout(p=p ,title = sprintf("Sélection du \n %s au %s",min(eventSelected$x),max(eventSelected$x)),
                shapes = list(
                    list(type = "rect",
                        fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                        x0 =  min(eventSelected$x), x1 = max(eventSelected$x), xref = "x",
                        y0 = 0, y1 = max(v$debits$Q), yref = "y"))
            )              
            return(p) 
          }           
        }        
      })
  
  # Graphique des volumes journaliers sur la période sélectionnée,
  # voir SIVA plotly_volume_jour
  
  output$bar_plotly_volume_jour <- plotly::renderPlotly({
        validate(need(input$bar_bttn, "choisir des dates"))
        validate(need(input$bar_datefin>input$bar_datedebut, "Le temps s'écoulant de manière linéaire, la date de fin est toujours ultérieure à la date de début"))
        
        if(is.null(v$Qj)){
          return()
        } else { 
          p <- SIVA::plotly_volume_jour (QV=v$Qj) # dans la fonction il y a source = "volume_jour"
          
        }
      }
  )
  
  # Plotly des valeurs de la journée sélectionnée
  # eventclick$customdata renvoie la date 
  # le customdata est passé dans le dataset généré pour le graphe plotly_volume_jour
  # voir plotly_volume_jour
  output$bar_plotly_journalier_vanne_volet <- plotly::renderPlotly({
        validate(need(input$bar_bttn, "choisir des dates"))
        if(is.null(v$Q12345)){
          return()
        } else { 
          eventclick <- event_data("plotly_click", source = "volume_jour")
          validate(need(!is.null(eventclick),"cliquer sur un élément du graphe des volumes \n pour voir le détail des débits journaliers"))
          validate(need("customdata"%in%colnames(eventclick),"cliquez sur un des éléments colorés = volume (les élements en gris donnent le débit)"))
          SIVA::plotly_journalier_vanne_volet(date = eventclick$customdata, debit_traite=v$Q12345)
        }
      })
  
 # Tableau de données 
 # Utilise la variable réactive v$Qj = tableau des données journalières
  
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
}
