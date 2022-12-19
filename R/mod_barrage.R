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
mod_barrage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #
    observeEvent(input$bar_bttn,{
      #browser()
      #shinyCatch({
    validate(need(exists("pool"), "Il faut une connexion vers la base"))
    shinybusy::show_modal_spinner(text="chargement base") # show the modal window
    debit_barrage <-
      load_debit_barrage (debut = as.POSIXct(
        strptime(input$bar_datedebut, format = "%Y-%m-%d")
      ),
      fin = as.POSIXct(
        strptime(input$bar_datefin, format = "%Y-%m-%d")
      ),
      con=pool)
    shinybusy::remove_modal_spinner() # remove it when done
    shinybusy::show_modal_spinner(text="calcul")
    debit_barrage <-traitement_siva(debit_barrage)
    Q12345 <- debit_total(param, param0 = param, debit_barrage)
    Q12345$tot_vol <- debit_barrage$tot_vol # volume total au barrage d'Arzal
    Q12345$volet_vanne <-
      debit_barrage$tot_vol_barrage + debit_barrage$tot_vol_volet # volume total toutes les dix minutes sur volets et vannes
    

    Qj <-
      as.data.frame(
        dplyr::select(
          Q12345,
          Q,
          date,
          volvoletcalcule,
          debit_moyen_cran,
          tot_vol,
          volet_vanne,
          tot_vol_siphon,
          tot_vol_passe
        ) %>%
          dplyr::group_by(date) %>%
          dplyr::summarize(
            volumevilainecalcule = 
              sum(Q * 600, volvoletcalcule, tot_vol_passe, tot_vol_siphon, na.rm =TRUE),
            tot_vol = sum(tot_vol, na.rm = TRUE),
            volet_vanne = sum(volet_vanne),
            debit_moyen_cran = mean(debit_moyen_cran)
          ) %>%
          dplyr::mutate(
            debitvilainecalcule = volumevilainecalcule / (24 * 60 * 60),
            tot_bar = tot_vol / (24 * 60 * 60),
            tot_volvan_bar = volet_vanne / (24 * 60 * 60)
          )
      )
    niveaux <- debit_barrage %>% select(horodate, niveauvilaineb,
                                        niveaumerb) %>%
      rename(horodate = horodate) %>% # todo get rid of horodate in code
      pivot_longer(
        cols = c("niveauvilaineb", "niveaumerb"),
        names_to = "source",
        names_prefix = "niveau",
        values_to = "niveau"
      )
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
    g1 <- ggplot()+ geom_line(aes(x=horodate, y=niveau, col=source), data=niveaux) 
    g2 <- ggplot()+  geom_line(aes(x=horodate, y=Q, col=vanne), data=debits_vannes)+ 
      geom_line(aes(x=horodate, y=Q, col=volet), data=debits_volets)
    shinybusy::remove_modal_spinner()
    # sorties graphiques 
    
    output$bar_plot_niveaux <- renderPlot({
      g1
    })
    output$bar_plot_debits <- renderPlot({
      g2
    })
    output$bar_Qj <- DT::renderDT({
      DT::datatable(Qj,
                    rownames=TRUE,          
                    extensions = "Buttons",
                    option=list(
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "500px",
                      order=list(3,"asc"),
                      lengthMenu=list(c(-1,5,20,50),c("All","5","20","50")),
                      "pagelength"=-1,
                      dom= "Blfrtip",
                      scrollX = T, 
                      buttons=list(
                        list(extend="excel",
                             filename = "Qj"))
                    ))
    })
    },    ignoreInit = TRUE)
 
  })
}
    
## To be copied in the UI
# mod_barrage_ui("mod_barrage_1")
    
## To be copied in the server
# mod_barrage_server("mod_barrage_1")
