#' isac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_isac_ui <- function(id){
  ns <- NS(id)
  tabPanel(title="Isac",
      icon=icon("leaf"),
      sidebarLayout(
          sidebarPanel(width = 3, 
              dateInput(ns("isa_datedebut"),
                  label = "date de début :",
                  value = Sys.Date()-20,
                  language = "fr",
                  format = "dd/mm/yyyy"),
              dateInput(ns("isa_datefin"), 
                  label = "date de fin :", 
                  value =Sys.Date(),
                  language = "fr",
                  format = "dd/mm/yyyy"), 
              checkboxGroupInput(ns("isa_choix"), 
                  label = h5("Variables :"),
                  choices = list(
                      "Niveau Pont de Cran"="pontdecran",
                      "Niveau Isac Guenrouet"="guenrouet",
                      "Niveau Redon ecluse"= "redon_ecluse",
                      "Niveau Vilaine barrage" = "vilaine_barrage",
                      "Niveau Marais Isac" = "isac_niveau_marais",
                      "Niveau aval Isac 1 (Thénot)" = "isac_aval1",
                      "Niveau amont Isac 1 (Thénot) = isac_amont1",
                      "Niveau aval Isac 2 (Thénot)" = "isac_aval2",
                      "Niveau amont Isac 2 (Thénot)" = "isac_amont2",
                      "Fonct. cumulé pompe 1" = "isac_fonctionnement_cumul_p1",
                      "Fonct. cumulé pompe 3" = "isac_fonctionnement_cumul_p3",
                      "Position vanne 1" = "isac_position_vanne_1",
                      "Position vanne 2" = "isac_position_vanne_2",
                      "Pluviometrie barrage Arzal"="pluvio_barrage_10min",
                      "Pluviometrie Isac"="pluvio_isac_10min"),           
                  inline=FALSE,
                  selected = c("pontdecran",
                      "guenrouet",
                      "isac_niveau_marais",
                      "isac_amont2",
                      "isac_fonctionnement_cumul_p1",
                      "isac_fonctionnement_cumul_p3",
                      "isac_position_vanne_1",
                      "isac_position_vanne_2",
                      "pluvio_barrage_10min",
                      "pluvio_isac_10min"
                  )
              ),
              shinyWidgets::actionBttn(
                  inputId = ns("bttn_isa"),
                  label = "OK",
                  style = "fill", 
                  color = "primary"					
              )
          ), #end sidebarpanel
          mainPanel(width = 9,  
              fluidRow(
                  valueBoxOutput(ns("box_pluvio"), width=3),
                  valueBoxOutput(ns("box_debit_pompes"), width=3),
                  valueBoxOutput(ns("box_volume_pompes"), width=3)
              ),
              fluidRow(
                  valueBoxOutput(ns("box_cout"), width=3),
                  valueBoxOutput(ns("box_cout_carbone"), width=3),
                  valueBoxOutput(ns("box_delta_h"), width=3)
              ),  
              h3("Niveau"),
              shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                      outputId = ns("isac_rmarchart_niveau")
                  )              
              ),
              shinycssloaders::withSpinner(                    
                  plotly::plotlyOutput(
                      outputId = ns("isa_fonct_thenot")))
          )# end main panel
      )) 
}

#' isac Server Functions
#'
#' @noRd 
mod_isac_server <- function(id){
  moduleServer( id, function(input, output, session){
        ns <- session$ns
        va <- reactiveValues(isac_dat = NULL)
        
        get_tags <- function(){          
          codes <- input$isa_choix
          if (!all(codes%in%SIVA::isac$code)) stop(
                sprintf("Erreur interne, code %s pas dans la liste des codes pour l'Isac", 
                    codes[!codes%in%SIVA::isac$code]))
          tags <- SIVA::isac[SIVA::isac$code %in% codes,"tag"] %>% pull()
          return(tags)          
        }
        
        observeEvent(input$bttn_isa,{              
              
              validate(need(exists("pool"), "Il faut une connexion vers la base"))
              tags_isac <- get_tags()               
              
              if (length(tags_isac)>0){
                shinybusy::show_modal_spinner(text="chargement base") # show the modal window
                isac_dat <-
                    load_isac(
                        debut = as.POSIXct(strptime(input$isa_datedebut, format = "%Y-%m-%d")),
                        fin = as.POSIXct(strptime(input$isa_datefin, format = "%Y-%m-%d")),
                        tags = tags_isac,
                        con = pool
                    )
                shinybusy::remove_modal_spinner()
                # calculs des débits et conso + filtration données
               
                isac_dat <- traitement_isac(isac_dat)
                
                
              } else {
                isac_dat <- data.frame()
              }                            
              
              # filtrage des données, calcul des débits, temps et coûts
              
              
              
              va$isac_dat <- isac_dat   
            })
        
        output$box_pluvio <- renderValueBox({
              validate(need(!is.null(va$isac_dat), ""))              
              if(nrow(va$isac_dat)>0){                
                if ("pluvio_isac_10min" %in% colnames(va$isac_dat)){
                  pluvio_isac <- paste(sum(va$isac_dat$pluvio_isac_10min, na.rm=TRUE),"mm")
                } else {
                  pluvio_isac <- "Non chargée"
                }
                if ("pluvio_barrage_10min" %in% colnames(va$isac_dat)){
                  pluvio_barrage <- paste(sum(va$isac_dat$pluvio_barrage_10min, na.rm=TRUE),"mm")
                } else {
                  pluvio_barrage <- "Non chargée"
                }
                string_pluvio <- sprintf("Isac %s <br/> Arzal %s",pluvio_isac,pluvio_barrage)    
                valueBox(
                    value=HTML(string_pluvio),
                    subtitle=p("Pluviométrie",style = 'color:GhostWhite;font-size: 150%;'),
                    icon=icon("cloud-rain"),
                    color="purple")
              } else {
                valueBox(
                    value="Pas de données",
                    subtitle="Pluvio totale isac",
                    icon=icon("cloud-rain"),
                    color="black")
              }
            })
        output$box_debit_pompes <- renderValueBox({
              validate(need(!is.null(va$isac_dat), ""))              
              if(nrow(va$isac_dat)>0){                
                if (all(c("isac_amont2",
                            "guenrouet",
                            "isac_fonctionnement_cumul_p1",
                            "isac_fonctionnement_cumul_p3") %in%
                        colnames(va$isac_dat))){
                  Q_pompes <- paste(mean(va$isac_dat$Q_pompes, na.rm=TRUE),"m3/s")
                } else {
                  Q_pompes <- "Non calculé"
                }                
                  valueBox(
                    value=p(Q_pompes, style = 'color:LemonChiffon;font-size: 100%;'), 
                    subtitle=h5("Débit pompes", style='color:LightCyan'),
                    icon=icon("arrow-up-from-water-pump"), 
                    color="navy"
                )
              } else {
                valueBox(
                    value="",
                    subtitle="Débit pompes",
                    icon=icon("arrow-up-from-water-pump"), 
                    color="aqua"
                )
              }     
            })
        
        output$box_volume_pompes <- renderValueBox({
              validate(need(!is.null(va$isac_dat), ""))              
              if(nrow(va$isac_dat)>0){                
                if (all(c("isac_amont2",
                            "guenrouet",
                            "isac_fonctionnement_cumul_p1",
                            "isac_fonctionnement_cumul_p3") %in%
                        colnames(va$isac_dat))){
                  V_pompes <- paste(sum(va$isac_dat$Q_pompes*600, na.rm=TRUE),"m3/s")
                } else {
                  V_pompes <- "Non calculé"
                }                
                valueBox(
                    value=p(V_pompes, style = 'color:Navy;font-size: 100%;'), 
                    subtitle=h5("Volume pompes",style='color:MidnightBlue;'),
                    icon=icon("arrow-up-from-water-pump"), 
                    color="aqua"
                )
              } else {
                valueBox(
                    value="",
                    subtitle="Volume pompes",
                    icon=icon("arrow-up-from-water-pump"), 
                    color="aqua"
                )
              }     
            })
        
        output$box_cout <- renderValueBox({
              validate(need(!is.null(va$isac_dat), ""))              
              if(nrow(va$isac_dat)>0){                
                if (all(c("isac_amont2",
                            "guenrouet",
                            "isac_fonctionnement_cumul_p1",
                            "isac_fonctionnement_cumul_p3") %in%
                        colnames(va$isac_dat))){
                  cout <- paste(sum(va$isac_dat$cout_euros, na.rm=TRUE),"€")
                } else {
                  cout <- "Non calculé"
                }                
                valueBox(
                    value=h3(cout, style = 'color:DarkGreen;'),
                    subtitle="Coût en électricité des pompes €",
                    icon=icon("euro-sign"), 
                    color="teal"
                )
              } else {
                valueBox(
                    value="",
                    subtitle="Coût €",
                    icon=icon("euro-sign"), 
                    color="teal"
                )
              }     
            })
        
        output$box_cout_carbone <- renderValueBox({
              validate(need(!is.null(va$isac_dat), ""))              
              if(nrow(va$isac_dat)>0){                
                if (all(c("isac_amont2",
                            "guenrouet",
                            "isac_fonctionnement_cumul_p1",
                            "isac_fonctionnement_cumul_p3") %in%
                        colnames(va$isac_dat))){
                  cout_carbone <- paste(sum(va$isac_dat$cout_carbone, na.rm=TRUE),"kg CO2")
                } else {
                  cout_carbone <- "Non calculé"
                }                
                valueBox(
                    value=h3(cout_carbone),
                    subtitle="Coût carbone des pompes",
                    icon=icon("earth-europe", class="fa-duotone"), 
                    color="red"
                )
              } else {
                valueBox(
                    value="",
                    subtitle="Coût carbone",
                    icon=icon("earth-europe", class="fa-duotone"), 
                    color="red"
                )
              }     
            })
        
        output$box_delta_h <- renderValueBox({
              validate(need(!is.null(va$isac_dat), ""))              
              if(nrow(va$isac_dat)>0){                
                if (all(c("isac_amont2",
                            "guenrouet") %in%
                        colnames(va$isac_dat))){
                  delta_h <- paste(round(mean(va$isac_dat$h, na.rm=TRUE),2),"m")
                } else {
                  delta_h <- "Non calculé"
                }                
                valueBox(
                    value=h3(delta_h, style='color:Olive;'),
                    subtitle="Delta H moyen au Thénot",
                    icon=icon("bridge-water"), # fas fa-dot-circle
                    color="orange"
                )
              } else {
                valueBox(
                    value="",
                    subtitle="Delta H moyen",
                    icon=icon("bridge-water"), # fas fa-dot-circle
                    color="orange"
                )
              }     
            })
        

        
        
      })
}

## To be copied in the UI
# mod_isac_ui("isac_1")

## To be copied in the server
# mod_isac_server("isac_1")
