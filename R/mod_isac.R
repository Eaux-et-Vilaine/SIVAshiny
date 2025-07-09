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
                      "Niveau Vilaine au Pont de Cran"="pontdecran",
                      "Niveau Isac à Guenrouet"="guenrouet",
                      "Niveau Vilaine écluse Redon"= "redon_ecluse",
                      "Niveau Vilaine Arzal" = "vilaine_barrage",
                      "Niveau Marais Isac" = "isac_niveau_marais",
                      "Niveau Vilaine aval Prévert (1)" = "isac_aval1",
                      "Niveau Isac amont Prévert (1)" = "isac_amont1",
                      "Niveau Vilaine aval Prévert (2)" = "isac_aval2",
                      "Niveau Isac amont Prévert (2)" = "isac_amont2",
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
                      "isac_aval1",
                      "isac_amont2",
                      "isac_fonctionnement_cumul_p1",
                      "isac_fonctionnement_cumul_p3",
                      "isac_position_vanne_1",
                      "isac_position_vanne_2",
                      "pluvio_barrage_10min",
                      "pluvio_isac_10min"
                  )
              ),
              
              h5("Débits et analyse environnementale :"),
              uiOutput(ns("isa_output_text")),
              uiOutput(ns("isa_output_radio_amont")),
              uiOutput(ns("isa_output_radio_aval")),
              uiOutput(ns("isa_output_radio_marais")),
              shinyWidgets::actionBttn(
                  inputId = ns("bttn_isa"),
                  label = "OK",
                  style = "fill", 
                  color = "primary"					
              )
          
          
          ), #end sidebarpanel
          mainPanel(width = 9,    
              shinydashboard::tabBox(                          
                  id= "ttabs_isa", 
                  width = 10, 
                  tabPanel(title = "Graphiques",
                      value=2, 
                      h3("Niveau"),
                      fluidRow(
                          valueBoxOutput(ns("box_pluvio"), width=4),
                          valueBoxOutput(ns("box_delta_h"), width=4)
                      
                      ),              
                      shinycssloaders::withSpinner(
                          rAmCharts::amChartsOutput(
                              outputId = ns("isa_rmarchart_niveau")
                          )              
                      ),
                      h3("Pompes"),
                      fluidRow(
                          valueBoxOutput(ns("box_cout"), width=3),
                          valueBoxOutput(ns("box_cout_carbone"), width=3),
                          valueBoxOutput(ns("box_debit_pompes"), width=3),
                          valueBoxOutput(ns("box_volume_pompes"), width=3)
                      
                      ),  
                      h3("Vannage du Prévert (Théhillac)"),              
                      shinycssloaders::withSpinner(                    
                          plotly::plotlyOutput(
                              outputId = ns("isa_fonct_thenot"))
                      ),
                      h3("Indicateurs"),              
                      valueBoxOutput(ns("box_continuite"), width=4),                        
                      shinycssloaders::withSpinner(                    
                          plotOutput(
                              outputId = ns("isa_continuite"),
                              height=100,
                              width="90%"
                          )
                      ),
                      valueBoxOutput(ns("box_ornitho"), width=4),
                      shinycssloaders::withSpinner(                    
                          plotOutput(
                              outputId = ns("isa_avifaune"),
                              height=100,
                              width="90%")),
                      valueBoxOutput(ns("box_brochet"), width=4),
                      shinycssloaders::withSpinner(                    
                          plotOutput(
                              outputId = ns("isa_brochet"),
                              height=100,
                              width="90%")
                      )
                  ), # end tab panel
                  tabPanel(title = "Tableaux",
                      value=3,
                      h2("Niveaux, débits et indicateurs"),
                      shinycssloaders::withSpinner(DT::DTOutput(ns("isa_table")))
                  ),
                  tabPanel(title = "Informations",
                      value=3,
                      includeMarkdown(app_sys("app/www/isac.md"))
                  )
              )#end tabBox
          )# end main panel
      )) 
}

#' isac Server Functions
#'
#' @noRd 
mod_isac_server <- function(id){
  moduleServer( id, function(input, output, session){
        ns <- session$ns
        va <- reactiveValues(isac_dat = NULL, isac_dat1 = NULL)
        
        get_tags <- function(){          
          codes <- input$isa_choix
          if (!all(codes%in%SIVA::isac$code)) stop(
                sprintf("Erreur interne, code %s pas dans la liste des codes pour l'Isac", 
                    codes[!codes%in%SIVA::isac$code]))
          tags <- SIVA::isac[SIVA::isac$code %in% codes,"tag"] %>% pull()
          return(tags)          
        }
        
        
        output$isa_output_radio_amont <- renderUI({
              
              codes <- input$isa_choix               
              isac_niveaux_amont <- isac[c(12,10,17,2),]
              isac_niveaux_amont <- isac_niveaux_amont[isac_niveaux_amont$code %in% codes,]              
              radioButtons(inputId=ns("isa_radio_debit_amont"),
                  label = "Niveau amont:",
                  selected = isac_niveaux_amont$code[1],
                  choiceValues = isac_niveaux_amont$code, 
                  choiceNames = isac_niveaux_amont$libelle)
            })
        output$isa_output_radio_aval <- renderUI({
              codes <- input$isa_choix            
              isac_niveaux_aval <- isac[c(9,1,11,3,4),]
              isac_niveaux_aval <- isac_niveaux_aval[isac_niveaux_aval$code %in% codes,]              
              radioButtons(inputId = ns("isa_radio_debit_aval"),
                  label = "Niveau aval:",
                  selected = isac_niveaux_aval$code[1],
                  choiceValues = isac_niveaux_aval$code, 
                  choiceNames = isac_niveaux_aval$libelle)           
            })
        output$isa_output_radio_marais <- renderUI({
              codes <- input$isa_choix            
              isac_niveaux_marais <- isac[c(17,2,12,10),]
              isac_niveaux_marais <- isac_niveaux_marais[isac_niveaux_marais$code %in% codes,]              
              radioButtons(inputId = ns("isa_radio_marais"),
                  label = "Niveau marais:",
                  selected = isac_niveaux_marais$code[1],
                  choiceValues = isac_niveaux_marais$code, 
                  choiceNames = isac_niveaux_marais$libelle)           
            })
        
        
        
        observeEvent(input$bttn_isa,{              
              
              validate(need(exists("pool"), "Il faut une connexion vers la base"))
              validate(need(input$isa_datefin>input$isa_datedebut, "la date de fin doit être supérieure à la date de début"))
              
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
#    calcul des débits quand le choix des radiobutton pour les niveaux amont et aval est effectué
        
        observe({
              validate(need(!is.null(va$isac_dat), "")) 
              # ne relancer qu'après avoir cliqué sur le bouton.
              validate(need(input$isa_radio_debit_amont %in%colnames(va$isac_dat), "appuyer sur le bouton"))
              validate(need(input$isa_radio_debit_aval %in%colnames(va$isac_dat),"appuyer sur le bouton"))
              isac_dat <- va$isac_dat
              hamont = va$isac_dat[,input$isa_radio_debit_amont]
              haval = va$isac_dat[,input$isa_radio_debit_aval]
              hvanne1 = va$isac_dat$isac_position_vanne_1
              hvanne2 = va$isac_dat$isac_position_vanne_2
              debit <-  debit_vannes_isac(
                  hamont = hamont,
                  haval = haval,
                  hvanne1 = hvanne1,
                  hvanne2 = hvanne2
              )
              isac_dat1 <- cbind(isac_dat, debit)
              va$isac_dat1 <- isac_dat1
            }, label= "calcul_debit_isac")
        
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
                    value=p(HTML(string_pluvio), style = 'font-size: 80%;'),
                    subtitle=h5("Pluviométrie",style = 'color:GhostWhite'),
                    icon=icon("cloud-rain"),
                    color="purple")
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
                  Q_pompes <- paste(round(mean(va$isac_dat$Q_pompes, na.rm=TRUE),2),"m3/s")
                } else {
                  Q_pompes <- "Non calculé"
                }                
                valueBox(
                    value=p(Q_pompes, style = 'color:LemonChiffon;font-size: 90%;'), 
                    subtitle=h5("Débit pompes", style='color:LightCyan'),
                    icon=icon("arrow-up-from-water-pump"), 
                    color="navy"
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
                  V_pompes <- paste(round(sum(va$isac_dat$Q_pompes*600, na.rm=TRUE)),"m3")
                } else {
                  V_pompes <- "Non calculé"
                }                
                valueBox(
                    value=p(V_pompes, style = 'color:Navy;font-size: 100%;'), 
                    subtitle=h5("Volume pompes",style='color:MidnightBlue;'),
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
                  cout <- paste(round(sum(va$isac_dat$cout_euros, na.rm=TRUE)),"€")
                } else {
                  cout <- "Non calculé"
                }                
                valueBox(
                    value=h3(cout, style = 'color:DarkGreen;'),
                    subtitle=h5("Coût €",style='color:GhostWhite;'),
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
                  cout_carbone <- sum(va$isac_dat$cout_carbone, na.rm=TRUE)
                  cout_carbone_txt <- paste(round(cout_carbone),"kg CO2")
                  color <- "black"
                  if (cout_carbone == 0) color <- "lime"
                  if (cout_carbone > 0) color <- "orange"
                  
                } else {
                  cout_carbone <- "Non calculé"
                  color <- "black"
                }                
                valueBox(
                    value=h3(cout_carbone_txt),
                    subtitle=h5("Coût carbone",style='color:MidnightBlue;'),
                    icon=icon("earth-europe"), 
                    color=color
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
                    value=p(delta_h, style='color:Olive;font-size: 100%;'),
                    subtitle=h5("Delta H moyen",style='color:GhostWhite;'),
                    icon=icon("bridge-water"), # fas fa-dot-circle
                    color="teal"
                )
              } 
            })
        
        
        output$box_continuite <- renderValueBox({ 
              validate(need(!is.null(va$isac_dat1), ""))              
              if(nrow(va$isac_dat1)>0){   
                
                isac_dat1 <- va$isac_dat1
                transp <- isac_dat1 %>% group_by(mig) %>%
                    summarize(N=n()) %>% 
                    ungroup() %>%
                    mutate(perc=N/sum(N))
                if (transp$perc[transp$mig=="1-transparente"]>0.5) color <- "green"
                if (transp$perc[transp$mig=="1-transparente"]<=0.5 & 
                    transp$perc[transp$mig=="1-transparente"]>0.2) color <- "orange"
                if (transp$perc[transp$mig=="1-transparente"]<=0.2 ) color <- "red"
                
                transptxt <- sprintf("transparent %s %% <br/> difficile %s %%", round(transp$perc[transp$mig=="1-transparente"]*100), 
                    round(transp$perc[transp$mig=="2-difficile"]*100)) 
                valueBox(
                    value = p(HTML(transptxt), style = 'font-size: 80%;'),
                    subtitle = h5("Continuité piscicole",style='color:GhostWhite;'),
                    icon = icon("fish"), 
                    color = color)
              }
            })
        
        output$box_ornitho <- renderValueBox({                        
              validate(need(!is.null(va$isac_dat1), ""))  
              isac_dat1 <- va$isac_dat1
              isac_dat1$avifaune <- indicateur_avifaune_isac(isac_dat1,  niveau_marais=input$isa_radio_marais)
              avi <- isac_dat1 %>% group_by(avifaune) %>%
                  summarize(N=n()) %>% 
                  ungroup() %>%
                  mutate(perc=N/sum(N))
              avi_sel <- avi$avifaune[which.max(avi$perc)]
              avi_value <- 100 * avi$perc[which.max(avi$perc)]
              avi_txt <- sprintf("%s (%s %%)", avi_sel, round(avi_value)) 
              color <- switch(as.character(avi_sel), 
                  "0-bon"="blue",
                  "1-moyen"="orange",
                  "2-mauvais"="red",
                  "3-hors periode"="teal",
                  "4-inconnu"="black")
              valueBox(
                  value = p(avi_txt, style = 'color:Navy;font-size: 80%;'),
                  subtitle = h5("Accueil avifaune",style='color:GhostWhite;'),
                  icon=icon("kiwi-bird"), 
                  color=color)
              
            })
        
        output$box_brochet <- renderValueBox({                        
              validate(need(!is.null(va$isac_dat1), ""))              
              if(nrow(va$isac_dat1)>0){ 
                
                
                isac_dat1 <- va$isac_dat1
                isac_dat1$brochet <- indicateur_brochet_isac(isac_dat1,                     
                    niveau_marais=input$isa_radio_marais)
                bro <- isac_dat1 %>% group_by(brochet) %>%
                    summarize(N=n()) %>% 
                    ungroup() %>%
                    mutate(perc=N/sum(N))
                bro_sel <- bro$brochet[which.max(bro$perc)]
                bro_value <- 100 * bro$perc[which.max(bro$perc)]
                bro_txt <- sprintf("%s (%s %%)", bro_sel, round(bro_value)) 
                color <- switch(as.character(bro_sel), 
                    "0-repro-bon"="blue",
                    "1-repro-moyen"="orange",
                    "2-repro-mauvais"="red",
                    "3-emergence-bon"="lime",
                    "4-emergence-moyen"="yellow",
                    "5-emergence-mauvais"="maroon",
                    "6-inconnu/hors periode"="black")
                
                valueBox(
                    value=h4(bro_txt, style = 'color:DarkGreen;'),
                    subtitle=h5("Reproduction brochet",style='color:White;'),
                    icon=icon("fish"), 
                    color=color)
              }
            })
        
        # Niveaux ------------------------------------------------------------- 
        
        
        output$isa_rmarchart_niveau <- rAmCharts::renderAmCharts({
              validate(need(!is.null(va$isac_dat), "cliquez sur le bouton OK pour charger des valeurs"))
              if(nrow(va$isac_dat)>0){
                which_niveau <- grep("Niveau", attributes(va$isac_dat)$libelle)+1
                isac_dat_niveau <- va$isac_dat %>%  
                    select("horodate",all_of(which_niveau))                
                
                rAmCharts::amTimeSeries(
                        isac_dat_niveau, 
                        'horodate',
                        colnames(isac_dat_niveau)[2:ncol(isac_dat_niveau)],
                        bullet = "round",
                        color =   randomcoloR::distinctColorPalette(ncol(isac_dat_niveau)-1),
                        #backgroundColor = "#40555E",
                        #backgroundAlpha = 0.4,
                        bulletSize =  6,
                        aggregation = "Average",
                        #fillAlphas = 0.2,
                        groupToPeriods = c('10mm', '30mm', 'hh', 'DD', 'MAX'),
                        #  c('hh', 'DD', '10DD','MM','MAX'),
                        linewidth = 0.4,
                        legend = TRUE           
                    ) %>%
                    rAmCharts::setExport(enabled = TRUE)
              }
            })
        
        ## Debits ------------------------------------------------------------- 
        
        
        output$isa_fonct_thenot <- plotly::renderPlotly({
              validate(need(!is.null(va$isac_dat1), "cliquez sur le bouton OK pour charger des valeurs"))
              if(nrow(va$isac_dat1)>0){
                isac_dat1 <- va$isac_dat1
                
                plotly::ggplotly(
                    ggplot(isac_dat1) + 
                        geom_line(aes(x=horodate, y= Q, colour="Q-vannes")) +
                        geom_line(aes(x=horodate, y = Q_pompes, colour="Q-pompes")) +
                        scale_colour_manual("Débit", values=c("Q-vannes"=bleu_EV,"Q-pompes"=orange_EV)) +
                        theme_bw()
                )
              }
            })
        
        
        output$isa_continuite <- renderPlot({
              validate(need(!is.null(va$isac_dat1), "cliquez sur le bouton OK pour charger des valeurs"))
              if(nrow(va$isac_dat1)>0){               
                isac_dat1 <- va$isac_dat1
                ggplot(isac_dat1) +
                    geom_rect(
                        aes(
                            xmin = horodate,
                            xmax = horodate + as.difftime(10, units = "mins"),
                            ymin = -30,
                            ymax = -35,
                            fill = mig
                        )) +
                    scale_fill_manual("Transparence migratoire", values = c("1-transparente"="blue",
                            "2-difficile"="orange",
                            "3-bloquee"="red",
                            "4-inf5cm"="purple"))+
                    theme_minimal() +
                    
                    theme(legend.position="bottom",
#                        legend.title = element_text(size=12),
#                        legend.text = element_text(size=12),
#                        legend.key.height =unit(1, 'cm'),
#                        legend.key.width =unit(1, 'cm'),
                        legend.direction = "horizontal",
                        axis.line.x = element_blank(), axis.line.y = element_blank(), 
                        axis.text.x = element_blank(), axis.text.y = element_blank(), 
                        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                        axis.title.x = element_blank(), axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
                
              }
            })
        
        # TODO faut il une sonde différente ?        
        output$isa_avifaune <- renderPlot({
              validate(need(!is.null(va$isac_dat1), "cliquez sur le bouton OK pour charger des valeurs"))
              if(nrow(va$isac_dat1)>0){
                isac_dat1 <- va$isac_dat1   
                isac_dat1$avifaune <- indicateur_avifaune_isac(isac_dat1,  niveau_marais=input$isa_radio_marais)
                ggplot(isac_dat1) +
                    geom_rect(
                        aes(
                            xmin = horodate,
                            xmax = horodate + as.difftime(10, units = "mins"),
                            ymin = -40,
                            ymax = -45,
                            fill = avifaune
                        )) +
                    scale_fill_manual("Accueil avifaune", values = c("0-bon"="blue",
                            "1-moyen"="orange",
                            "2-mauvais"="red",
                            "3-hors periode"="white",
                            "4-inconnu"="black"))+
                    theme_minimal() +
                    theme(axis.line.x = element_blank(), axis.line.y = element_blank(), 
                        axis.text.x = element_blank(), axis.text.y = element_blank(), 
                        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                        axis.title.x = element_blank(), axis.title.y = element_blank())+
                    theme(legend.position="bottom",                        
                        legend.direction = "horizontal",
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
              }
            })
        output$isa_brochet <- renderPlot({
              validate(need(!is.null(va$isac_dat1), "cliquez sur le bouton OK pour charger des valeurs"))
              if(nrow(va$isac_dat1)>0){
                isac_dat1 <- va$isac_dat1
                isac_dat1$brochet <- indicateur_brochet_isac(isac_dat1,  niveau_marais=input$isa_radio_marais)
                ggplot(isac_dat1) +
                    geom_rect(
                        aes(
                            xmin = horodate,
                            xmax = horodate + as.difftime(10, units = "mins"),
                            ymin = -50,
                            ymax = -55,
                            fill = brochet
                        )) +
                    scale_fill_manual("Repro brochet", values = c("0-repro-bon"="blue",
                            "1-repro-moyen"="orange",
                            "2-repro-mauvais"="red",
                            "3-emergence-bon"="cyan",
                            "4-emergence-moyen"="gold",
                            "5-emergence-mauvais"="firebrick",
                            "6-inconnu/hors periode"="black")) +
                    
                    
                    theme_minimal() +
                    theme(axis.line.x = element_blank(), axis.line.y = element_blank(), 
                        axis.text.x = element_blank(), axis.text.y = element_blank(), 
                        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                        axis.title.x = element_blank(), axis.title.y = element_blank())+
                    theme(legend.position="bottom",                               
                        legend.direction = "horizontal",
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
              }
            })
        
      })
}

## To be copied in the UI
# mod_isac_ui("isac_1")

## To be copied in the server
# mod_isac_server("isac_1")
