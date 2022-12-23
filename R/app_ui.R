#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd 
app_ui <- function(request) {
  tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # Your application UI logic
      navbarPage(
          theme = bslib::bs_theme(version = 5, primary = "#00218f", success = "#33b5ff", 
              info = "#00C9C4", warning = "#ffb428", base_font = "Segoe UI Symbol", 
              heading_font = "Georgia", font_scale = NULL, `enable-gradients` = TRUE, 
              bootswatch = "cerulean"),
          title="SIVA",
# pour les icones j'ai tout essayé ? pas d'icone si icon in navbar
#              div(id="img-id",
#                  htmltools::img(src=system.file("app/www/favicon.png", package = "SIVAshiny"),
#                      #alt="logo-SIVA",                    
#                      style = "position: fixed;
#                          left: 10px;
#                          top: 20px;"),
#                 "SIVA"
#              ),
          
          mod_vilaine_aval_ui("mod_vilaine_aval_1"),
          #mod_barrage_ui("mod_barrage_1"),
          # partie barrage : pas dans un module ! --------------
          tabPanel(title="Barrage",
              icon=icon("poll"),
              sidebarLayout(
                  sidebarPanel(width = 2, 
                      dateInput("bar_datedebut",
                          label = "date de début :",
                          value = Sys.Date()-7,
                          weekstart = 1, # lundi
                          language = "fr",
                          format = "dd/mm/yyyy"),
                      dateInput("bar_datefin", 
                          label = "date de fin :", 
                          value =Sys.Date(),
                          weekstart = 1, # lundi
                          language = "fr",
                          format = "dd/mm/yyyy"),
                      shinyWidgets::actionBttn(
                          inputId = "bar_bttn",
                          label = "OK",
                          style = "fill", 
                          color = "primary"					
                      )
                  ),
                  mainPanel(
                      shinydashboard::tabBox(                          
                          id= "ttabs", 
                          width = 10, 
                          tabPanel(title = "Graph jour",
                              value=2,                               
                              h3("Volume journaliers au barrage"),
                              shinycssloaders::withSpinner(plotly::plotlyOutput("bar_plotly_volume_jour")),
                              h3("Débit des vannes et volets"),
                              shinycssloaders::withSpinner(plotly::plotlyOutput("bar_plotly_journalier_vanne_volet"))
                          ),
                          tabPanel(title ="Graph 10 min",
                              value=1, 
                              p("Vous pouvez sélectionner une gramme de niveaux"),
                              h3("Graphiques de niveaux"),
                              shinycssloaders::withSpinner(plotly::plotlyOutput("bar_plotly_niveau_10")),
                              h3("Graphiques de debit"),
                              shinycssloaders::withSpinner(plotly::plotlyOutput("bar_plotly_debit_10"))
                          ),
                          
                          
                          tabPanel(title = "Tableaux", 
                              value=3,      
                              h2("Débits barrage"),
                              shinycssloaders::withSpinner(DT::DTOutput("bar_Qj"))
                          ),
                          tabPanel(title = "Signification des variables", 
                              value=4,      
                              includeMarkdown(app_sys("app/www/variables.md"))
                          )
                      
                      )
                  )  
              )
          ),
          tabPanel("Isac (x)",
          
          ),
          tabPanel("Trevelo (x)",
          
          ),
          tabPanel("Passe à poissons (x)",
          
          ),
          navbarMenu("A propos",
              tabPanel("Table",
                  DT::dataTableOutput("table")
              ),
              tabPanel("About",
                  fluidRow(
                      column(6,
                          # changer au deployement
                          includeMarkdown(app_sys("app/www/about.md"))
                      ),
                      column(3,
                          img(class="img-polaroid",
                              src=paste0("http://upload.wikimedia.org/",
                                  "wikipedia/commons/9/92/",
                                  "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                          tags$small(
                              "Source: Barrage d'Arzal ",
                              "Automobile Club's July 10, 2005 show at the ",
                              "Endicott Estate in Dedham, MA by ",
                              a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                  "User:Sfoskett")
                          )
                      )
                  )
              )
          )
      )
  )
  
  
  
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
      "www",
      app_sys("app/www")
  )
  
  tags$head(
      favicon(),
      bundle_resources(
          path = app_sys("app/www"),
          app_title = "SIVAshiny"
      )
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert()
  )
}
