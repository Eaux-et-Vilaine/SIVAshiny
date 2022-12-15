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
    #fluidPage(
    #  shinythemes::themeSelector(),
    # tags$head(
    #   tags$link(rel = "shortcut icon", href = app_sys("favicon.ico")),
    #   #-- biblio js ----
    #   tags$link(rel="stylesheet", type = "text/css",
    #             href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
    #   tags$link(rel="stylesheet", type = "text/css",
    #             href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
    # ),
    ##-- Logo ----
    # list(tags$head(HTML('<link rel="icon", href="favicon.png",
    #                     type="image/png" />'))),
    # div(style="padding: 1px 0px; width: '100%'",
    #     titlePanel(
    #       title="", windowTitle = "SIVA"
    #     )
    # ),
    ##-- Favicon ----
    # Your application UI logic
    navbarPage(
      theme = bslib::bs_theme(version = 5, primary = "#00218f", success = "#33b5ff", 
                              info = "#00C9C4", warning = "#ffb428", base_font = "Segoe UI Symbol", 
                              heading_font = "Georgia", font_scale = NULL, `enable-gradients` = TRUE, 
                              bootswatch = "cerulean"),
      title=
        div(id="img-id",
            img(src=system.file("app/www/favicon.png", package = "SIVAshiny"),
                height =  "100px"),
            # style = "position: fixed;
            #       left: 10px;
            #       top: 5px;",
            "SIVA"
        ),
        # span(
        #   img(src=app_sys("favicon.png"),              
        #       height =  "200px"),
        #   "SIVA"
        # ),
      # id = "navbar",
      # windowTitle = "window main navbar",
      # ---- modules are tabpanels
      
      mod_vilaine_aval_ui("mod_vilaine_aval_1"),
      mod_barrage_ui("mod_barrage_1"),
      tabPanel("Isac",
               
      ),
      tabPanel("Trevelo",
               
      ),
      tabPanel("Passe à poissons",
               
      ),
      tabPanel("Autres astreintes",
               
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
                                     "Source: Photographed at the Bay State Antique ",
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
