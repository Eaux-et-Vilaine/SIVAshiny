library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(SIVA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rAmCharts)
library(plotly)
library(dplyr)
library(stringr)
library(summaryBox) # remotes::install_github("deepanshu88/summaryBox")
library(pool)
#library(crosstalk)
#source("R/app_config.R")
pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = get_golem_config("dbnamemysql."),
  host = get_golem_config("hostmysql."),
  username = get_golem_config("umysql."),
  password = get_golem_config("pwdmysql."),
  port=get_golem_config("portmysql.")
)
onStop(function() {
      pool::poolClose(pool)
    })

bleu_EV <- "#00218f"
turquoise_EV <- "#00C9C4"
orange_EV <- "#ff7557"
jaune_EV <- "#ffb428"
bleu_EVf <- "#001350"
jaune_EVf <- "#AD7000"
orange_EVf <- "#b2513c"
bleu_clair_EV <- "#33b5ff"
turquoise_EVf <- "#007873"