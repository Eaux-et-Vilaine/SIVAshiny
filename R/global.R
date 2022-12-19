library(shiny)
library(shinipsum)
library(DT)
library(shinythemes)
library(SIVA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rAmCharts)
library(plotly)
library(dplyr)
library(stringr)
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


