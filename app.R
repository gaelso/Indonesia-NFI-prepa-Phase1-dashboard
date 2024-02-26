#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##
## GLOBAL ######################################################################
##

# remotes::install_github("timelyportfolio/d3treeR")
# remotes::install_github('thomasp85/gganimate')

library(shiny)
library(bslib)
#library(crosstalk)
library(leaflet)
library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(treemap)
library(d3treeR)
library(gganimate)
library(forcats)
library(readxl)
library(shinyjs)
#library(shiny.i18n)
library(shinyWidgets)
library(htmltools)
library(bsicons)
library(plotly)
library(readxl)

## Load data
source("R/data-preparation.R", local = TRUE)
source("R/data-res-init.R", local = TRUE)

## Prepa tables
source("R/data-res-progress.R", local = TRUE)
## ERROR NEED TO BE FIXED IN PREPA SCRIPT ## 
## source("R/data-res-user.R", local = TRUE)
source("R/data-res-QAQC.R", local = TRUE)

## Load Extra functions for leaflet setStyle() and setShapeStyle()
# source("R/leaflet-setStyle-js.R", local = TRUE)
# source("R/leaflet-setShapeStyle.R", local = TRUE)

## Source modules
# source("R/mod_home_UI.R", local = TRUE)
# source("R/mod_home_server.R", local = TRUE)
# source("R/mod_portal_UI.R", local = TRUE)
# source("R/mod_portal_server.R", local = TRUE)
# source("R/mod_calc_UI.R", local = TRUE)
# source("R/mod_calc_server.R", local = TRUE)
# source("R/mod_about_UI.R", local = TRUE)



##
## UI ##########################################################################
##

ui <- tagList(
  
  ## Setup ---------------------------------------------------------------------
  shiny::withMathJax(),
  shinyjs::useShinyjs(),
  shinyWidgets::useSweetAlert(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  htmltools::htmlDependency(
    name = "flag-icons",
    version = "6.6.6",
    src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"), 
    stylesheet = "css/flag-icons.min.css"
  ),
  leafletjs,
  ## UI elements ---------------------------------------------------------------
  page_navbar(
    id = "navbar",
    ## ++ Styling ++++++
    title = "CEO progress dashboard",
    window_title = "CEO progress",
    theme = bs_theme(
      version = 5,
      bootswatch = "minty",
      base_font = font_google("Open Sans", wght = c(400, 700)),
      code_font = font_google("Fira Code"),
      heading_font = font_google("Quicksand", wght = 700)
    ),
    fillable = "portal",
    bg = "#f8f9fa",
    
    ## ++ Panels +++++
    nav_panel(
      title = "Overview", 
      value = "overview", 
      icon = icon("earth-asia"),
      mod_overview_UI("tab_ov") ## See R/mod_overview_UI.R
      ),
    
    nav_panel(
      title = "Progress",
      value = "progress",
      icon  = icon("chart-line"),
      mod_progress_UI("tab_prog") ## See R/mod_progress_UI.R
    ),

    # nav_panel(
    #   title = i18n$t("Calculations"), 
    #   value = "calc", 
    #   icon = icon("chart-line"), 
    #   mod_calc_UI("tab_calc") ## See R/mod_calc_UI.R
    # ),
    # 
    # nav_panel(
    #   title = i18n$t("Info"), 
    #   value = "info", 
    #   icon = icon("lightbulb"), 
    #   mod_about_UI("tab_info") ## See R/mod_about_UI.R
    # ),
    
    
  ) |> ## End page_navbar
    tagAppendAttributes(.cssSelector = "nav", class = "navbar-expand-lg") 
) ## End tagList
  


##
## Server ######################################################################
##

server <- function(input, output, session) {
  
  ## Reactives -----------------------------------------------------------------
  
  rv <- reactiveValues(
    to_portal = NULL,
    to_calc   = NULL,
    to_about  = NULL
  )
  
  
  ## Server modules ------------------------------------------------------------
  mod_overview_server("tab_ov", rv = rv)
  mod_progress_server("tab_prog", rv = rv)
  
  ## OBSERVERS -----------------------------------------------------------------
  
  observeEvent(rv$to_portal, {
    updateNavlistPanel(inputId = "navbar", session = session, selected = "portal")
  })
  
  observeEvent(rv$to_calc, {
    updateNavlistPanel(inputId = "navbar", session = session, selected = "calc")
  })
  
} ## End server


##
## Run the application #########################################################
##

shinyApp(ui = ui, server = server)
