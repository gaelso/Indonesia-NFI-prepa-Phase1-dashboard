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

library(shiny)
library(bslib)
#library(crosstalk)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(treemap)
library(d3treeR)
library(forcats)
library(readxl)
library(shinyjs)
library(shiny.i18n)
library(shinyWidgets)
library(htmltools)
library(bsicons)
library(plotly)
library(readxl)

## Load data
source("R/data-AD.R", local = TRUE)
source("R/data-FRL.R", local = TRUE)

## Load Extra functions for leaflet setStyle() and setShapeStyle()
source("R/leaflet-setStyle-js.R", local = TRUE)
source("R/leaflet-setShapeStyle.R", local = TRUE)

## Initiate translation
i18n <- Translator$new(translation_csvs_path = 'translation')
i18n$set_translation_language('en')

## UI element Language selector
# language_selector <- shinyWidgets::radioGroupButtons(
#   inputId = "language",
#   label = NULL, 
#   choiceNames = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-tl"></i> TL'),
#   choiceValues = c("en", "te"),
#   selected = "en"
#   )

language_selector2 <- shinyWidgets::pickerInput(
  inputId = "language",
  label = NULL, 
  choices = c("en", "te"),
  choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-tl"></i> TL')),
  selected = "en",
  width = "auto",
  option = pickerOptions(style = "z-index:10000;")
)

## Source modules
source("R/mod_home_UI.R", local = TRUE)
source("R/mod_home_server.R", local = TRUE)
source("R/mod_portal_UI.R", local = TRUE)
source("R/mod_portal_server.R", local = TRUE)
source("R/mod_calc_UI.R", local = TRUE)
source("R/mod_calc_server.R", local = TRUE)
source("R/mod_about_UI.R", local = TRUE)



##
## UI ##########################################################################
##

ui <- tagList(
  
  ## Setup ---------------------------------------------------------------------
  shiny::withMathJax(),
  shinyjs::useShinyjs(),
  shinyWidgets::useSweetAlert(),
  shiny.i18n::usei18n(i18n),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  htmltools::htmlDependency(
    name = "flag-icons",
    version = "6.6.6",
    src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"), 
    stylesheet = "css/flag-icons.min.css"
  ),
  # tags$body(includeHTML("piwik-tracker.html")),
  # tags$head(includeHTML("piwik-tracker-draft-sync.html")),
  # tags$body(includeHTML("piwik-tracker-draft.html")),
  tags$head(includeHTML("ga-tracker-draft-head.html")),
  tags$body(includeHTML("ga-tracker-draft-body.html")),
  leafletjs,
  ## UI elements ---------------------------------------------------------------
  page_navbar(
    id = "navbar",
    ## ++ Styling ++++++
    title = div(HTML('<i class="fi fi-tl"></i>'), i18n$t("Timor Leste REDD+ Geoportal"), style = "display:inline;"),
    window_title = "TL REDD+ Geoportal",
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
      title = i18n$t("Home"), 
      value = "home", 
      icon = icon("campground"),
      mod_home_UI("tab_home") ## See R/mod_home_UI.R
      ),
    
    nav_panel(
      title = i18n$t("Portal"), 
      value = "portal", 
      icon  = icon("map"),
      mod_portal_UI("tab_portal") ## See R/mod_portal_UI.R
    ),
    
    nav_panel(
      title = i18n$t("Calculations"), 
      value = "calc", 
      icon = icon("chart-line"), 
      mod_calc_UI("tab_calc") ## See R/mod_calc_UI.R
    ),
    
    nav_panel(
      title = i18n$t("Info"), 
      value = "info", 
      icon = icon("lightbulb"), 
      mod_about_UI("tab_info") ## See R/mod_about_UI.R
    ),
    
    nav_spacer(),
    
    nav_item(language_selector2)
    
  ) |> ## End page_navbar
    tagAppendAttributes(.cssSelector = "nav", class = "navbar-expand-lg") 
) ## End tagList
  
# 
##
## Server ######################################################################
##

server <- function(input, output, session) {
  
  ## Reactives -----------------------------------------------------------------
  
  r_lang <- reactive({ input$language })
  
  rv <- reactiveValues(
    to_portal = NULL,
    to_calc   = NULL,
    to_about  = NULL
  )
  
  
  ## Server modules ------------------------------------------------------------
  mod_home_server("tab_home", r_lang = r_lang, rv = rv)
  mod_portal_server2("tab_portal", r_lang = r_lang)
  
  
  ## OBSERVERS -----------------------------------------------------------------
  
  observeEvent(input$language, {
    shiny.i18n::update_lang(language = input$language)
  })
  
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
