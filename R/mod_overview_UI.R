
mod_overview_UI <- function(id){
  
  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  ##
  ## UI Elements ###############################################################
  ##
  
  ## Title ---------------------------------------------------------------------
  ov_title <- card(
    h5(
      "Remote sensing data interpretation workshop for Phase 1 of the", 
      br(), 
      "improved Indonesia National Forest Inventory design", 
      style = "text-align:center;"
    )
  )
  
  
  ## Intro ---------------------------------------------------------------------
  ov_intro <- card(
    p(
    strong("23-27 October 2023"), br(),
    strong("Bali, Indonesia")
    ),
    img(
      src = "img-WS-Bali.png",
      style = "width:100%; max-width:300px; margin-left:auto; margin-right:auto; border-radius:0.4rem"
    )
  )
  
  ## Highlights ----------------------------------------------------------------
  ov_highlights <-  card(
    layout_column_wrap(
          width = "240px",
          #fixed_width = TRUE,
          #height = "240px",
          fill = FALSE,
          style = "margin-top: auto; margin-bottom: auto;",
          value_box(
            title ="Plots to analyze",
            value = "9,894",
            showcase = bsicons::bs_icon("pin-map", size = NULL),
            theme_color = "danger"
          ),
          value_box(
            title = "Plot analyzed twice for QAQC",
            value = "1,010",
            showcase = bsicons::bs_icon("pin-map-fill", size = NULL),
            theme_color = "secondary"
          )
        ),
        layout_column_wrap(
          width = "240px",
          #fixed_width = TRUE,
          #height = "240px",
          fill = FALSE,
          style = "margin-top: auto; margin-bottom: auto;",
          value_box(
            title = "Participants",
            value = "45",
            showcase = bsicons::bs_icon("people", size = NULL),
            theme_color = "primary"
          ),
          value_box(
            title = "collection days",
            value = "4",
            showcase = bsicons::bs_icon("calendar-week", size = NULL),
            theme_color = "info",
            div("8 half-day working sessions", style = "font-size:small;")
          )
        )
  )
  
  # ov_highlights <-  card(
  #   layout_column_wrap(
  #     width = "280px",
  #     #fixed_width = TRUE,
  #     #height = "240px", 
  #     fill = FALSE,
  #     style = "margin-top: auto; margin-bottom: auto;",
  #     value_box(
  #       title ="FREL (tCO2/y)",
  #       value = "412,532",
  #       showcase = bsicons::bs_icon("arrow-up", size = NULL),
  #       theme_color = "danger"
  #     ),
  #     value_box(
  #       title = "FRL (tCO2/y)",
  #       value = "-335,162",
  #       showcase = bsicons::bs_icon("arrow-down-up", size = NULL),
  #       theme_color = "info"
  #     )
  #   ),
  #   layout_column_wrap(
  #     width = "280px",
  #     #fixed_width = TRUE,
  #     #height = "240px", 
  #     fill = FALSE,
  #     style = "margin-top: auto; margin-bottom: auto;",
  #     value_box(
  #       title = "Deforestation (ha/y)",
  #       value = "711",
  #       showcase = vb_df,
  #       full_screen = TRUE,
  #       theme_color = "secondary"
  #     ),
  #     value_box(
  #       title = "Afforestation (ha/y)",
  #       value = "9,311",
  #       showcase = vb_af,
  #       full_screen = TRUE,
  #       theme_color = "primary"
  #     )
  #   ),
  #   div(
  #     em("Average Forest area change and Forest Reference (Emission) Levels 2017-2021."),
  #     style = "text-align: center; font-size: small;"
  #   )
  # )
  
  ## Spatial -------------------------------------------------------------------
  # ov_spatial <- card(
  #   div(
  #     id = ns("spatial_en"),
  #     h5("Spatial data"),
  #     p("The REDD+ Geoportal displays spatial information on land use and land use change during the reference period 2017-2021."),
  #     p("It includes:"),
  #     tags$ul(
  #       tags$li("Base layers to switch between canvas, high resolution images and OpenStreetMap."),
  #       tags$li("The hexagonal sampling grid for land use and land use change visual interpretation, including additional information on the hexagons were change was detected."),
  #       tags$li("Visual interpretation results: annual land use and REDD+ activities.")
  #     )
  #   ),
  #   actionButton(
  #     inputId = ns("to_portal"), 
  #     label = "go to Portal", 
  #     class = "btn-primary",
  #     style = "width: 128px; margin: auto"
  #     )
  # )
  # 
  # ## Calculations --------------------------------------------------------------
  # ov_calc <- card(
  #   div(
  #     id = ns("calc_en"),
  #     h5("Calculations"),
  #     p("The spatial data is converted to annual land use change matrices (Activity Data) and completed by carbon stock changes associated with each category of land use change (Emission Factors)."),
  #     # p("When sampling points (both activity data and forest inventory plots) values are aggregated, the sampling uncertainty is added."),
  #     p("The matrices are then aggregated into annual greenhouse gas emissions and removals from the forestry sector."),
  #     p("To see the matrices and carbon accounting results, go to:")
  #   ),
  #   actionButton(
  #     inputId = ns("to_calc"), 
  #     label = i18n$t("go to Calculations"), 
  #     class = "btn-primary",
  #     style = "width: 128px; margin: auto"
  #     )
  # )
  # 
  # ## About ---------------------------------------------------------------------
  # ov_info <- card(
  #   div(
  #     id = ns("info_en"),
  #     h5("More information"),
  #     p("This portal was developed to improve transparency on greenhouse gas emissions and removals of Timor Leste's forestry sector."),
  #     p("It also includes information on the context around this portal, what is REDD+, some of the technical terms associated, as well as the methods for data collection and analysis."),
  #     p("Find out more in the Info tab.")
  #   ),
  #   actionButton(
  #     inputId = ns("to_info"), 
  #     label = i18n$t("More information"), 
  #     class = "btn-primary",
  #     style = "width: 128px; margin: auto"
  #   )
  # )
  
  ov_about <- card(
    div(
      "
      This workshop's objective is to analyze around 12,000 hexagonal plots using 
      remote sensing products such as Landsat, Sentinel or NICFI planet imagery, as 
      well as GEDI data.
      ",
      br(),
      "
      Users assess the land use in 2023, changes between 2017 and 2023 as well as 
      plot accessibility. This information will be used to select field locations 
      for the Phase 2 of the improved NFI.
      "
      
    ),
    img(
      src = "group_photo.jpeg",
      style = "width:100%; max-width:1200px; margin-left:auto; margin-right:auto; border-radius:0.4rem"
    )
  )
  
  ##
  ## UI organization ###########################################################
  ##
  
  # UI elements wrapped in a tagList() function
  tagList(
    
    ov_title,
    
    layout_column_wrap(
      width = "300px",
      style = css(grid_template_columns = "1fr 2fr"),
      ov_intro,
      ov_highlights
    ),
    
    br(),
    
    ov_about,

    # layout_column_wrap(
    #   width = "200px",
    #   ov_spatial,
    #   ov_calc,
    #   ov_info
    # ),
    
    br()
    
  )
  
} ## END module UI function
