
mod_progress_server <- function(id, rv) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## OUTPUTS #################################################################
    output$gr_progress_all <- renderPlot(gr_progress_all)
    
    ## OBSERVERS ###############################################################
    
    observeEvent(input$to_portal, {
      rv$to_portal <- input$to_portal
    })
    
    observeEvent(input$to_calc, {
      rv$to_calc <- input$to_calc
    })
    
    observeEvent(input$to_info, {
      rv$to_info <- input$to_info
    })
    
  }) ## END module server function
  
}