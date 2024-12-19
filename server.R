function(input, output, session) {
  
  studies <- reactiveVal({
    readRDS('./data/studies.rds')
  })
  
  
  output$studies_tbl <- renderDataTable({
    
    req(studies())

    datatable(select(studies(), org, title, phase),
               selection = 'none',
              options = list(pageLength = 20),
              colnames=c("Organization", "Title", "Phase"), 
              rownames= FALSE)
  })


  output$mapa <- renderLeaflet({
    
    studies_initial <- readRDS('./data/studies.rds')
    
    leaflet(studies_initial) %>%
      setView(lng = 0, lat = 0, zoom = 1.5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(label = ~title, color="blue") 
  })
  
  observeEvent(input$api_request, {
    
    print(input$mapa_bounds)
    new_data <- get_studies(input$mapa_bounds, input$phase, input$condition)
    
    studies(new_data)
  })
  
  observe({
    
    print(studies())

    if(nrow(studies())>0) {
      leafletProxy("mapa", session, data=studies()) %>%
        clearMarkers() %>%
        addCircleMarkers(
          label = ~title, #lapply(lbl, htmltools::HTML),
          color="blue")
    } else {
      leafletProxy("mapa", session) %>%
        clearMarkers()
    }

  })
}
