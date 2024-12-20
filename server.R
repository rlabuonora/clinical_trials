function(input, output, session) {
  
  studies <- reactiveVal({
    readRDS('./data/studies.rds')
  })
  
  
  output$studies_tbl <- DT::renderDT({

    req(studies())
    
    studies_df <- studies()
   

    datatable(dplyr::select(studies_df, org, title, phase, status),
               selection = 'none',
              options = list(pageLength = 20),
              colnames=c("Organization", "Title", "Phase", "Status"),
              rownames= FALSE)
    
  })

  output$mapa <- renderLeaflet({
    
    studies_initial <- readRDS('./data/studies.rds')
    
    leaflet(studies_initial) %>%
      fitBounds( lng1 = -137.7246, lat1 = 14.77488,
                 lng2 = -57.74414, lat2 = 55.87531) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(label = ~title, color="blue") 
  })
  
  observeEvent(input$api_request, {
    
    print(input$mapa_bounds)
    center_and_radius <- get_center_and_radius(input$mapa_bounds)
    new_data <- get_studies(lat=center_and_radius$latitude,
                            lon=center_and_radius$longitude,
                            radius=center_and_radius$radius,
                            status=input$status,
                            phase=input$phase, 
                            condition=input$condition)
    
    if (!is.null(new_data)) {
      
      new_data <- new_data$data %>%
        filter_map_bounds(input$mapa_bounds)
      
      studies(new_data)
    }

  })
  
  observe({
    
    studies_df <- studies()

    if(nrow(studies_df)>0) {
      leafletProxy("mapa", session, data=studies_df) %>%
        clearMarkers() %>%
        addCircleMarkers(
          ~lon, ~lat,
          popup= ~paste0('<b>', title, '</b><br>',
                         org,'<br>', 
                         'Status: ', status, '<br>',
                         '<a href="', link, '" target="_blank">View Study Page</a>'),
          #label = ~title, #lapply(lbl, htmltools::HTML),
          color="blue")
    } else {
      leafletProxy("mapa", session) %>%
        clearMarkers()
    }

  })
}
