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
      setView(lng = 0, lat = 0, zoom = 1.5) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(label = ~title, color="blue") 
  })
  
  observeEvent(input$api_request, {
    
    #print(input$mapa_bounds)
    center_and_radius <- get_center_and_radius(input$mapa_bounds)
    print(input$mapa_bounds)
    print(center_and_radius)
    new_data <- get_studies(lat=center_and_radius$latitude,
                            lon=center_and_radius$longitude,
                            radius=center_and_radius$radius,
                            status="COMPLETED",
                            phase=input$phase, 
                            condition=input$condition)
    
    if (!is.null(new_data)) {
      
      new_data <- new_data$data |> 
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
          label = ~title, #lapply(lbl, htmltools::HTML),
          color="blue")
    } else {
      leafletProxy("mapa", session) %>%
        clearMarkers()
    }

  })
}
