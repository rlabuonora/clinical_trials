function(input, output, session) {
  
  studies <- reactiveVal({
    #readRDS('./data/studies.rds')
    center_and_radius <- get_center_and_radius(initial$mapa_bounds)
    new_data <- get_studies(lat=center_and_radius$latitude,
                            lon=center_and_radius$longitude,
                            radius=center_and_radius$radius,
                            status=initial$status,
                            phase=initial$phase, 
                            condition=initial$condition)
    new_data$data
  })
  
  # runs when button is clicked
  observeEvent(input$api_request, {
    
    center_and_radius <- get_center_and_radius(input$mapa_bounds)
    new_studies <- get_studies(lat=center_and_radius$latitude,
                            lon=center_and_radius$longitude,
                            radius=center_and_radius$radius,
                            status=input$status,
                            phase=input$phase, 
                            condition=input$condition)
    
    
    if (!is.null(new_studies$data)) {
      
      new_data <- new_studies$data %>%
        filter_map_bounds(input$mapa_bounds)
      
      studies(new_data)
    } else {
      studies(new_studies$data)
    }
    

  })
  
  # runs when app starts
  output$mapa <- renderLeaflet({

    leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(lng1 = initial$mapa_bounds$west, #west
                lat1 = initial$mapa_bounds$south,  # south
                lng2 = initial$mapa_bounds$east, # east
                lat2 = initial$mapa_bounds$north) %>%  # north
      addProviderTiles("CartoDB.Positron") 
  })
  
  observe({
    
    req(studies())
    
    studies_df <- studies()

    if(nrow(studies_df)>0) {
      leafletProxy("mapa", session, data=studies_df) %>%
        clearMarkers() %>%
        addCircleMarkers(
          clusterOptions = markerClusterOptions(),
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
  
  output$studies_tbl <- DT::renderDT({
    
    req(studies())
    
    studies_df <- studies() %>%
      mutate(title=str_c('<a href="', link, '" target="_blank">', str_trunc(title,100), '</a>')) %>%
      mutate(phase=phase %>% str_to_title() %>% str_replace_all("_", " ")) %>%
      mutate(status=status %>% str_to_title() %>% str_replace_all("_", " "))
      
    
    datatable(dplyr::select(studies_df, org, title, phase, status),
              selection = 'none',
              escape=FALSE,
              options = list(pageLength = 20,
                             columnDefs = list(
                               list(title="Organization",
                                    width="200px",
                                    targets=0),
                               list(title="Title",
                                    width="800px",
                                    targets=1),
                               list(title="Phase",
                                    targets=2),
                               list(title="Status",
                                    targets=3))),
              #colnames=c("Organization", "Title", "Phase", "Status"),
              rownames= FALSE)
    
  })
  
  
}
