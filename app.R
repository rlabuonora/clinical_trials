library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(leaflet)

header <- dashboardHeader(
  title = "Clinical Trials Finder",
  dropdownMenu(
    type = "notifications", 
    icon = icon("question-circle"),
    badgeStatus = NULL,
    headerText = "Data Source:",
    
    notificationItem("https://clinicaltrials.gov/", 
                     icon = icon("link"),
                     href = "https://clinicaltrials.gov/")
  )
)


body <- dashboardBody(
  use_theme(mytheme),
  tabBox(
    width=NULL,
    tabPanel(
      title="Find Studies",
      box(
        width = NULL, 
        headerBorder=FALSE,
        status = "primary",
        fluidRow(
          style = "margin-bottom: 20px;",
          column(width = 6,
                 withSpinner(
                   leafletOutput("mapa", 
                                 height = 600))),
          column(width=4,
                 h2("Focus Your Search"),
                 selectizeInput("phase",
                                "Study phase",
                                choices = PHASES_CHOICES, 
                                selected = initial$phase,
                                multiple = TRUE),
                 selectizeInput("status",
                                "Study Status",
                                choices = STATUS_CHOICES, 
                                selected = initial$status,
                                multiple = TRUE),
                 textInput("condition", "Condition/Disease", value = initial$condition),
                 textInput("search_term", "Other terms"),
                 fluidRow(
                   column(2,
                          actionButton("api_request", "Fetch Data")),
                   column(8,
                          div(uiOutput("message"), style="margin-top: 7px;")
                   )
                 ))))),
    tabPanel(
      title="Explore Studies",
      box(
        width = NULL, 
        headerBorder=FALSE,
        title="",
        status = "primary",
        fluidRow(
          column(width=12,
                 withSpinner(
                   DT::dataTableOutput("studies_tbl")))))),
    tabPanel(
      title="FAQ",
      column(width=6,
             h1("Question 1"),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur."),
             h1("Question 2"),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.")
      ))
  ) # Close tabBox
) # Close dashboardBody




ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

server <- function(input, output, session) {
  
  
  studies <- reactiveVal({
    #readRDS('./data/studies.rds')
    center_and_radius <- get_center_and_radius(initial$mapa_bounds)
    new_data <- get_studies(lat=center_and_radius$latitude,
                            lon=center_and_radius$longitude,
                            radius=center_and_radius$radius,
                            status=initial$status,
                            phase=initial$phase, 
                            condition=initial$condition,
                            search_term = "")
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
                               condition=input$condition,
                               search_term = input$search_term)
    
    message(paste0("Found ", new_studies$count, " studies."))
    
    
    if (!is.null(new_studies$data)) {
      
      new_data <- new_studies$data 
      
      studies(new_data)
    } else {
      
      studies(new_studies$data)
    }
  })
  
  # runs when app starts
  output$mapa <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(lng1 = initial$mapa_bounds$west,   # west
                lat1 = initial$mapa_bounds$south,  # south
                lng2 = initial$mapa_bounds$east,   # east
                lat2 = initial$mapa_bounds$north) %>%  # north
      addProviderTiles("CartoDB.Positron") 
  })
  
  observe({
    
    req(studies())
    
    studies_df <- studies()
    
    if(nrow(studies_df)>0) {
      leafletProxy("mapa", session, data=studies_df) %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
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
        clearMarkers() %>%
        clearMarkerClusters()
    }
    
  })
  
  output$message <- renderText({
    paste0("Showing ", nrow(studies()), " studies.")
  })
  
  output$studies_tbl <- DT::renderDT({
    
    req(studies())
    
    print(nrow(studies()))
    
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

shinyApp(ui, server)

