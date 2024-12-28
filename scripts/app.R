library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(fresh)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(fresh)
library(DT)
library(htmltools)
library(stringr)


## 1. Helper functions for interacting with the API

get_studies <- function(lat=39.0035707, lon=-77.1013313, radius=50, phases=NULL, status=NULL, condition = "") {
  
  
  base_url <- "https://clinicaltrials.gov/api/v2/studies"
  
  geo_filter <- paste0("distance(", lat, ",", lon, ",", radius, "mi)")
  
  
  query_params <- list(
    `filter.geo` = geo_filter,
    pageSize=10000, 
    sort="LastUpdatePostDate",
    fields = paste(
      "protocolSection.identificationModule",
      "protocolSection.statusModule",
      "protocolSection.designModule",
      "protocolSection.contactsLocationsModule",
      sep = ","
    ),
    countTotal="true")
  
  # Add condition if provided
  if (condition != "") {
    query_params$`query.cond` <- condition
  }
  
  if (!is.null(status) && length(status) > 0) {
    query_params$`filter.overallStatus` <- paste(status, collapse = "|")
  }
  
  if (!is.null(phases) && length(phases)>0) {
    query_params$`query.term` <- paste(phases, collapse = "|")
  }
  
  # Construct the full URL with query parameters
  url <- httr::modify_url(base_url, query = query_params)
  
  # Make the API request
  response <- tryCatch(
    {
      httr::GET(url, httr::add_headers(`accept` = "application/json"))
    },
    error = function(e) {
      warning("Network error: ", e$message)
      return(NULL)
    }
  )
  
  # Handle edge cases for the response
  if (is.null(response) || httr::http_error(response)) {
    warning("Failed to fetch data. HTTP error: ", httr::status_code(response))
    return(NULL)
  }
  
  # Parse the content of the response
  data <- tryCatch(
    {
      httr::content(response, as = "parsed", type = "application/json")
    },
    error = function(e) {
      warning("Failed to parse API response: ", e$message)
      return(NULL)
    }
  )
  
  # Handle 
  if (data$totalCount==0) {
    result <- data.frame(
      title=character(0),
      org=character(0),
      status=character(0),
      phase=character(0),
      link=character(0),
      endDate=character(0),
      lon=character(0),
      lat=character(0)
    )
  } else {
    result <- do.call(rbind, lapply(data$studies, process_study))
  }
  
  
  return(list(count=data$totalCount, 
              data=result))
  
}


# CONSTANTS
STATUSES <- c("ACTIVE_NOT_RECRUITING",
              "COMPLETED",
              "ENROLLING_BY_INVITATION",
              "NOT_YET_RECRUITING",
              "RECRUITING",
              "SUSPENDED",
              "TERMINATED",
              "WITHDRAWN",
              "AVAILABLE",
              "NO_LONGER_AVAILABLE",
              "TEMPORARILY_NOT_AVAILABLE",
              "APPROVED_FOR_MARKETING",
              "WITHHELD",
              "UNKNOWN")

STATUS_LABELS <- STATUSES %>% str_to_title() %>% str_replace_all("_", " ")
STATUS_CHOICES <- setNames(STATUSES, STATUS_LABELS)

PHASES <- c("NA",
            "EARLY_PHASE1",
            "PHASE1",
            "PHASE2",
            "PHASE3",
            "PHASE4")

PHASES_CHOICES <- setNames(PHASES, c("Na", "Early Phase 1", "Phase 1", "Phase 2", "Phase 3", "Phase 4"))

initial <- list(
  phase=PHASES[3],
  status=STATUSES[2],
  condition="breast cancer",
  mapa_bounds = list(north=55.87531, 
                     east=-57.74414,
                     south=14.77488,
                     west=-137.7246))


# Function to convert leaflet map bounds to center and radius for the API
get_center_and_radius <- function(map_bounds) {
  
  haversine <- function(lat1, lon1, lat2, lon2) {
    R <- 3958.8  # Earth's radius in miles
    lat1 <- lat1 * pi / 180
    lon1 <- lon1 * pi / 180
    lat2 <- lat2 * pi / 180
    lon2 <- lon2 * pi / 180
    dlat <- lat2 - lat1
    dlon <- lon2 - lon1
    a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(R * c)
  }
  
  
  if (map_bounds$north-map_bounds$south>=180) return()
  if (map_bounds$east-map_bounds$west>=180) return()
  # Extract bounds
  north <- map_bounds$north
  south <- map_bounds$south
  east <- map_bounds$east
  west <- map_bounds$west
  
  # Calculate center
  center_lat <- (north + south) / 2
  center_lon <- (east + west) / 2
  
  # Calculate radius (distance from center to a corner, e.g., north-east)
  radius <- haversine(center_lat, center_lon, north, east)
  
  return(list(
    latitude = round(center_lat, 5),
    longitude = round(center_lon, 5),
    radius = round(radius, 4)
  ))
}

# remove rows outside map bounds
filter_map_bounds <- function(df, map_bounds) {
  dplyr::filter(df, lon >= map_bounds$west & lon <= map_bounds$east &
                  lat >= map_bounds$south & lat <= map_bounds$north)
}


# extract data frame row from each result
process_study <- function(study) {
  
  safe_extract <- function(x) {
    if (is.null(x)) return(NA) else return(x)
  }
  
  identification_module <- study$protocolSection$identificationModule
  title <- safe_extract(identification_module$briefTitle)
  id <- safe_extract(identification_module$nctId)
  link <- paste0("https://clinicaltrials.gov/study/", id)
  org <- safe_extract(identification_module$organization$fullName)
  
  status_module <- study$protocolSection$statusModule
  
  status <- safe_extract(status_module$overallStatus) %>% str_to_title() %>% str_replace_all("_", " ")
  endDate <- safe_extract(status_module$completionDateStruct$date)
  
  phase <- safe_extract(study$protocolSection$designModule$phases[[1]]) %>% str_to_title() %>% str_replace_all("_", " ")
  
  lon <- safe_extract(study$protocolSection$contactsLocationsModule$locations[[1]]$geoPoint$lon)
  lat <- safe_extract(study$protocolSection$contactsLocationsModule$locations[[1]]$geoPoint$lat)
  
  return(data.frame(
    title = title,
    org = org,
    status = status,
    phase=phase,
    link=link,
    endDate=endDate,
    lon=lon,
    lat=lat
  ))
  
}


# Function to convert leaflet map bounds to center and radius for the API
get_center_and_radius <- function(map_bounds) {
  
  haversine <- function(lat1, lon1, lat2, lon2) {
    R <- 3958.8  # Earth's radius in miles
    lat1 <- lat1 * pi / 180
    lon1 <- lon1 * pi / 180
    lat2 <- lat2 * pi / 180
    lon2 <- lon2 * pi / 180
    dlat <- lat2 - lat1
    dlon <- lon2 - lon1
    a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(R * c)
  }
  
  
  if (map_bounds$north-map_bounds$south>=180) return()
  if (map_bounds$east-map_bounds$west>=180) return()
  # Extract bounds
  north <- map_bounds$north
  south <- map_bounds$south
  east <- map_bounds$east
  west <- map_bounds$west
  
  # Calculate center
  center_lat <- (north + south) / 2
  center_lon <- (east + west) / 2
  
  # Calculate radius (distance from center to a corner, e.g., north-east)
  radius <- haversine(center_lat, center_lon, north, east)
  
  return(list(
    latitude = round(center_lat, 5),
    longitude = round(center_lon, 5),
    radius = round(radius, 4)
  ))
}

# remove rows outside map bounds
filter_map_bounds <- function(df, map_bounds) {
  dplyr::filter(df, lon >= map_bounds$west & lon <= map_bounds$east &
                  lat >= map_bounds$south & lat <= map_bounds$north)
}




# Server
server <- function(input, output, session) {
  
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
    
    print(colnames(studies()))
    
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

# UI

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

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
  # tags$head(tags$style('#mapa-div .box-header{ display: none}')),
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
                 selectizeInput("phase",
                                "Phase",
                                choices = PHASES_CHOICES, 
                                selected = initial$phase,
                                multiple = TRUE),
                 selectizeInput("status",
                                "Status",
                                choices = STATUS_CHOICES, 
                                selected = initial$status,
                                multiple = TRUE),
                 textInput("condition", "Condition", value = initial$condition),
                 actionButton("api_request", "Fetch Data"))))),
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
      )
    )
  )
)




ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

shinyApp(ui, server)
