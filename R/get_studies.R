# Helpers

# API call


library(httr2)
library(stringr)

get_studies <- function(lat = 39.0035707, lon = -77.1013313,
                        radius = 50, phases = NULL,
                        status = NULL,
                        condition = "",
                        search_term = "") {
  
  base_url <- "https://clinicaltrials.gov/api/v2/studies"
  
  # Initialize query parameters
  query_params <- list(
    pageSize = 1000,  # Page size (adjust if API allows a larger value)
    sort = "@relevance",
    fields = paste(
      "protocolSection.identificationModule",
      "protocolSection.statusModule",
      "protocolSection.designModule",
      "protocolSection.contactsLocationsModule",
      sep = ","
    ),
    countTotal = "true"
  )
  
  # Add optional parameters
  if (condition != "") {
    query_params$`query.cond` <- condition
  }
  
  if (FALSE) {
    
    geo_filter <- paste0("distance(", lat, ",", lon, ",", radius, "mi)")
    query_params$`filter.geo` <- geo_filter
    
  }
  
  if (!is.null(status) && length(status) > 0) {
    query_params$`filter.overallStatus` <- paste(status, collapse = "|")
  }
  
  if (!is.null(phases) && length(phases) > 0) {
    query_params$`query.term` <- paste(phases, collapse = "|")
  }
  
  if (search_term != "") {
    phases <- c(phases, search_term)
  }
  
  if (!is.null(phases) && length(phases)>0) {
    query_params$`query.term` <- paste(phases, collapse = "|")
  }
  
  # Initialize variables for pagination
  all_studies <- list()  # To collect all studies
  next_token <- NULL     # Initial nextToken is NULL
  
  repeat {
    # Add the nextToken to the query parameters (if it exists)
    if (!is.null(next_token)) {
      query_params$pageToken <- next_token
    }
    
    # Build the request
    req <- request(base_url) |>
      req_url_query(!!!query_params) |>  # Add all query parameters
      req_headers(`accept` = "application/json")
    
    # Log the query URL for debugging
    #message("Query URL: ", req_url(req))
    
    # Perform the request
    response <- tryCatch(
      req_perform(req),
      error = function(e) {
        warning("Network error: ", e$message)
        return(NULL)
      }
    )
    
    # Handle errors in the response
    if (is.null(response) || resp_is_error(response)) {
      warning("Failed to fetch data. HTTP error: ", resp_status(response))
      return(NULL)
    }
    
    # Parse the response content
    data <- tryCatch(
      resp_body_json(response),
      error = function(e) {
        warning("Failed to parse API response: ", e$message)
        return(NULL)
      }
    )
    
    # Append studies to the list
    if (!is.null(data$studies)) {
      all_studies <- append(all_studies, data$studies)
    }
    
    # Update nextToken
    next_token <- data$nextPageToken
    
    # Exit loop if no more pages
    if (is.null(next_token)) break
  }
  
  # Combine all studies into a data frame
  if (length(all_studies) == 0) {
    result <- data.frame(
      title = character(0),
      org = character(0),
      status = character(0),
      phase = character(0),
      link = character(0),
      endDate = character(0),
      lon = character(0),
      lat = character(0)
    )
  } else {
    result <- do.call(rbind, lapply(all_studies, process_study))
  }
  
  return(list(count = length(all_studies), data = result))
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
  
  print(map_bounds)
  dplyr::filter(df, lon >= map_bounds$west & lon <= map_bounds$east &
                  lat >= map_bounds$south & lat <= map_bounds$north)
}

