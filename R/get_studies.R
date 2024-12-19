
# Helper


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
  
  status <- safe_extract(status_module$overallStatus)
  endDate <- safe_extract(status_module$completionDateStruct$date)
  
  phase <- safe_extract(study$protocolSection$designModule$phases[[1]])
  
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

# TODO: pass params to the API
get_studies <- function(lat=39.0035707, lon=-77.1013313, radius=50, phases=NULL, status=NULL, condition = "") {
  

  base_url <- "https://clinicaltrials.gov/api/v2/studies"
  
  geo_filter <- paste0("distance(", lat, ",", lon, ",", radius, "mi)")
  
  
  query_params <- list(
                       `filter.geo` = geo_filter,
                       pageSize=200, 
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
  
  # Return the parsed data
  result <- do.call(rbind, lapply(data$studies, process_study)) 
  

  return(list(count=data$totalCount, 
              data=result))
  
}

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

# Function to compute center and radius from map bounds
get_center_and_radius <- function(map_bounds) {
  
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


filter_map_bounds <- function(df, map_bounds) {
  dplyr::filter(df, lon >= map_bounds$west & lon <= map_bounds$east &
                  lat >= map_bounds$south & lat <= map_bounds$north)
}

