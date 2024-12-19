
# Helper
process_study <- function(study) {
  
  safe_extract <- function(x) {
    if (is.null(x)) return(NA) else return(x)
  }

  title <- safe_extract(study$protocolSection$identificationModule$briefTitle)
  id <- safe_extract(study$protocolSection$identificationModule$nctId)
  org <- safe_extract(study$protocolSection$identificationModule$organization$fullName)
  status <- safe_extract(study$protocolSection$statusModule$overallStatus)
  endDate <- safe_extract(study$protocolSection$statusModule$completionDateStruct$date)
  phase <- safe_extract(study$protocolSection$designModule$phases[[1]])
  lon <- safe_extract(study$protocolSection$contactsLocationsModule$locations[[1]]$geoPoint$lon)
  lat <- safe_extract(study$protocolSection$contactsLocationsModule$locations[[1]]$geoPoint$lat)
  
  return(data.frame(
    title = title,
    org = org,
    status = status,
    phase=phase,
    endDate=endDate,
    lon=lon,
    lat=lat
  ))

}

# TODO: pass params to the API
get_studies <- function(map_bounds=NULL, phases=NULL, overallStatus=NULL, condition = "") {
  
  base_url <- "https://clinicaltrials.gov/api/v2/studies"

  query_params <- list(pageSize=20, 
                       #fields="protocolSection",
                       countTotal="true")
  
  # Add overallStatus if provided
  if (!is.null(overallStatus) && length(overallStatus) > 0) {
    query_params$`filter.overallStatus` <- paste(overallStatus, collapse = "|")
  }
  
  # Add condition if provided
  if (condition != "") {
    query_params$`query.cond` <- condition
  }
  
  if (!is.null(phase) && length(phase)>0) {
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
  
  if (!is.null(map_bounds)) {
    result <- result %>% 
    dplyr::filter(lon >= map_bounds$west & lon <= map_bounds$east &
                    lat >= map_bounds$south & lat <= map_bounds$north)
  }
  return(list(count=data$totalCount, 
              data=result))
  
}

studies <- get_studies()
#saveRDS(studies, file="data/studies.rds")
