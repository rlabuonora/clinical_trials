library(fresh)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(fresh)
library(DT)
library(htmltools)
library(stringr)
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


# Helper function to filter programs within map bounds
# de bounds
filter_bounds <- function(data, bounds) {
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  stopifnot("latitude" %in% colnames(data))
  
  subset(data,
         latitude >= latRng[1] & latitude <= latRng[2] &
           longitude >= lngRng[1] & longitude <= lngRng[2])
}


# load data
status <- c("ACTIVE_NOT_RECRUITING",
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

PHASES <- c("NA",
           "EARLY_PHASE1",
           "PHASE1",
           "PHASE2",
           "PHASE3",
           "PHASE4")
