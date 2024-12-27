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



# load data
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
