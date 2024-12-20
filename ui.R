library(shinydashboard)
library(leaflet)
library(shinycssloaders)

header <- dashboardHeader(
  title = "",
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
               leafletOutput("mapa", 
                             height = 600)),
    column(width=4,
              selectizeInput("phase",
                                  "Phase",
                                  choices = PHASES, 
                                  multiple = TRUE),
              selectizeInput("status",
                          "Status",
                          choices = STATUSES, 
                          multiple = TRUE),
           textInput("condition", "Condition"),
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
                 DT::dataTableOutput("studies_tbl")))))
  )))




dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)