library(shinydashboard)
library(leaflet)
library(shinycssloaders)

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
    ))
  ) # Close tabBox
) # Close dashboardBody




dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)