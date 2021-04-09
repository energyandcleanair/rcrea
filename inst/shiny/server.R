library(shiny)
library(shinydashboard)
require(rcrea)
library(lubridate)
library(scales)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras2)

# library(creatrajs)
library(plotly)

server <- function(input, output, session) {

    # URL Management
    observe({
        query <- parseQueryString(session$clientData$url_search)
        if(!is.null(query$tab)) {
            updateNavbarPage(session,
                             "nav-page",
                             selected = query$tab)
        }
    })


    # Tab 1: Measurements  -----------------------------------------------------
    source(file.path("server", "tab_measurements.R"),  local = TRUE)$value

    # Tab 2: Exceedances -----------------------------------------------------
    # source(file.path("server", "tab_exceedances.R"),  local = TRUE)$value

    # Tab 3: Trajectories  -----------------------------------------------------
    source(file.path("server", "tab_trajectories.R"),  local = TRUE)$value

    # Tab 4 : Download -----------------------------------------------------
    # source(file.path("server", "tab_download.R"),  local = TRUE)$value
}
