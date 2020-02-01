library(creadb)
library(lubridate)

server <- function(input, output) {

    # Global Variables ---------------------------------------
    # Reactive Lists ------------------------------------------
    # Reactive Values ---------------------------------------
    # General Observers -----------------------------------

    # Tab 1 -----------------------------------------------------
    # Reactive Values ---------------------------------------
    meas <- reactive({
        city <- input$city
        poll <- input$poll
        averaging <- input$averaging
        req(city, poll, averaging)

        # Get measurements
        creadb::measurements(city=city, poll=poll, average_by=averaging, with_metadata = F)
    })


    # Observers -----------------------------------------------
    # Event Observers --------------------------------------
    # Output Elements --------------------------------------
    # Download Handlers ----------------------------------


    # Output Elements --------------------------------------
    output$meas_plot <- renderPlot({
        poll <- input$poll
        averaging <- input$averaging
        req(poll, averaging)
        creadb::plot_measurements(meas(), input$poll, running_days=NULL, color_by='city', average_by=averaging, subplot_by=NULL, type='ts')
    })


    # Reactive Values ---------------------------------------
    exc_status <- reactive({
        year <- input$year
        country <- input$country
        standard_orgs <- input$standard_org
        req(year, country)

        # Get exceedances
        creadb::exceedance_status(country=country, year=year, with_location = T)
    })

    # Output Elements --------------------------------------
    output$exc_status_map <- renderPlot({
        creadb::map_exceedance_status(exc_status())
    })
}
