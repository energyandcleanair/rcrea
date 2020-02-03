require(creadb)
library(lubridate)
library(raster)

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
        years <- input$years
        req(city, poll, averaging)

        date_from <- lubridate::ymd(years[1]*10000+101)
        date_to <- lubridate::ymd(years[2]*10000+1231)

        # Get measurements
        creadb::measurements(city=city, poll=poll, date_from=date_from, date_to=date_to, average_by=averaging, with_metadata = F)
    })


    # Observers -----------------------------------------------
    # Event Observers --------------------------------------
    # Output Elements --------------------------------------
    # Download Handlers ----------------------------------


    # Output Elements --------------------------------------
    output$meas_plot <- renderPlot({
        poll <- input$poll
        averaging <- input$averaging
        plot_type <- input$plot_type
        req(poll, averaging, plot_type)

        type <- switch(plot_type,
               "ts" = "ts",
               "ts_year" = "ts",
               "heatmap" = "heatmap",
               "heatmap_w_text" = "heatmap_w_text")

        color_by <-  switch(plot_type,
                            "ts" = "city",
                            "ts_year" = "year",
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)

        subplot_by <-  switch(plot_type,
                            "ts" = NULL,
                            "ts_year" = "city",
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)

        creadb::plot_measurements(meas(), input$poll, running_days=NULL, color_by=color_by, average_by=averaging, subplot_by=subplot_by, type=type)
    })


    # Reactive Values ---------------------------------------
    countries_map <- reactive({
        getData("GADM", country=input$country, level=1)
    })

    exc_status <- reactive({
        year <- input$year
        country <- input$country
        standard_orgs <- input$standard_org
        req(year, country)

        # Get exceedances
        creadb::exceedance_status(country=country, year=year, organization=standard_orgs, with_location = T)
    })

    # Output Elements --------------------------------------
    output$exc_status_table <- renderDataTable({
        exc_status()
    })

    output$exc_status_map <- renderPlot({
        creadb::map_exceedance_status(exc_status()) + geom_sf(data=sf::st_as_sf(countries_map()), fill = NA)
    })
}
