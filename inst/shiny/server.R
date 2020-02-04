require(creadb)
library(lubridate)
library(raster)

server <- function(input, output, session) {

    # Global Variables ---------------------------------------
    # Reactive Lists ------------------------------------------
    # Reactive Values ---------------------------------------
    # General Observers -----------------------------------

    # Tab 1 -----------------------------------------------------
    # Reactive Values ---------------------------------------
    meas <- reactive({
        city <- input$city
        poll <- input$poll
        averaging <-  input$averaging
        years <- input$years
        req(city, poll, averaging, years)

        date_from <- lubridate::ymd(years[1]*10000+101)
        date_to <- lubridate::ymd(years[2]*10000+1231)

        # Get measurements
        creadb::measurements(city=city, poll=poll, date_from=date_from, date_to=date_to, average_by=averaging, with_metadata = F)
    })


    # Observers -----------------------------------------------

    # Event Observers --------------------------------------
    observeEvent(input$averaging, {
        updateNumericInput(session, "running_width", label = paste("Running average (", input$averaging, ")",sep=""))
    })

    # Output Elements --------------------------------------
    # Download Handlers ----------------------------------
    # Downloadable csv of selected dataset ----
    output$downloadMeas <- downloadHandler(
        filename = function() {
            paste("measurements_", input$city,"_",input$poll, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(meas(), file, row.names = FALSE)
        }
    )


    # Output Elements --------------------------------------
    output$meas_plot <- renderPlot({
        poll <- input$poll
        averaging <- input$averaging
        plot_type <- input$plot_type
        running_width <- input$running_width

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

        creadb::plot_measurements(meas(), input$poll, running_width=running_width, color_by=color_by, average_by=averaging, subplot_by=subplot_by, type=type)
    })

    # Tab 2 -----------------------------------------------------

    # Reactive Values ---------------------------------------
    # countries_map <- reactive({
    #     getData("GADM", country=input$country, level=1)
    # })

    exc_status <- reactive({
        year <- input$exc_year
        city <- input$exc_city
        poll <- input$exc_poll
        standard_org <- input$exc_standard_org
        req(year, city, poll, standard_org)

        # Get exceedances status
        creadb::exceedance_status(city=city, year=year, poll=poll, standard_org=standard_org, with_location = F)
    })

    exc <- reactive({
        year <- input$exc_year
        city <- input$exc_city
        poll <- input$exc_poll
        standard_org <- input$exc_standard_org
        req(year, city, poll, standard_org)

        date_from=lubridate::ymd(year*10000+101)
        date_to = lubridate::ymd(year*10000+1231)

        # Get exceedances
        creadb::exceedances(city=city, poll=poll, date_from=date_from, date_to=date_to, standard_org=standard_org)
    })

    # Downloadable csv of selected dataset ----
    output$exc_download <- downloadHandler(
        filename = function() {
            paste("standard_exceedances.csv", sep = "")
        },
        content = function(file) {
            write.csv(exc(), file, row.names = FALSE)
        }
    )

    # Output Elements --------------------------------------
    output$exc_status_table <- DT::renderDataTable({
        exc_status()
    })

    output$exc_table <- DT::renderDataTable({
        exc()
    })

    # output$exc_status_map <- renderPlot({
    #     creadb::map_exceedance_status(exc_status()) + geom_sf(data=sf::st_as_sf(countries_map()), fill = NA)
    # })


}
