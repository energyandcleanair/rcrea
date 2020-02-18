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

    targets <- reactive({
        country <- input$country
        city <- input$city
        poll <- input$poll
        req(country, city, poll)

        # Get measurements
        creadb::targets(country=country, city=city, poll=poll)
    })

    scales <- reactive({
        poll <- input$poll
        req(poll)

        # Get scales
        creadb::scales(poll=poll)
    })

    # Event Observers --------------------------------------
    observeEvent(input$averaging, {
        updateNumericInput(session, "running_width", label = paste("Running average (", input$averaging, ")",sep=""))
    })


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
    output$selectInputCity <- renderUI({
        selectInput("city", "City:", multiple=T, choices = (locations %>% filter(country==input$country))$city)
    })

    output$selectInputTarget <- renderUI({
        selectInput("target", "Applicable targets:", multiple=T, choices = targets()$short_name)
    })

    output$selectInputScale <- renderUI({
        selectInput("scale", "Applicable scales:", multiple=T, choices = scales()$name)
    })


    output$meas_plot <- renderPlot({
        poll <- input$poll
        averaging <- input$averaging
        plot_type <- input$plot_type
        running_width <- input$running_width
        city <- input$city
        country <- input$country

        req(poll, averaging, plot_type, city, country)

        type <- switch(plot_type,
               "ts" = "ts",
               "ts_year" = "ts",
               "heatmap" = "heatmap",
               "heatmap_w_text" = "heatmap_w_text")

        color_by <-  switch(plot_type,
                            "ts" = switch(input$overlayCities+1, NULL, "city"),
                            "ts_year" = "year",
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)

        subplot_by <-  switch(plot_type,
                            "ts" = switch(input$overlayCities+1, "city", NULL),
                            "ts_year" = "city",
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)

        meas_plot <- creadb::plot_measurements(meas(), input$poll, running_width=running_width, color_by=color_by, average_by=averaging, subplot_by=subplot_by, type=type)

        # Adding target lines if any
        if(!is.null(input$target)){
            for (i_target in 1:length(input$target)){
                target <- targets() %>% filter(short_name == input$target[i_target])
                target_line <- creadb::partial_plot_target(poll=poll, target=target, country=country, city=city, location_id=NULL,
                                                           average_by=averaging,
                                                           date_from = min(meas()$date), date_to = max(meas()$date),
                                                           type=type, color_by=color_by)

                if(!is.null(target_line)){
                    meas_plot <- meas_plot + target_line
                }
            }
        }

        # Adding scale colours if any and if timeseries
        if(type=='ts'){
            if(!is.null(input$scale)){
                for (i_scale in 1:length(input$scale)){
                    scale <- scales() %>% filter(name == input$scale[i_scale]) %>% filter(poll == poll)

                    if(plot_type=='ts_year'){
                        date_from <- as.POSIXct("0000-01-01")
                        date_to <- as.POSIXct("0001-01-01")
                    }else{
                        date_from <- min(meas()$date)
                        date_to <- max(meas()$date)
                    }
                    meas_plot <- add_plot_scale(meas_plot, scale=scale, date_from=date_from, date_to=date_to)
                }
            }
        }

        meas_plot
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

        exc() %>% filter(aggregation_period %in% input$exc_aggregation_period)
    })

    # output$exc_status_map <- renderPlot({
    #     creadb::map_exceedance_status(exc_status()) + geom_sf(data=sf::st_as_sf(countries_map()), fill = NA)
    # })
}
