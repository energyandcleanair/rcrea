require(rcrea)
library(lubridate)
library(scales)
library(shinyWidgets)

server <- function(input, output, session) {

    # Global Variables ---------------------------------------
    # Reactive Lists ------------------------------------------
    # Reactive Values ---------------------------------------
    # General Observers -----------------------------------

    # Tab 1 -----------------------------------------------------
    # Reactive Values ---------------------------------------
    meas <- reactive({

        # To trigger refresh
        input$meas_refresh
        source <- isolate(input$source)
        country <- isolate(input$country)
        city <- isolate(input$city)
        poll <- isolate(input$poll)
        averaging <-  isolate(input$averaging)
        years <- isolate(input$years)
        req(country, city, poll, averaging, years)

        print("Fetching measurements")

        if(all(city == wholecountry_name)){
            city = NULL
            aggregate_level='country'
        }else{
            aggregate_level='city'
        }

        date_from <- lubridate::ymd(years[1]*10000+101)
        date_to <- lubridate::ymd(years[2]*10000+1231)

        # Get measurements
        rcrea::measurements(country=country, city=city, poll=poll, date_from=date_from, date_to=date_to, average_by=averaging, aggregate_level=aggregate_level, source=source, with_metadata = F, deweathered=NULL)
    })

    targets <- reactive({

        # Make it reactive to meas
        meas()

        country <- isolate(input$country)
        city <- isolate(input$city)
        poll <- isolate(input$poll)
        req(country, city, poll)

        # Get measurements
        rcrea::targets(country=country, city=city, poll=poll)
    })

    scales <- reactive({

        # Make it reactive to meas
        meas()

        poll <- isolate(input$poll)
        req(poll)

        # Get scales
        rcrea::scales(poll=poll)
    })

    # Event Observers --------------------------------------
    observeEvent(input$averaging, {
        updateNumericInput(session, "running_width", label = paste("Running average (", input$averaging, ")",sep=""))
    })


    # Download Handlers ----------------------------------
    # Downloadable csv of selected dataset ----
    output$download_csv <- downloadHandler(
        filename = function() {
            paste("measurements.csv", sep = "")
        },
        content = function(file) {
            source_ <- input$source
            write.csv(meas() %>% dplyr::filter(source==source_), file, row.names = FALSE)
        }
    )

    output$download_rds <- downloadHandler(
        filename = function() {
            paste("measurement.rds", sep = "")
        },
        content = function(file){
            source_ <- input$source
            saveRDS(meas() %>% dplyr::filter(source==source_), file)
        }
    )


    # Output Elements --------------------------------------

    output$selectInputCountry <- renderUI({
        req(input$source)
        filtered_locations <- locations %>% dplyr::filter(source==input$source)
        countries <- unique(filtered_locations$country)
        countries <- countries[!is.na(countries)]
        names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name', custom_match = list(XK='Kosovo')))
        countries <- countries[!is.na(names(countries))]

        selectInput("country", "Country:", multiple=T, choices = countries)
    })

    output$selectInputCity <- renderUI({
        req(input$country)
        filtered_locations <- locations %>% dplyr::filter(source==input$source)

        choices = c(wholecountry_name, (filtered_locations %>%
                                            dplyr::filter(country==input$country))$city)
        selectInput("city", "City/Region:", multiple=T, choices = choices)
    })

    output$selectInputTarget <- renderUI({
        selectInput("target", "Applicable targets:", multiple=T, choices = targets()$short_name)
    })

    output$selectInputScale <- renderUI({
        selectInput("scale", "Applicable scales:", multiple=T, choices = scales()$name)
    })

    output$selectInputProcess <- renderUI({
        require(meas())
        process_ids <- unique(meas()$process_id)
        choices = process_ids
        selectInput("process", "Processing:", multiple=F, choices = choices)
    })


    output$meas_plot <- renderPlot({

        # To trigger refresh
        input$meas_refresh

        poll <- isolate(input$poll)
        averaging <- isolate(input$averaging)
        city <- isolate(input$city)
        source_ <- isolate(input$source)

        # Plotting parameteres
        months <- input$months
        running_width <- input$running_width
        scales <- input$scale
        targets <- input$target
        plot_type <- input$plot_type
        process_ <- input$process

        req(poll, averaging, plot_type, city, months, source_)

        type <- switch(plot_type,
               "ts" = "ts",
               "ts_year" = "ts",
               "heatmap" = "heatmap",
               "heatmap_w_text" = "heatmap_w_text")

        color_by <-  switch(plot_type,
                            "ts" = switch(input$overlayCities+1, NULL, "region_id"),
                            "ts_year" = "year",
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)

        subplot_by <-  switch(plot_type,
                            "ts" = switch(input$overlayCities+1,
                                          c(if(length(poll)>1) "poll" else NULL,
                                            if(length(city)>1) "region_id" else NULL),
                                          , NULL),
                            "ts_year" = c(if(length(poll)>1) "poll" else NULL,
                                          if(length(city)>1) "region_id" else NULL),
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)



        meas_plot_data <- meas() %>% dplyr::filter(lubridate::month(date)>=months[1],
                                                   lubridate::month(date)<=months[2],
                                                   source==source_,
                                                   process_id==process_)

        meas_plot <- plot_measurements(meas_plot_data, poll=poll, running_width=running_width, color_by=color_by, average_by=averaging, subplot_by=subplot_by, type=type)

        if(plot_type=='ts_year'){
            month_date <- meas_plot_data$date
            lubridate::year(month_date) <- 0
            meas_plot <- meas_plot + scale_x_datetime(limits=c(min(month_date),max(month_date)),
                                                      breaks = seq(min(month_date),max(month_date), "1 month"),
                                                      labels=scales::date_format("%b", tz=attr(min(month_date),"tz"))
                                                      )
        }

        # Adding target lines if any
        if(!is.null(targets)){
            for (i_target in 1:length(target)){
                target <- targets() %>% dplyr::filter(short_name == targets[i_target])
                target_line <- rcrea::partial_plot_target(poll=poll, target=target, country=country, city=city, location_id=NULL,
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
            if(!is.null(scales)){
                for (i_scale in 1:length(scales)){
                    scale <- scales() %>% dplyr::filter(name == scales[i_scale]) %>% dplyr::filter(poll == poll)

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

    exc <- reactive({
        country <- input$exc_country
        year <- input$exc_year
        city <- input$exc_city
        req(year, country, city)

        # Get exceedances status
        exceedances(country=country, city=city, year=year) %>%
            mutate(status=cut(
                ifelse(exceedance_allowed_per_year==0,
                       exceedance_this_year, exceedance_this_year/exceedance_allowed_per_year),
                breaks=exc_status_breaks,
                labels=exc_status_labels
            ))
    })


    # Downloadable csv of selected dataset ----
    output$exc_download_csv <- downloadHandler(
        filename = function() {
            paste("standard_exceedances.csv", sep = "")
        },
        content = function(file) {
            write.csv(exc(), file, row.names = FALSE)
        }
    )

    output$exc_download_rds <- downloadHandler(
        filename = function() {
            paste("standard_exceedances.rds", sep = "")
        },
        content = function(file) {
            saveRDS(exc(), file)
        }
    )

    # Output Elements --------------------------------------
    output$selectInputExcCity <- renderUI({
        cities <- unique((locations %>% dplyr::filter(country==input$country))$city)
        pickerInput("exc_city","City", choices=cities, options = list(`actions-box` = TRUE),multiple = T)
        # selectInput("exc_city", "City:", multiple=T, selected = cities, choices = cities)
    })

    output$exc_table <- DT::renderDataTable({
         DT::datatable(data=exc() %>%
            dplyr::filter(aggregation_period %in% input$exc_aggregation_period) %>%
            dplyr::filter(poll %in% input$exc_poll) %>%
            dplyr::filter(standard_org %in% input$exc_standard_org) %>%
            dplyr::filter(status %in% input$exc_status) %>%
            dplyr::mutate(threshold_str=paste0(threshold," ", unit, " [", aggregation_period,"]")) %>%
            dplyr::select(
                city,
                poll,
                status,
                exceedance_this_year,
                exceedance_allowed_per_year,
                breach_date,
                threshold_str,
                standard_org,
            ),
            options = list(
                columnDefs = list(list(visible=FALSE, targets=c())),
                pageLength = 15,
                autoWidth = TRUE,
                rowCallback = JS(
                    "function(row, data) {",
                    "var n_exc = data[3];",
                    "var str_exc = n_exc < 1 ? (n_exc * 100).toFixed(0).toString() + '%' :  Math.floor(n_exc).toString() + ' times';",
                    "$('td:eq(3)', row).html(str_exc);",
                    "}"
                    )
                # callback = JS("var tips = ['tooltip1', 'tooltip2', 'tooltip3', 'tooltip4', 'tooltip5'],
                #             firstRow = $('#exc_status_table thead tr th');
                #             for (var i = 0; i < tips.length; i++) {
                #               $(firstRow[i]).attr('title', tips[i]);
                #             }")
         ),
         rownames = FALSE,
         ) %>%
            formatDate(c(6), "toLocaleDateString") %>%
            formatStyle(
                'status',
                target = 'row',
                backgroundColor = styleEqual(exc_status_labels, exc_status_colours)
            )
    })



    # output$exc_status_map <- renderPlot({
    #     rcrea::map_exceedance_status(exc_status()) + geom_sf(data=sf::st_as_sf(countries_map()), fill = NA)
    # })

    # Tab 3 : Download -----------------------------------------------------
    down <- reactive({

        # Take a dependency on input$down_refresh
        input$down_refresh

        source <- isolate(input$source)
        country <- isolate(input$down_country)
        years <- isolate(input$down_years)
        city <- isolate(input$down_city)
        poll <- isolate(input$down_poll)
        averaging <- isolate(input$down_averaging)
        req(years, country, city, poll, averaging)

        date_from <- lubridate::ymd(years[1]*10000+101)
        date_to <- lubridate::ymd(years[2]*10000+1231)

        # Get measurements
        rcrea::measurements(country=country, city=city, poll=poll, date_from=date_from, date_to=date_to, average_by=averaging, source=source, with_metadata = F) %>%
            dplyr::select(region_id, date, poll, unit, source, timezone, process_id) %>%
            dplyr::arrange(desc(date))
    })


    output$selectInputDownCity <- renderUI({
        cities <- unique((locations %>% dplyr::filter(country==input$down_country))$city)
        pickerInput("down_city","City", choices=cities, options = list(`actions-box` = TRUE), multiple = T)
        # selectInput("exc_city", "City:", multiple=T, selected = cities, choices = cities)
    })

    output$down_table <- DT::renderDataTable({
        format_date_str <- ifelse(input$down_averaging=='hour', 'toLocaleTimeString', 'toLocaleDateString')
        DT::datatable(data=down(),
                      options = list(
                          columnDefs = list(list(visible=FALSE, targets=c())),
                          pageLength = 15,
                          autoWidth = TRUE,
                          rowCallback = JS(
                              "function(row, data) {",
                              "var timezone = data[5];",
                              "var datetime = new Date(Date.parse(data[1]));",
                              "const options = {
                                  timeZone: timezone,
                              };",
                              "var str_datetime = datetime.toLocaleDateString('en-GB',options) +'  '+ datetime.toLocaleTimeString('en-US',options);",
                              "$('td:eq(1)', row).html(str_datetime);",
                              "}"
                          )
                          ),
                      rownames = FALSE
        ) # %>% formatDate(c(2), format_date_str)
    })

    output$down_download_csv <- downloadHandler(
        filename = function() {
            paste("measurements.csv", sep = "")
        },
        content = function(file) {
            write.csv(down(), file, row.names = FALSE)
        }
    )

    output$down_download_rds <- downloadHandler(
        filename = function() {
            paste("measurements.rds", sep = "")
        },
        content = function(file) {
            saveRDS(down(), file)
        }
    )

}
