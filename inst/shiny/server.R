require(rcrea)
library(lubridate)
library(scales)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras2)
# library(creatrajs)
library(plotly)

server <- function(input, output, session) {

    observe({
        query <- parseQueryString(session$clientData$url_search)
        if(!is.null(query$tab)) {
            updateNavbarPage(session,
                             "nav-page",
                             selected = query$tab)
        }
    })

    # Global Variables ---------------------------------------
    # Reactive Lists ------------------------------------------
    # Reactive Values ---------------------------------------
    # General Observers -----------------------------------

    # Tab 1 -----------------------------------------------------
    # Reactive Values ---------------------------------------

    region_choices <- reactive({
        req(input$source)
        req(input$regionLevel)
        req(input$country)

        filtered_locations <- locations %>% dplyr::filter(source==input$source, level==input$regionLevel)
        region_name_col <- "name"
        region_id_col <- "id"

        l <- filtered_locations %>%
            dplyr::filter(country %in% input$country) %>%
            dplyr::filter_at(c(region_name_col, region_id_col), ~ !is.na(.)) %>%
            dplyr::distinct_at(c(region_id_col, region_name_col))

        choices = l %>% dplyr::pull(region_id_col)
        choices=setNames(choices, l %>% dplyr::pull(region_name_col))
        choices
    })

    meas <- reactive({

        # To trigger refresh
        input$meas_refresh
        source <- isolate(input$source)
        country <- isolate(input$country)
        region <- isolate(input$region)
        region_level <- isolate(input$regionLevel)
        poll <- isolate(input$poll)
        averaging <-  isolate(input$averaging)
        years <- isolate(input$years)
        req(country, region, poll, averaging, years)

        print("Fetching measurements")
        if(averaging == noaveraging_name){
            averaging = NULL
        }

        if(all(region == wholecountry_name)){
            region = NULL
            aggregate_level='country'
        }else{
            aggregate_level=region_level
        }

        date_from <- lubridate::ymd(years[1]*10000+101)
        date_to <- lubridate::ymd(years[2]*10000+1231)

        # Get measurements
        rcrea::measurements(country=country, location_id=region, poll=poll, date_from=date_from, date_to=date_to,
                            average_by=averaging, aggregate_level=aggregate_level, source=source,
                            with_metadata = T, deweathered=NULL, population_weighted = NULL)
    })

    targets <- reactive({

        # Make it reactive to meas
        meas()

        country <- isolate(input$country)
        city <- isolate(input$region)
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

    output$selectInputRegion <- renderUI({
        req(input$country)
        req(input$regionLevel)
        pickerInput("region",
                    input$regionLevel,
                    choices = region_choices(),
                    options = pickerOptions(
                        actionsBox=T,
                        selectedTextFormat="count > 3",
                        liveSearch=T),
                    multiple = T)
        # selectInput("region", "City/Region:", multiple=T, choices = region_choices())
    })

    output$selectInputTarget <- renderUI({
        selectInput("target", "Applicable targets:", multiple=T, choices = targets()$short_name)
    })

    output$selectInputScale <- renderUI({
        selectInput("scale", "Applicable scales:", multiple=T, choices = scales()$name)
    })

    output$selectInputProcess <- renderUI({
        req(meas())
        if(nrow(meas())==0){
            return(c())
        }
        #Select non-deweather / non-population-weighted by default: putting them first
        process_ids <- meas() %>%
            dplyr::distinct(process_id) %>%
            dplyr::left_join(processes, by=c("process_id"="id")) %>%
            dplyr::arrange(!is.na(deweather), !is.na(weighting)) %>%
            dplyr::pull(process_id)
        choices = process_ids
        value = ifelse(length(process_ids)>0, process_ids[1], NULL)
        selectInput("process", "Processing:", multiple=T, choices = choices, selected = value)
    })

    output$meas_plot_message <- renderText({
        req(meas())
        if(nrow(meas())==0){
            return(HTML("<div style='margin-top: 40px;'>No measurement found</div>"))
        }else{
            return(NULL)
        }
    })


    output$meas_plot <- renderPlot({

        # To trigger refresh
        input$meas_refresh

        if(nrow(meas())==0){
            return(NULL)
        }

        poll <- isolate(input$poll)
        averaging <- isolate(input$averaging)
        region <- isolate(input$region)
        source_ <- isolate(input$source)
        region_choices_ <- isolate(region_choices())

        # Plotting parameteres
        months <- input$months
        running_width <- input$running_width
        scales <- input$scale
        targets <- input$target
        plot_type <- input$plot_type
        process_ <- input$process

        req(poll, averaging, plot_type, region, months, source_)

        if(averaging == noaveraging_name){
            averaging = NULL
        }

        type <- switch(plot_type,
                       "ts" = "ts",
                       "ts_year" = "ts",
                       "yoy" = "yoy",
                       "yoy_year" = "yoy",
                       "heatmap" = "heatmap",
                       "heatmap_w_text" = "heatmap_w_text")

        color_by <-  switch(plot_type,
                            "ts" = switch(input$overlayCities+1, NULL, "location_name"),
                            "yoy" = switch(input$overlayCities+1, NULL, "location_name"),
                            "ts_year" = "year",
                            "yoy_year" = "year",
                            "heatmap" = NULL,
                            "heatmap_w_text" = NULL)

        subplot_by <-  switch(plot_type,
                              "ts" = switch(input$overlayCities+1,
                                            c(if(length(poll)>1) "poll" else NULL,
                                              if(length(region)>1) "location_name" else NULL),
                                            if(length(poll)>1) "poll" else NULL
                              ),
                              "yoy" = switch(input$overlayCities+1,
                                             c(if(length(poll)>1) "poll" else NULL,
                                               if(length(region)>1) "location_name" else NULL),
                                             if(length(poll)>1) "poll" else NULL
                              ),
                              "ts_year" = c(if(length(poll)>1) "poll" else NULL,
                                            if(length(region)>1) "location_name" else NULL),
                              "yoy_year" = c(if(length(poll)>1) "poll" else NULL,
                                             if(length(region)>1) "location_name" else NULL),
                              "heatmap" = NULL,
                              "heatmap_w_text" = NULL)



        meas_plot_data <- meas() %>% dplyr::filter(lubridate::month(date)>=months[1],
                                                   lubridate::month(date)<=months[2],
                                                   source==source_,
                                                   process_id %in% process_)

        # Replace region ids with region name
        id_to_name <- setNames(names(region_choices_),tolower(unname(region_choices_)))
        # meas_plot_data <- meas_plot_data %>% dplyr::mutate(location_name=id_to_name[region_id])
        if(nrow(meas_plot_data)==0) return()

        meas_plot <- rcrea::plot_measurements(meas_plot_data, poll=poll, running_width=running_width, color_by=color_by, average_by=averaging, subplot_by=subplot_by, type=type,
                                       linetype_by=ifelse(length(process_)>1,"process_id",NA))

        if(plot_type %in% c('ts_year','yoy_year')){
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
                target_line <- rcrea::partial_plot_target(poll=poll, target=target, country=country, city=region, location_id=NULL,
                                                          average_by=averaging,
                                                          date_from = min(meas()$date), date_to = max(meas()$date),
                                                          type=type, color_by=color_by)

                if(!is.null(target_line)){
                    meas_plot <- meas_plot + target_line
                }
            }
        }

        # Adding scale colours if any and if timeseries
        if(type %in% c('ts','yoy')){
            if(!is.null(scales)){
                for (i_scale in 1:length(scales)){
                    scale <- scales() %>% dplyr::filter(name == scales[i_scale]) %>% dplyr::filter(poll == !!poll)

                    if(plot_type %in% c('ts_year','yoy_year')){
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

    output$processes_table <- DT::renderDataTable({

        poll <- isolate(input$poll)
        averaging <- isolate(input$averaging)
        region <- isolate(input$region)
        source_ <- isolate(input$source)
        req(poll, averaging, region, source_)

        meas_ <- meas()
        req(meas_)

        DT::datatable(data=processes %>%
                          dplyr::filter(id %in% meas_$process_id) %>%
                          dplyr::select(id, "Filtering"=filter, "Spatial aggregation"=agg_spatial, "Temporal aggregation"=agg_temp, "Deweathering"=deweather)
                      ,
                      options = list(
                          dom = 't',
                          columnDefs = list(list(visible=FALSE, targets=c())),
                          pageLength = 15,
                          autoWidth = TRUE
                          # ,
                          # rowCallback = JS(
                          #     "function(row, data) {",
                          #     "var n_exc = data[3];",
                          #     "var str_exc = n_exc < 1 ? (n_exc * 100).toFixed(0).toString() + '%' :  Math.floor(n_exc).toString() + ' times';",
                          #     "$('td:eq(3)', row).html(str_exc);",
                          #     "}"
                          # )
                          # callback = JS("var tips = ['tooltip1', 'tooltip2', 'tooltip3', 'tooltip4', 'tooltip5'],
                          #             firstRow = $('#exc_status_table thead tr th');
                          #             for (var i = 0; i < tips.length; i++) {
                          #               $(firstRow[i]).attr('title', tips[i]);
                          #             }")
                      ),
                      rownames = FALSE,
        )
        # %>%
        #     formatDate(c(6), "toLocaleDateString") %>%
        #     formatStyle(
        #         'status',
        #         target = 'row',
        #         backgroundColor = styleEqual(exc_status_labels, exc_status_colours)
        #     )
    }
    )

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
            dplyr::mutate(status=cut(
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

    # Tab 3: Trajectories  -----------------------------------------------------

    # Some log related functions
    # trajs_logs_raw <- ""
    trajs_logs <- reactiveValues(msg="")

    trajs_add_log <- function(message){
        trajs_logs$msg <- paste(isolate(trajs_logs$msg), "\n", message)
    }

    read_gcs_url <- function(url, force_uncache=T){
        tryCatch({
            trajs_add_log(url)
            if(force_uncache){
                url <- paste0(url,"?a=1") # GCS caches files otherwise
            }
            r <- readRDS(url(gsub(" ","%20",url)))
            trajs_add_log("SUCCESS")
            r
        },error=function(e){
            trajs_add_log("FAILED")
            return(NULL)
        })
    }

    # Not refreshing for every intermediate step
    # when sliding date
    trajs_date <- reactive({
        input$trajs_date
    }) %>% debounce(1000)

    trajs_files <- reactive({
        trajs_add_log("Listing available files")
        # Get list of trajectories available
        gcs_get_bucket(trajs.bucket)
        files <- gcs_list_objects(prefix=paste0(trajs.folder,"/"),
                                  detail = "summary",
                                  delimiter = "/")
        trajs_add_log(sprintf("%d files found", nrow(files)))
        files.trajs <- files %>%
            dplyr::filter(stringr::str_detect(name, ".trajs.RDS$"))

        tibble::tibble(
            gcs_name=files.trajs$name,
            location_id=gsub(".trajs.RDS",
                             "",
                             basename(files.trajs$name)))
    })

    trajs_locations <- reactive({
        req(trajs_files())
        rcrea::locations(id=unique(trajs_files()$location_id),
                         level="city",
                         with_metadata=T,
                         with_geometry=T) %>%
            dplyr::left_join(trajs_files(),
                             by=c("id"="location_id")) %>%
            dplyr::distinct(id, name, country, gcs_name, geometry)
    })

    trajs_location_id <- reactive({
        req(input$trajs_city)
        req(input$trajs_country)

        trajs_locations() %>%
            dplyr::filter(country==input$trajs_country,
                          name==input$trajs_city) %>%
            dplyr::pull(id)
    })

    trajs_location_geometry <- reactive({
        req(input$trajs_city)
        req(input$trajs_country)

        trajs_locations() %>%
            dplyr::filter(country==input$trajs_country,
                          name==input$trajs_city) %>%
            dplyr::pull(geometry)
    })

    trajs <- reactive({
        req(trajs_location_id())

        gcs_name <- trajs_locations() %>%
            dplyr::filter(id==trajs_location_id()) %>%
            dplyr::pull(gcs_name)

        gcs_url <- paste0(trajs.bucket_base_url, gcs_name)
        read_gcs_url(gcs_url)
    })

    trajs_dates <- reactive({
        req(trajs())
        trajs() %>%
            dplyr::pull(date) %>%
            unique() %>%
            sort(decreasing=T)
    })

    # trajs_fires_all <- reactive({
    #     req(trajs_location_id())
    #
    #     gcs_url <- paste0(trajs.bucket_base_url,
    #                       trajs.folder,"/",
    #                       trajs_location_id(),
    #                       ".fires.RDS")
    #     read_gcs_url(gcs_url)
    # })

    trajs_weather <- reactive({
        req(trajs_location_id())

        gcs_url <- paste0(trajs.bucket_base_url,
                          trajs.folder,"/",
                          trajs_location_id(),
                          ".weather.RDS")
        read_gcs_url(gcs_url)
    })

    # trajs_fire <- reactive({
    #     req(trajs_fires_all())
    #     req(trajs_date())
    #
    #     trajs_fires_all() %>%
    #         dplyr::filter(lubridate::date(acq_date)==trajs_date())
    # })

    trajs_meas_all <- reactive({
        req(trajs_location_id())
        gcs_url <- paste0(trajs.bucket_base_url,
                          trajs.folder,"/",
                          trajs_location_id(),
                          ".meas.RDS")
        read_gcs_url(gcs_url)
    })

    trajs_meas_date <- reactive({

        req(trajs_meas_all())
        req(trajs_date())

        trajs_meas_all() %>%
            dplyr::filter(date==trajs_date())

    })

    # trajs_meas <- reactive({
    #     req(trajs_location_id())
    #     gcs_url <- paste0(trajs.bucket_base_url,
    #                       trajs.folder,"/",
    #                       trajs_location_id(),
    #                       ".dew.RDS")
    #     tryCatch({
    #         m.dew <-  readRDS(url(gcs_url))
    #
    #         # Get observations
    #         rcrea::measurements(location_id=trajs_location_id(),
    #                             date_from="2017-01-01",
    #                             deweathered = F,
    #                             # process_id="city_day_mad",
    #                             source=m.dew$source[1],
    #                             poll=m.dew$poll[1])
    #     }, error=function(c){
    #         return(NULL)
    #     })
    # })

    # trajs_plot_url <- reactive({
    #     date_ <- tolower(trajs_date())
    #     city_ <- tolower(input$trajs_city)
    #     country_ <- tolower(input$trajs_country)
    #     req(date_, country_, city_)
    #
    #     url.short <- trajs_files() %>%
    #         dplyr::filter(tolower(country)==country_,
    #                        date==date_,
    #                        tolower(city)==city_) %>%
    #         dplyr::pull(name) %>% as.character()
    #     paste("https://storage.googleapis.com", trajs.bucket, url.short, sep="/")
    #
    # })

    trajs_points <- reactive({
        req(trajs())
        req(trajs_date())

        date_ <- tolower(trajs_date())

        tryCatch({
            trajs() %>%
                dplyr::filter(date==date_) %>%
                tidyr::unnest(trajs, names_sep=".") %>%
                dplyr::select(date=trajs.traj_dt, lon=trajs.lon, lat=trajs.lat, run=trajs.run)
        }, error=function(e){
            trajs_add_log(sprintf("Failed to read trajectories (%s)",e))
            return(NULL)
        })
    })

    trajs_plot_poll <- reactive({

        req(trajs_meas_all())
        req(trajs_date())
        req(input$trajs_running_width)

        poll <- rcrea::poll_str(trajs_meas_all()$poll[1])
        unit <- trajs_meas_all()$unit[1]
        hovertemplate <- paste('%{y:.0f}',unit)
        m <- trajs_meas_all()     %>%
            select(date, observed, predicted, predicted_nofire)
        m.rolled <- rcrea::utils.running_average(m, input$trajs_running_width, vars_to_avg = c("observed","predicted","predicted_nofire"))


        # selected <- which(trajs_meas_obs()$date==trajs_date())
        m.rolled %>%
            plot_ly(
                type="scatter",
                mode="lines"
            ) %>%
            plotly::add_lines(x=~date,
                              y=~observed,
                              name="Observed",
                              opacity=0.4,
                              hovertemplate = hovertemplate,
                              line = list(
                                  color = 'rgb(0, 0, 0)',
                                  width = 1
                              )) %>%
            plotly::add_lines(x=~date,
                              y=~predicted,
                              name="Predicted with fire",
                              hovertemplate = hovertemplate,
                              line = list(
                                  color = 'red',
                                  width = 2
                              )) %>%
            plotly::add_lines(x=~date,
                              y=~predicted_nofire,
                              name="Predicted without fire",
                              hovertemplate = hovertemplate,
                              line = list(
                                  color = 'orange',
                                  width = 2
                              )) %>%
            plotly::layout(
                showlegend = F,
                hovermode  = 'x unified',
                # title=list(
                #     text=sprintf("%s [%s]",poll, unit),
                #     x=0.1,
                #     font=list(size=10)
                # ),
                yaxis = list(
                    # title="", #sprintf("%s [%s]",poll, unit),
                    rangemode = 'tozero'
                ),
                xaxis = list(
                    title="",
                    # showspikes = T,
                    spikemode  = 'across+toaxis',
                    spikesnap = 'cursor',
                    # spikedash = 'solid',
                    showline=T,
                    showgrid=T
                    ),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                fig_bgcolor   = "rgba(0, 0, 0, 0)"
                ) %>%
            plotly::add_annotations(
                text = sprintf("%s [%s]",poll, unit),
                x = -0.05,
                y = 1.15,
                yref = "paper",
                xref = "paper",
                xanchor = "left",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 12)
            )
    })

    trajs_plot_fire <- reactive({

        req(trajs_weather())
        req(input$trajs_running_width)

        f <- trajs_weather() %>%
            dplyr::select(date, value=fire_count)

        f.rolled <- rcrea::utils.running_average(f, input$trajs_running_width)

        # selected <- which(trajs_meas_obs()$date==trajs_date())
        f.rolled %>%
            plot_ly(
                x = ~date,
                y = ~value
                # selectedpoints=as.list(selected),
            ) %>%
            plotly::add_lines(name="Fire count") %>%
            plotly::layout(
                # title=list(
                #     text="Fire count (within 10km of trajectories)",
                #     x=0.1,
                #     font=list(size=10)
                # ),
                showlegend = F,
                hovermode  = 'x unified',
                yaxis = list(
                     title="", #Fire count (within 10km of trajectories)",
                    rangemode = 'tozero'
                ),
                xaxis = list(title="")) %>%
            plotly::add_annotations(
                    text = "Fire count (within 10km of trajectories)",
                    x = -0.05,
                    y = 1.15,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "left",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 12)
                )
    })

    trajs_plot_firecontribution <- reactive({

        req(trajs_meas_all())
        req(trajs_date())
        req(input$trajs_running_width)

        poll <- rcrea::poll_str(trajs_meas_all()$poll[1])
        unit <- trajs_meas_all()$unit[1]
        hovertemplate <- paste('%{y:.0f}',unit)
        m <- trajs_meas_all()     %>%
            select(date, observed, predicted, predicted_nofire) %>%
            mutate(value=predicted-predicted_nofire) %>%
            select(date, value)

        m.rolled <- rcrea::utils.running_average(m, input$trajs_running_width)


        # selected <- which(trajs_meas_obs()$date==trajs_date())
        m.rolled %>%
            plot_ly(
                type="scatter",
                mode="lines"
            ) %>%
            plotly::add_lines(x=~date,
                              y=~value,
                              hovertemplate = hovertemplate,
                              line = list(
                                  width = 2
                              )) %>%
            plotly::layout(
                # title=list(
                #     text=sprintf("Fire contribution to %s [%s]",poll, unit),
                #     x=0.1,
                #     font=list(size=10)
                #     ),
                showlegend = F,
                hovermode  = 'x unified',
                yaxis = list(
                    # title=sprintf("Fire contribution to %s [%s]",poll, unit),
                    rangemode = 'tozero'
                ),
                xaxis = list(
                    title="",
                    # showspikes = T,
                    spikemode  = 'across+toaxis',
                    spikesnap = 'cursor',
                    # spikedash = 'solid',
                    showline=T,
                    showgrid=T
                ),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                fig_bgcolor   = "rgba(0, 0, 0, 0)"
            ) %>%
            plotly::add_annotations(
                text = sprintf("Fire contribution to %s [%s]",poll, unit),
                x = -0.05,
                y = 1.15,
                yref = "paper",
                xref = "paper",
                xanchor = "left",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 12)
            )
    })

    # trajs_buffer <- reactive({
    #     req(trajs())
    #     req(trajs_date())
    #
    #     date_ <- tolower(trajs_date())
    #
    #     creatrajs::trajs.buffer(trajs()[trajs()$date==date_,"trajs"][[1]][[1]],
    #                             buffer_km=10)
    # })
    #


    # Download
    # output$trajs_download_jpg <- downloadHandler(
    #     # filename = function() {
    #     #     paste("trajectories.jpg", sep = "")
    #     # },
    #     # content = function(file) {
    #     #     write.csv(exc(), file, row.names = FALSE)
    #     # }
    # )


    # Output Elements --------------------------------------
    output$selectInputTrajsCountry <- renderUI({
        countries <- trajs_locations()$country %>% unique()
        names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name',
                                              custom_match = list(XK='Kosovo')))

        pickerInput("trajs_country","Country", choices=countries, options = list(`actions-box` = TRUE), multiple = F)
    })

    output$selectInputTrajsCity <- renderUI({
        req(input$trajs_country)
        cities <- trajs_locations() %>%
            dplyr::filter(country==input$trajs_country) %>%
            dplyr::pull(name) %>%
            unique()
        pickerInput("trajs_city","City", choices=cities, options = list(`actions-box` = TRUE), multiple = F)
    })

    output$selectInputTrajsDates <- renderUI({
        dates <- trajs_dates()
        # pickerInput("trajs_date","Date", choices=dates, options = list(`actions-box` = TRUE), multiple = F)
        # dateInput("trajs_date","Date", min=min(dates), max=max(dates))
        sliderInput("trajs_date",
                    NULL,
                    min = as.Date(min(dates),"%Y-%m-%d"),
                    max = as.Date(max(dates),"%Y-%m-%d"),
                    value=as.Date(max(dates)),
                    timeFormat="%Y-%m-%d",
                    ticks=T,
                    width="100%")
    })

    # output$trajsLogs <- renderText({
    #     req(trajs_logs)
    #     trajs_logs[["log"]]
    #     })

    output$trajsInfos <- renderUI({
        req(trajs_location_id())
        req(trajs_date())
        req(trajs_meas_date())

        l <- trajs_locations() %>%
            dplyr::filter(id==trajs_location_id())
        d <- trajs_meas_date()

        HTML(paste0("<b>",l$name," - ",d[["poll"]],"</b><br/>",
                    trajs_date(),"<br/>",
                    "Observed: ", round(d[["observed"]]), " ",d[["unit"]], "<br/>",
                    "Predicted: ", round(d[["predicted"]]), " ",d[["unit"]], "<br/>",
                    "Predicted (nofire): ", round(d[["predicted_nofire"]]), " ",d[["unit"]], "<br/>"
                    ))
    })

    output$trajsLogs <- renderText({
        trajs_logs$msg
    })

    output$trajsPlots <- renderPlotly({

        req(trajs_plot_poll())
        req(trajs_plot_fire())
        req(trajs_plot_firecontribution())

        plots <- list(
            trajs_plot_poll(),
            trajs_plot_firecontribution(),
            trajs_plot_fire()
        )

        plots <- plots[!is.na(plots)]

        plotly::subplot(plots,
                        nrows = length(plots),
                        shareX = TRUE,
                        titleX = FALSE,
                        titleY = FALSE,
                        shareY = T,
                        margin = 0.05
        ) %>%
            plotly::layout(hovermode='x',
                           xaxis = list(
                               title="",
                               # showspikes = T,
                               spikemode  = 'across+toaxis',
                               spikesnap = 'cursor',
                               # spikedash = 'solid',
                               showline=T,
                               showgrid=T)
            )


    })



    output$maptrajs <- renderLeaflet({
        leaflet(options = leafletOptions(preferCanvas = TRUE,
                                         zoomControl = FALSE)) %>%
            setView(80,30,6) %>%
            # addProviderTiles(providers$Stamen.TonerLite,
            #                  options = providerTileOptions(noWrap = TRUE)
            # )
            addProviderTiles('Stamen.Terrain', group="Terrain",
                             options=providerTileOptions(zindex=0)) %>%
            addProviderTiles('Esri.WorldImagery', group="Satellite",
                             options=providerTileOptions(zindex=0)) %>%
            addProviderTiles('OpenStreetMap', group = "OpenStreetMap",
                             options=providerTileOptions(zindex=0)) %>%
            # addProviderTiles("CartoDB.PositronOnlyLabels", group="Satellite") %>%
            # addProviderTiles('Esri.Topographic', group="Topographic") %>%
            # addProviderTiles('Esri.Terrain', group="Terrain") %>%
            addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
            # addProviderTiles("Esri.NatGeoWorldMap", group="NatGeo") %>%

            # leaflet.extras2::addGIBS(
            #     layers=c("MODIS_Combined_Value_Added_AOD"),
            #     group="Aerosol Optical Depth",
            #     dates=lubridate::today(),
            #     transparent = T,
            #     opacity = 0.7
            # ) %>%
            # leaflet.extras2::addGIBS(
            #     layers=c(
            #         "AIRS_L2_Dust_Score_Day"),
            #     group="Dust score",
            #     dates=lubridate::today(),
            #     transparent = T,
            #     opacity = 0.7
            # ) %>%
            # leaflet.extras2::addGIBS(
            #     layers=c(
            #         "MODIS_Combined_Value_Added_AOD"),
            #     group="Aerosol Optical Depth",
            #     dates=lubridate::today(),
            #     transparent = T,
            #     opacity = 0.7
            # ) %>%
            addLayersControl(
                baseGroups = c("Terrain", "Satellite", "OpenStreetMap", "Light"),
                overlayGroups = c("Trajectories", "Active fires"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })


    observe({

        req(trajs_date())

        wms_url <- sprintf("https://firms.modaps.eosdis.nasa.gov/wms/key/%s/",Sys.getenv("FIRMS_KEY"))
        wms_layer <- "fires_viirs_snpp"
        leaflet_layer_id <- "firms_wms"
        date_str <- strftime(as.Date(trajs_date()),"%Y-%m-%d")

        #https://firms.modaps.eosdis.nasa.gov/wms/key/YourMapKey/?REQUEST=GetMap&layers=fires_viirs,fires_modis&TIME=2020-01-01/2020-01-10&WIDTH=1024&HEIGHT=512&colors=240+40+40,250+200+50&size=2,2&BBOX=-180,-90,180,90


        leafletProxy("maptrajs") %>%
            removeTiles(leaflet_layer_id) %>%
            leaflet::addWMSTiles(
                wms_url,
                layers = wms_layer,
                layerId = leaflet_layer_id,
                group="Active fires",
                options = WMSTileOptions(
                    format = "image/png",
                    transparent = TRUE,
                    colors = "255+12+25",
                    TIME = date_str,
                    size=5,
                    zIndex=1000
                    ))
        # %>%
        #     leaflet.extras2::setDate(layers=c(
        #         "AIRS_L2_Dust_Score_Day","AIRS_L2_Dust_Score_Night"
        #         # "MODIS_Combined_Value_Added_AOD"
        #         ),
        #                              dates=as.Date(trajs_date()))

    })

    # Incremental changes to the map. Each independent set of things that can change
    # should be managed in its own observer.
    observe({

        req(trajs_points())
        # req(trajs_fire())

        map <- leafletProxy("maptrajs") %>%
            clearShapes() %>%
            clearMarkers

        trajs <- trajs_points() %>%
            dplyr::arrange(run, date)

        # fires <- trajs_fire()

        for(run in unique(trajs$run)){
            map <- addPolylines(map,
                                lng= ~ lon,
                                lat= ~ lat,
                                data = trajs[trajs$run==run,],
                                group = "Trajectories",
                                weight = 3)
        }

        # if(!is.null(fires)){
        #     map <- addCircleMarkers(map,
        #                             radius=5,
        #                             color="green",
        #                             stroke=F,
        #                             opacity=0.7,
        #                             group = "Active fires",
        #                             data = fires)
        # }

        map
    })

    observe({

        req(trajs_location_geometry())
        tryCatch({

            leafletProxy("maptrajs") %>%
                setView(
                    lng=sf::st_coordinates(trajs_location_geometry())[1],
                    lat=sf::st_coordinates(trajs_location_geometry())[2],
                    zoom = 6
                )
        }, error=function(e){NULL})


        # req(trajs())
        #
        # tryCatch({
        #     t<-trajs() %>%
        #         tidyr::unnest(trajs, names_sep=".") %>%
        #         dplyr::select(date=trajs.traj_dt, lon=trajs.lon, lat=trajs.lat, run=trajs.run)
        #
        #     buffer <- 0.5
        #     leafletProxy("maptrajs") %>%
        #         fitBounds(max(t$lon) + buffer,
        #                   max(t$lat) + buffer,
        #                   min(t$lon) - buffer,
        #                   min(t$lat) - buffer)
        # }, error=function(e){NULL})
    })




    # observe(trajs_logs_raw, {
    #     trajs_logs(trajs_logs_raw)
    #     })




    # output$imageTrajs <- renderUI({
    #     imgurl <- trajs_plot_url()
    #     # tags$img(src=imgurl[1], height=800) #TODO account for various met_types
    #
    #     image_output_list <-
    #         lapply(1:length(imgurl),
    #                function(i)
    #                {
    #                    tags$img(src=imgurl[i], height=800)
    #                    # imagename = i
    #                    # imageOutput(imagename)
    #                })
    #
    #     do.call(tagList, image_output_list)
    # })


    # Tab 4 : Download -----------------------------------------------------
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
        rcrea::measurements(country=country, city=city, poll=poll, date_from=date_from, date_to=date_to,
                            average_by=averaging, source=source, with_metadata = T) %>%
            dplyr::select(location_id, location_name, date, poll, unit, source, timezone, process_id) %>%
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
