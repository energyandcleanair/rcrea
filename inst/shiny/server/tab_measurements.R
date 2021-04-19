region_choices <- reactive({
  # req(input$source)
  # req(input$regionLevel)
  req(input$country)

  filtered_locations <- locations %>%
    dplyr::filter(
      # source==input$source,
      level=="city")
  # level==input$regionLevel)
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
  # source <- isolate(input$source)
  country <- isolate(input$country)
  region <- isolate(input$region)
  region_level <- "city" #isolate(input$regionLevel)
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
                      average_by=averaging, aggregate_level=aggregate_level,
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
  # req(input$source)
  filtered_locations <- locations #%>% dplyr::filter(source==input$source)
  countries <- unique(filtered_locations$country)
  countries <- countries[!is.na(countries)]
  names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name', custom_match = list(XK='Kosovo')))
  countries <- countries[!is.na(names(countries))]
  selectInput("country", "Country:", multiple=T, choices = countries)
})

output$selectInputSources <- renderUI({
  req(meas())
  if(nrow(meas())==0){
    choices <- c()
  }else{
    choices <- unique(meas()$source)
  }
  selected = ifelse(length(choices)>0, choices[1], NULL)


  selectInput("source",
              "Source:",
              choices=choices,
              multiple=F,
              selected=selected)
})

output$selectInputRegion <- renderUI({
  req(input$country)
  # req(input$regionLevel)
  pickerInput("region",
              "City:",#input$regionLevel,
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

output$meas_plot_message <- renderText({
  req(meas())
  if(nrow(meas())==0){
    return(HTML("<div style='margin-top: 40px;'>No measurement found</div>"))
  }else{
    return(NULL)
  }
})

output$meas_plot <- renderPlotly({

  # To trigger refresh
  input$meas_refresh

  if(nrow(meas())==0){
    return(NULL)
  }

  poll <- isolate(input$poll)
  averaging <- isolate(input$averaging)
  region <- isolate(input$region)
  region_choices_ <- isolate(region_choices())

  # Plotting parameteres
  source <- input$source
  # months <- input$months
  running_width <- input$running_width
  scales <- input$scale
  targets <- input$target
  plot_type <- input$plot_type
  process_ <- input$process

  req(poll, averaging, plot_type, region, source)

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



  meas_plot_data <- meas() %>% dplyr::filter(source==!!source,
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

  #meas_plot
  ggplotly(meas_plot) %>%
    layout(hovermode = "x",
           font=list(family = "Montserrat"))
})

observe({
  req(meas())
  req(input$source)
  selected_old <- isolate(input$process)
  if(nrow(meas())==0){
    process_ids = c()
  }else{
    process_ids <- meas() %>%
      dplyr::filter(source==input$source) %>%
      dplyr::distinct(process_id) %>%
      dplyr::left_join(processes, by=c("process_id"="id")) %>%
      dplyr::arrange(!is.na(deweather), !is.na(weighting)) %>%
      dplyr::pull(process_id)
  }
  #Select non-deweather / non-population-weighted by default: putting them first
  choices = process_ids
  selected = ifelse(!is.null(selected_old) && selected_old %in% choices,
                    selected_old,
                    ifelse(length(process_ids)>0, process_ids[1], NULL))

  updateSelectInput(session,
                    "process",
                    choices = choices,
                    selected = selected)

})


output$processes_table_lite <- renderUI({
  # tibble::tibble(
  #     name=c("sdf","sdf22"),
  #     explanation=c("sdfkljsdlkfj","asdkq")
  # )
  HTML(paste0("asd<div>wer<table style=\"width:100%\">",
              "<tr>",
              "<th>Process Id</th>",
              "<th>Definition</th>",
              "<th>Unit</th>",
              "</tr>",
              "<tr>",
              "<td>city_day_*</td>",
              "<td>Daily <b>observed</b> level</td>",
              "<td>µg/m3 or ppm</td>",
              "</tr>",
              "<tr>",
              "<td>anomaly_vs_counterfactual*</th>",
              "<td><b>Deweathered</b> indication of how observed values differs from what would be expected in these weather conditions,",
              "expressed as (observed-predicted)/predicted</td>",
              "<td>%</td>",
              "</tr>",
              "<tr>",
              "<td>anomaly_offsetted*</th>",
              "<td><b>Deweathered</b> indication of how observed values differs from what would be expected in these weather conditions,",
              "brought back to an absolute scale (observed-predicted) + average</td>",
              "<td>µg/m3 or ppm</td>",
              "</tr>",
              "</table></div>"))
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
})
