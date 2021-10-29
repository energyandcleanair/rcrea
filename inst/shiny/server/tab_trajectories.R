
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

  # Trajectories (no fire source in it)
  files.trajs <- files %>%
    dplyr::filter(stringr::str_detect(name, ".trajs..*.RDS$")) %>%
    mutate(location_id=gsub(".trajs.*.RDS","",basename(name))) %>%
    mutate(details=stringr::str_match(basename(name),
                                      sprintf("%s.trajs.(.*?).RDS",location_id))[,2]) %>%
    tidyr::separate(details, c("buffer","duration","pbl")) %>%
    rename(gcs_name_trajs=name) %>%
    filter(pbl %in% c("10m","50m")) %>%
    select(location_id, buffer, duration, pbl, gcs_name_trajs)

  # Weather & measurements
  files.meas_weather <- files %>%
    dplyr::filter(stringr::str_detect(name, ".(weather|meas)..*.RDS$")) %>%
    mutate(location_id=gsub(".(weather|meas).*.RDS","",basename(name))) %>%
    mutate(details=stringr::str_match(basename(name),
                                      sprintf("%s.(weather|meas).(.*?).RDS",location_id))[,3]) %>%
    tidyr::separate(details, c("buffer","duration","pbl","firesource")) %>%
    rename(gcs_name=name) %>%
    mutate(meas_or_weather=ifelse(stringr::str_detect(gcs_name, ".(weather)..*.RDS$"), "weather","meas")) %>%
    select(location_id, buffer, duration, pbl, gcs_name, firesource, meas_or_weather) %>%
    tidyr::pivot_wider(names_from=meas_or_weather, values_from=gcs_name, names_prefix="gcs_name_") %>%
    filter(pbl %in% c("10m","50m"))

  full_join(files.trajs, files.meas_weather)
})

trajs_locations <- reactive({
  req(trajs_files())
  rcrea::locations(id=unique(trajs_files()$location_id),
                   level=c("station","city"),
                   with_metadata=F,
                   with_geometry=T) %>%
    dplyr::left_join(trajs_files(),
                     by=c("id"="location_id")) %>%
    dplyr::distinct(id, name, country, geometry)
})

trajs_location_id <- reactive({
  req(input$trajs_city)
  req(input$trajs_country)

  trajs_locations() %>%
    dplyr::filter(country==input$trajs_country,
                  name==input$trajs_city) %>%
    dplyr::pull(id) %>%
    unique()
})

trajs_location_geometry <- reactive({
  req(input$trajs_city)
  req(input$trajs_country)

  trajs_locations() %>%
    dplyr::filter(country==input$trajs_country,
                  name==input$trajs_city) %>%
    dplyr::distinct(geometry) %>%
    dplyr::pull(geometry)
})

trajs_file <- reactive({
  req(trajs_location_id())
  req(input$trajs_buffer)
  req(input$trajs_duration)
  req(input$trajs_firesource)

  trajs_files() %>%
    filter(location_id==trajs_location_id(),
           buffer==input$trajs_buffer,
           duration==input$trajs_duration,
           (input$trajs_firesource=="NA" & is.na(firesource)) | (firesource==input$trajs_firesource))
})

trajs <- reactive({
  req(trajs_file())
  gcs_url <- paste0(trajs.bucket_base_url, trajs_file()$gcs_name_trajs)
  read_gcs_url(gcs_url)
})

trajs_dates <- reactive({
  req(trajs())
  trajs() %>%
    dplyr::pull(date) %>%
    unique() %>%
    sort(decreasing=T)
})

trajs_durations <- reactive({
  req(trajs_location_id())
  req(trajs_files())

  trajs_files() %>%
    filter(location_id==trajs_location_id()) %>%
    pull(duration) %>%
    unique()
})

trajs_buffers <- reactive({
  req(trajs_location_id())
  req(trajs_files())

  trajs_files() %>%
    filter(location_id==trajs_location_id()) %>%
    pull(buffer) %>%
    unique()
})


trajs_firesources <- reactive({
  req(trajs_location_id())
  req(trajs_files())

  trajs_files() %>%
    filter(location_id==trajs_location_id()) %>%
    pull(firesource) %>%
    unique() %>%
    tidyr::replace_na("NA")
})

trajs_weather <- reactive({
  req(trajs_file())
  gcs_url <- paste0(trajs.bucket_base_url, trajs_file()$gcs_name_weather)
  read_gcs_url(gcs_url)
})

trajs_meas_all <- reactive({
  req(trajs_file())
  gcs_url <- paste0(trajs.bucket_base_url, trajs_file()$gcs_name_meas)
  read_gcs_url(gcs_url)
})

trajs_polls <- reactive({
  req(trajs_meas_all())

  polls <- trajs_meas_all() %>%
    pull(poll) %>%
    unique()

  names(polls) <- rcrea::poll_str(polls)
  return(polls)
})

trajs_meas_date <- reactive({

  req(trajs_meas_all())
  req(trajs_date())

  trajs_meas_all() %>%
    dplyr::filter(date==trajs_date())

})

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
  trajs_poll <- input$trajs_poll
  req(input$trajs_running_width, trajs_poll)

  m <- trajs_meas_all() %>%
    filter(poll==!!trajs_poll)

  if(nrow(m)==0){
    return(NULL)
  }

  poll_name <- rcrea::poll_str(trajs_poll)
  unit <- unique(m$unit)
  hovertemplate <- paste('%{y:.0f}',unit)

  m <- m %>%
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
      paper_bgcolor = "rgba(0, 0, 0, 0)"
      # fig_bgcolor   = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::add_annotations(
      text = sprintf("%s [%s]", poll_name, unit),
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    ) %>%
    plotly::event_register('plotly_click')
})

trajs_plot_fire <- reactive({

  req(trajs_weather())
  req(input$trajs_running_width)
  req(input$trajs_firesource)

  if(input$trajs_firesource=="gfas"){
    fire_value="pm25_emission"
    fire_name="PM25 emission from fires"
  }else{
    fire_value="fire_count"
    fire_name="Fire count"
  }
  f <- trajs_weather() %>%
    dplyr::select_at(c("date", "value"=fire_value))

  f.rolled <- rcrea::utils.running_average(f, input$trajs_running_width)

  # selected <- which(trajs_meas_obs()$date==trajs_date())
  f.rolled %>%
    plot_ly(
      x = ~date,
      y = ~value
      # selectedpoints=as.list(selected),
    ) %>%
    plotly::add_lines(name=fire_name) %>%
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
      text = fire_name,
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})

trajs_plot_precip <- reactive({

  req(trajs_weather())
  req(input$trajs_running_width)

  f <- trajs_weather() %>%
    dplyr::select(date, value=precip) %>%
    mutate(value=value/10) #NOAA ISD has a 10 scaling factor

  f.rolled <- rcrea::utils.running_average(f, input$trajs_running_width)

  f.rolled %>%
    plot_ly(
      x = ~date,
      y = ~value
    ) %>%
    plotly::add_lines(name="Precipitation") %>%
    plotly::layout(
      showlegend = F,
      hovermode  = 'x unified',
      yaxis = list(
        title="",
        rangemode = 'tozero'
      ),
      xaxis = list(title="")) %>%
    plotly::add_annotations(
      text = "Precipitation (mm/day)",
      x = -0.05,
      y = 1.19,
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
  req(input$trajs_running_width)
  trajs_poll <- input$trajs_poll
  req(trajs_poll)

  m <- trajs_meas_all() %>%
    filter(poll==!!trajs_poll)

  if(nrow(m)==0){
    return(NULL)
  }

  poll_name <- rcrea::poll_str(trajs_poll)
  unit <- unique(m$unit)

  hovertemplate <- paste('%{y:.0f}',unit)
  m <- m %>%
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
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::add_annotations(
      text = sprintf("Fire contribution to %s [%s]",poll_name, unit),
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})


# Output Elements --------------------------------------
output$selectInputTrajsRunning <- renderUI({
  req(trajs_locations()) # Not required, but added so that all ui elements appear together
  sliderInput("trajs_running_width", "Rolling average (day)", min=1, max=30, value=7, step=1, sep="")
})

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

output$selectInputTrajsDuration <- renderUI({
  req(trajs_durations())
  pickerInput("trajs_duration","Duration", choices=trajs_durations(), options = list(`actions-box` = TRUE), multiple = F)
})

output$selectInputTrajsBuffer <- renderUI({
  req(trajs_buffers())
  pickerInput("trajs_buffer","Buffer", choices=trajs_buffers(), options = list(`actions-box` = TRUE), multiple = F)
})

output$selectInputTrajsFireSource <- renderUI({
  req(trajs_firesources())
  pickerInput("trajs_firesource","Fire source", choices=trajs_firesources(), options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputTrajsPoll <- renderUI({
  req(trajs_polls())
  selected <- input$trajs_poll
  if(is.null(selected) || !selected %in% trajs_polls()){
    selected <- trajs_polls()[1]
  }
  pickerInput("trajs_poll","Pollutant", choices=trajs_polls(),
              selected=selected,
              options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputTrajsPlots <- renderUI({
  plots <- list(
    "Pollutant"="pollutant",
    "Fire contribution"="fire_contrib",
    "Fire count"="fire_count",
    "Precipitation"="precip")

  pickerInput("trajs_plots","Charts", choices=plots, selected=c("pollutant","fire_contrib","fire_count"),
              options = list(`actions-box` = TRUE, `selected-text-format`= "count"), multiple = T)
})

createInputTrajsDate <- function(value=NULL){
  dates <- trajs_dates()
  if(is.null(value)){
    value <- max(dates)
  }

  # pickerInput("trajs_date","Date", choices=dates, options = list(`actions-box` = TRUE), multiple = F)
  # dateInput("trajs_date","Date", min=min(dates), max=max(dates))
  sliderInput("trajs_date",
              NULL,
              min = as.Date(min(dates),"%Y-%m-%d"),
              max = as.Date(max(dates),"%Y-%m-%d"),
              value=as.Date(value),
              timeFormat="%Y-%m-%d",
              ticks=T,
              width="100%")
}


output$selectInputTrajsDates <- renderUI({
  req(trajs_dates())
  createInputTrajsDate()
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
    dplyr::filter(id==trajs_location_id()) %>%
    distinct(name)
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
  req(input$trajs_plots)

  plots <- list(
    "pollutant"=trajs_plot_poll(),
    "fire_contrib"=trajs_plot_firecontribution(),
    "fire_count"=trajs_plot_fire(),
    "precip"=trajs_plot_precip()
  )[input$trajs_plots]

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


add_sentinel_layers <- function(map, date){
  for(l in names(sentinel_layers)){
    map <- map %>% addWMSTiles(
      sentinel_url,
      layers = sentinel_layers[[l]],
      layerId = l,
      group = l,
      options = WMSTileOptions(
        tileSize= 512,
        # attribution= '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
        # urlProcessingApi="https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
        # maxcc=20,
        # minZoom:6,
        # maxZoom:16,
        preset=sentinel_layers[[l]],
        # layers:"NO2",
        time=date,
        format = "image/png",
        transparent = F)
    )
  }
  return(map)
}

output$maptrajs <- renderLeaflet({
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE,
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
    addLayersControl(
      baseGroups = c("Terrain", "Satellite", "OpenStreetMap", "Light"),
      overlayGroups = c("Trajectories", "Active fires",
                        names(trajs_gibs_layers),
                        names(sentinel_layers)),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c(names(trajs_gibs_layers),
                 names(sentinel_layers)))


  for(l in names(trajs_gibs_layers)){
    map <- map %>%leaflet.extras2::addGIBS(
      layers=trajs_gibs_layers[[l]],
      group=l,
      dates=lubridate::today(),
      transparent = T,
      opacity = 0.7
    )
  }


  # Add S5P - NO2
  map <- add_sentinel_layers(map, date=lubridate::today())


  # sentinelHub = L.tileLayer.wms(baseUrl, {
  #   tileSize: 512,
  #   attribution: '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
  #   urlProcessingApi:"https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
  #   maxcc:20,
  #   minZoom:6,
  #   maxZoom:16,
  #   preset:"NO2",
  #   layers:"NO2",
  #   time:"2020-09-01/2021-03-17",
  #
  # });

  return(map)
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
      )) %>%
      leaflet.extras2::setDate(layers=stack(trajs_gibs_layers)$values,
                               dates=as.Date(trajs_date())) %>%


    # removeTiles(stack(trajs_gibs_layers)$values) %>%
    removeTiles(names(sentinel_layers)) %>%
    add_sentinel_layers(date=date_str)
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




# Click date
clickedDate <- reactiveVal()

observe({
  req(trajs_plot_poll())
  d <- unlist(event_data(event = "plotly_click",
                         priority = "event"))

  if(is.null(d)){return(NULL)}
  name <- intersect(c("x","x1"), names(d))
  clickedDate(d[[name]])
  output$selectInputTrajsDates = renderUI(createInputTrajsDate(value=d[[name]]))

})

