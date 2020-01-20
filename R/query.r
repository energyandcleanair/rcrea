source('R/setup.r')


filter_sanity <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% filter(avg_day > 0) %>%
            filter(!is.na(location_id)) %>%
            filter(!is.na(date)) %>%
            filter(!is.na(pollutant))  %>%
            filter(avg_day < 2000 | pollutant==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}

locations <- function(country=NULL, city=NULL, collect=TRUE){

  # Variable names must be different to column names
  country_ <- country
  city_ <- city

  # Connecting
  con = connection()
  result <- rpostgis::pgGetGeom(con, name=c("public","locations"), geom = "geometry")
  result <- sf::st_as_sf(result)
  # result <- dplyr::tbl(con, "locations") # Old version without explicit geomoetry column

  # Apply filters
  result <- switch(toString(length(country_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(country == country_), # Single country name
                   result %>% filter(country %in% country_) # Vector of country names
  )

  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(city == city_), # Single city name
                   result %>% filter(city %in% city_) # Vector of city names
  )

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% collect()
  }

  return(result)

}


measurements <- function(country=NULL,
                         city=NULL,
                         location_id=NULL,
                         poll=NULL,
                         date_from='2018-01-01',
                         date_to=NULL,
                         average_by='day',
                         collect=TRUE,
                         with_metadata=FALSE,
                         user_filter=NULL) {

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  poll_ <- poll
  location_id_ <- location_id

  # Connecting
  con = connection()

  query_initial = "measurements_daily"

  tryCatch({
    result <- dplyr::tbl(con, query_initial)
  },error = function(e) {
    print("Trying to reconnect")
    con <- connection(reconnect = TRUE)
    result <- dplyr::tbl(con, query_initial)
  })

  # Apply filters
  result <- switch(toString(length(country_)),
        "0" = result, # NULL
        "1" = result %>% filter(tolower(country) == tolower(country_)), # Single value
        result %>% filter(country %in% country_) # Vector
  )

  city_ = tolower(city_)
  result <- switch(toString(length(city_)),
         "0" = result, # NULL
         "1" = result %>% filter(tolower(city) == city_), # Single value
         result %>% filter(tolower(city) %in% city_) # Vector
  )

  poll_ <- tolower(poll_)
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(pollutant) == poll_), # Single value
                   result %>% filter(tolower(pollutant) %in% poll_) # Vector
  )

  result <- switch(toString(length(location_id_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(location_id == location_id_), # Single value
                   result %>% filter(location_id %in% location_id_) # Vector
  )


  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% filter(date >= date_from)
  )

  result <- switch(toString(length(date_to)),
                   "0" = result, # NULL
                   "1" = result %>% filter(date <= date_to)
  )


  result <- filter_sanity(result)

  if(!is.null(user_filter)){
    result <- user_filter(result)
  }

  # R package will use 'poll' instead of 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Apply time aggregation
  # measurements_daily is already aggregated by day, so we only aggregate further if <> 'day'
  if(average_by != 'day'){
    result <- result %>% group_by(city, location, location_id, date=DATE_TRUNC(average_by, date), poll) %>%
                dplyr::summarize(value=avg(avg_day)) %>% ungroup()
  }else{
    result <- result %>% dplyr::mutate(value=avg_day)
  }

  # Add metadata
  if(with_metadata){
    result <- result %>% dplyr::left_join(dplyr::tbl(con, "locations"), by=c("location_id"="id"))
  }


  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% collect()
  }

  return(result)
}


exceedances <- function(country=NULL,
                         city=NULL,
                         location_id=NULL,
                         poll=NULL,
                         date_from='2018-01-01',
                         collect=TRUE) {

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  poll_ <- poll
  location_id_ <- location_id

  # Connecting
  con = connection()

  query_initial = "standard_exceedances"


  tryCatch({
    result <- dplyr::tbl(con, query_initial)
  },error = function(e) {
    print("Trying to reconnect")
    con <- connection(reconnect = TRUE)
    result <- dplyr::tbl(con, query_initial)
  })

  # Apply filters
  result <- switch(toString(length(country_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(country) == tolower(country_)), # Single value
                   result %>% filter(country %in% country_) # Vector
  )

  city_ = tolower(city_)
  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(city) == city_), # Single value
                   result %>% filter(tolower(city) %in% city_) # Vector
  )

  poll_ <- tolower(poll_)
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(pollutant) == poll_), # Single value
                   result %>% filter(tolower(pollutant) %in% poll_) # Vector
  )

  result <- switch(toString(length(location_id_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(location_id == location_id_), # Single value
                   result %>% filter(location_id %in% location_id_) # Vector
  )


  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% filter(date >= date_from)
  )


  # Cast integer64 to integer. Prevents issues later on
  result <- result %>% dplyr::mutate_if(bit64::is.integer64, as.integer)

  # R package will use 'poll' instead of 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% collect()
  }

  return(result)
}


join_noaa_observations <- function(meas, measurements_averaged_by='day', collect=T, radius_km=20){

  # Joining noaa stations
  result <- meas %>% dplyr::left_join(dplyr::tbl(connection(),"noaa_ids_stations"),
                               suffix=c("", "_noaa_ids_stations"),
                               sql_on= sprintf("(st_dwithin(\"LHS\".geometry::geography, \"RHS\".geometry::geography, %f))",radius_km*1000.0)
                               )

  # Average noaa observatinos accordingly
  obs_averaged  <- dplyr::tbl(connection(),"noaa_ids_observations") %>%
    dplyr::mutate(date=DATE_TRUNC(measurements_averaged_by, date)) %>%
    dplyr::group_by(station_id, date) %>%
    dplyr::summarize(temp_c=mean(temp_c, na.rm=T),
                     slp_hp=mean(slp_hp, na.rm=T),
                     wind_deg=mean(wind_deg, na.rm=T),
                     wind_ms=mean(wind_ms, na.rm=T),
                     sky_code=max(sky_code, na.rm=T),
                     prec_1h_mm=max(prec_1h_mm, na.rm=T),
                     prec_6h_mm=max(prec_6h_mm, na.rm=T)
                     ) %>%
    ungroup()

  # Joining noaa observationos
  result <- result %>% dplyr::left_join(obs_averaged,
                                        sql_on= "(\"LHS\".id_noaa_ids_stations=\"RHS\".station_id) AND (\"LHS\".date=date_trunc('day',\"RHS\".date))"
                                        )

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% collect()
  }
}
