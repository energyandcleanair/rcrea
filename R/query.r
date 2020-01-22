source('R/setup.r')


filter_sanity_daily <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% dplyr::filter(avg_day > 0) %>%
    dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(pollutant))  %>%
    dplyr::filter(avg_day < 1500 | pollutant==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}

filter_sanity_raw <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% dplyr::filter(value > 0) %>%
    dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(pollutant))  %>%
    dplyr::filter(value < 1500 | pollutant==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}

locations <- function(country=NULL, city=NULL, id=NULL, collect=TRUE, con=NULL){

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  id_ <- id

  # Connecting
  con = if(!is.null(con)) con else connection()
  if(collect){
    result <- rpostgis::pgGetGeom(con, name=c("public","locations"), geom = "geometry")
    result <- sf::st_as_sf(result)
  }else{
    result <- dplyr::tbl(con, "locations") # Old version without explicit geomoetry column
  }

  # Apply filters
  result <- switch(toString(length(country_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(country == country_), # Single country name
                   result %>% dplyr::filter(country %in% country_) # Vector of country names
  )

  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(city == city_), # Single city name
                   result %>% dplyr::filter(city %in% city_) # Vector of city names
  )

  result <- switch(toString(length(id_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(id == id_), # Single station id
                   result %>% dplyr::filter(id %in% id_) # Vector of station ids
  )

  # # Whether to collect the query i.e. actually run the query
  # if(collect){
  #   result <- result %>% collect()
  # }
  return(result)
}


#' Query Measurements
#'
#' @param country ISO2 code of the country of interest [optional]
#' @param city City as indicated in OpenAQ location
#' @param location_id Identifier of the air quality measurement station (referring to \code{id} in \code{\link{stations}})
#' @param poll Pollutant name (e.g. creadb::CO, "co", creadb::PM25, "pm25")
#' @param date_from Beginning date of queried measurements ('yyyy-mm-dd')
#' @param date_to End date of queried measurements ('yyyy-mm-dd')
#' @param average_by How to time-average results e.g. 'day', 'week', 'month' or 'year' ('hour' not available yet)
#' @param collect T/F Whether to collect results into local tibble (see \code{\link{dbplyr::collect}})
#' @param user_filter Additional filtering function for measurements applied before time aggregation
#' (although the most time-granular observation is already a daily-average calculated after scraping)
#'
#' @return a tibble (locally collected or not) of measurements matching search criteria.
#' @export
#'
#' @examples
#' meas_bj <- creadb::measurements(city=c('Beijing'), date_from='2018-01-01',average_by='month',with_metadata=T)
#'
measurements <- function(country=NULL,
                         city=NULL,
                         location_id=NULL,
                         poll=NULL,
                         date_from='2015-01-01',
                         date_to=NULL,
                         average_by='day',
                         collect=TRUE,
                         with_metadata=FALSE,
                         user_filter=NULL,
                         con=NULL) {

  # Variable names must be different to column names
  # country_ <- country
  # city_ <- city
  poll_ <- poll
  # location_id_ <- location_id

  # Connecting
  con = if(!is.null(con)) con else connection()

  # Find locations that match filters
  locs <- locations(country=country,
                    city=city,
                    id=location_id,
                    collect=F,
                    con=con)

  # Take measurements at these locations
  query_initial = switch(average_by,
                         "hour" = "measurements",
                         "measurements_daily")

  tryCatch({
    result <- dplyr::tbl(con, query_initial)
  },error = function(e) {
    print("Trying to reconnect")
    con <- connection(reconnect = TRUE)
    result <- dplyr::tbl(con, query_initial)
  })

  result <- result %>% dplyr::select(-c(city, country)) %>% # Avoid double columns (plus location is last truth)
                       dplyr::right_join(locs, by=c("location_id"="id"))

  poll_ <- tolower(poll_)
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(pollutant) == poll_), # Single value
                   result %>% dplyr::filter(tolower(pollutant) %in% poll_) # Vector
  )

  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date_trunc('day', date) >= date_from)
                   # We have an index on day in Postgres hence the date_trunc to make query faster
  )

  result <- switch(toString(length(date_to)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date <= date_to)
  )

  result <- switch(average_by,
                   "hour" = filter_sanity_raw(result),
                   filter_sanity_daily(result)
  )

  if(!is.null(user_filter)){
    result <- user_filter(result)
  }

  # R package will use 'poll' instead of 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Apply time aggregation
  # measurements_daily is already aggregated by day, so we only aggregate further if <> 'day'
  if(average_by == 'hour'){
    result <- result %>% group_by(city, geometry, location, location_id, date=DATE_TRUNC(average_by, date), poll) %>%
      dplyr::summarize(value=avg(value)) %>% dplyr::ungroup()
  }else if(average_by == 'day'){
    result <- result %>% dplyr::mutate(value=avg_day) %>%
      dplyr::select(-c(id)) # To mimic aggregation so that columns are more or less similar
  }else{
    result <- result %>% dplyr::group_by(city, geometry, location, location_id, date=DATE_TRUNC(average_by, date), poll) %>%
      dplyr::summarize(value=avg(avg_day)) %>% dplyr::ungroup()
  }

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
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
                   "1" = result %>% dplyr::filter(tolower(country) == tolower(country_)), # Single value
                   result %>% dplyr::filter(country %in% country_) # Vector
  )

  city_ = tolower(city_)
  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(city) == city_), # Single value
                   result %>% dplyr::filter(tolower(city) %in% city_) # Vector
  )

  poll_ <- tolower(poll_)
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(pollutant) == poll_), # Single value
                   result %>% dplyr::filter(tolower(pollutant) %in% poll_) # Vector
  )

  result <- switch(toString(length(location_id_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(location_id == location_id_), # Single value
                   result %>% dplyr::filter(location_id %in% location_id_) # Vector
  )


  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date >= date_from)
  )


  # Cast integer64 to integer. Prevents issues later on
  result <- result %>% dplyr::mutate_if(bit64::is.integer64, as.integer)

  # R package will use 'poll' instead of 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}


join_city_weather <- function(meas, measurements_averaged_by='day', collect=T, radius_km=20, con=NULL){

  # Connecting
  con = if(!is.null(con)) con else connection()

  # Check measurements have been fetched with metadata
  if(!"geometry" %in% colnames(meas)){
    stop("Station geometry missing. Please query measurements with the option 'with_metadata=T'")
  }

  # Find noaa stations close by
  noaa_stations <- meas %>% dplyr::distinct(location_id, geometry) %>%
                       dplyr::left_join(dplyr::tbl(con,"noaa_ids_stations"),
                               suffix=c("", "_noaa"),
                               sql_on= sprintf("(st_dwithin(\"LHS\".geometry::geography, \"RHS\".geometry::geography, %f))",radius_km*1000.0)
                               ) %>%
                       dplyr::rename(noaa_station_id=id) %>%
                       dplyr::distinct(noaa_station_id) %>%
                       dplyr::filter(!is.na(noaa_station_id))

  dates <- meas %>% dplyr::distinct(date, .keep_all = FALSE)

  # Get averaged noaa observations accordingly
  obs_averaged  <- dplyr::tbl(con,"noaa_ids_observations") %>%
    dplyr::right_join(noaa_stations, by=c("station_id"="noaa_station_id")) %>%
    dplyr::mutate(date=DATE_TRUNC(measurements_averaged_by, date)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(temp_c=mean(temp_c, na.rm=T),
                     slp_hp=mean(slp_hp, na.rm=T),
                     wind_deg=mean(wind_deg, na.rm=T),
                     wind_ms=mean(wind_ms, na.rm=T),
                     sky_code=max(sky_code, na.rm=T),
                     prec_1h_mm=max(prec_1h_mm, na.rm=T),
                     prec_6h_mm=max(prec_6h_mm, na.rm=T),
                     rh_percent=mean(rh_percent, na.rm=T),
                     ) %>%
    dplyr::ungroup()

  # Joining noaa stations
  result <- meas %>% dplyr::group_by(date, poll) %>%
    dplyr::summarize(value=mean(value, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(obs_averaged,
                     suffix=c("", "_noaa"),
                     by=c("date")
    )

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}
