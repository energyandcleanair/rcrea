source('R/setup.r')

filter_sanity_daily <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% dplyr::filter(avg_day > 0) %>%
    dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(avg_day < 1500 | poll==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}


filter_sanity_raw <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% dplyr::filter(value > 0) %>%
    dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(value < 1500 | poll==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}


locations <- function(country=NULL, city=NULL, id=NULL, collect=TRUE, with_location=TRUE, con=NULL){

  # Variable names must be different to column names
  country_ <- tolower(country)
  city_ <- tolower(city)
  id_ <- id


  # Connecting
  con = if(!is.null(con)) con else connection()
  result <- tbl_safe(con, "locations") # Old version without explicit geomoetry column

    # Apply filters
  result <- switch(toString(length(country_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(country) == country_), # Single country name
                   result %>% dplyr::filter(tolower(country) %in% country_) # Vector of country names
  )

  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(city) == city_), # Single city name
                   result %>% dplyr::filter(tolower(city) %in% city_) # Vector of city names
  )

  result <- switch(toString(length(id_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(id == id_), # Single station id
                   result %>% dplyr::filter(id %in% id_) # Vector of station ids
  )

  # Keeping only interesting columns
  cols <- if(with_location) c("id", "name", "city", "country", "geometry") else c("id", "name", "city", "country")

  result <- result %>% dplyr::select_at(cols)


  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
    if(with_location){
      result <- result %>% dplyr::mutate(geometry=sf::sf_as_sfc.pq_geometry(geometry))
    }
  }

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
#' @param average_by How to time-average results e.g. 'hour', day', 'week', 'month' or 'year'
#' @param collect T/F Whether to collect results into local tibble (see \code{\link{dbplyr::collect}})
#' @param with_metadata T/F Whether to add additional information columnes (e.g. city, country, location name, geometry). If True, query takes significantly more time
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
  poll_ <- tolower(poll)

  # Find locations that match filters
  locs <- locations(country=country,
                    city=city,
                    id=location_id,
                    collect=F)

  # Connecting
  con = if(!is.null(con)) con else connection()

  # Take measurements at these locations
  query_initial = switch(average_by,
                         "hour" = "measurements",
                         "measurements_daily")

  result <- tbl_safe(con, query_initial)

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Joining location information
  result <- result %>% dplyr::select(-c(city, country)) %>% # Avoid double columns (plus location is last truth)
                       dplyr::right_join(locs, by=c("location_id"="id"))

  # Filtering based on user request
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(poll) == poll_), # Single value
                   result %>% dplyr::filter(tolower(poll) %in% poll_) # Vector
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



  if(with_metadata){
    group_by_meta_cols <- c("location", "name", "city", "country","geometry")
  }else{
    group_by_meta_cols <- c("city", "location") # keeping city regardless because required in plots
  }

  # Apply time aggregation
  # if hour, we fetched the raw measurements table
  # if not hour (e.g. day, week, month, year), we fetched from measurements_daily which is already aggregated by day,
  # so we only aggregate further if <> 'day'
  if(average_by == 'hour'){
    result <- result %>% dplyr::mutate(date=DATE_TRUNC(average_by, date)) %>%
      dplyr::group_by_at(c(group_by_meta_cols, "location_id", "date", "poll")) %>%
      dplyr::summarize(value=avg(value)) %>% dplyr::ungroup()
  }else if(average_by == 'day'){
    result <- result %>% dplyr::mutate(value=avg_day) %>%
      dplyr::select(-c(id)) %>% select_at(c(group_by_meta_cols, "location_id", "date", "poll", "value")) # To mimic aggregation so that columns are more or less similar
  }else{
    result <- result %>% dplyr::mutate(date=DATE_TRUNC(average_by, date)) %>%
      dplyr::group_by_at(c(group_by_meta_cols, "location_id", "date", "poll")) %>%
      dplyr::summarize(value=avg(avg_day)) %>% dplyr::ungroup()
  }

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}

standards <- function(collect=TRUE){
  # Connecting
  con = connection()
  standards  <- tbl_safe(con,"standards")
  if(collect){
    standards <- standards %>% collect()
  }
  return(standards)
}

exceedances <- function(country=NULL,
                         city=NULL,
                         location_id=NULL,
                         poll=NULL,
                         standard_org=NULL,
                         date_from='2018-01-01',
                         date_to=NULL,
                         collect=TRUE) {

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  poll_ <- poll
  location_id_ <- location_id
  standard_org_ <- standard_org

  # Connecting
  con = connection()
  result  <- tbl_safe(con,"standard_exceedances")

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

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
                   "1" = result %>% dplyr::filter(tolower(poll) == poll_), # Single value
                   result %>% dplyr::filter(tolower(poll) %in% poll_) # Vector
  )

  result <- switch(toString(length(location_id_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(location_id == location_id_), # Single value
                   result %>% dplyr::filter(location_id %in% location_id_) # Vector
  )

  result <- switch(toString(length(standard_org_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(standard_org == standard_org_), # Single value
                   result %>% dplyr::filter(standard_org %in% standard_org_) # Vector
  )

  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date >= date_from)
  )

  result <- switch(toString(length(date_to)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date <= date_to)
  )

  # Cast integer64 to integer. Prevents issues later on
  result <- result %>% dplyr::mutate_if(bit64::is.integer64, as.integer)

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}


exceedance_status <- function(country=NULL,
                              city=NULL,
                              location_id=NULL,
                              poll=NULL,
                              standard_org=NULL,
                              year=lubridate::year(now()),
                              collect=TRUE,
                              with_location=F) {

  if(with_location && !collect){
    stop("Adding location only works with collect")
  }

  # Get exceedances
  excs <- exceedances(country=country,
                   city=city,
                   location_id=location_id,
                   poll=poll,
                   standard_org=standard_org,
                   date_from=lubridate::ymd(year*10000+101),
                   date_to = lubridate::ymd(year*10000+1231),
                   collect=F)

  # Get maximum status
  excs <- excs %>% dplyr::mutate(status= ifelse(exceedance_allowed_per_year==0,
                                     pmin(exceedance_this_year*100, 100),
                                     pmin(exceedance_this_year/exceedance_allowed_per_year*100, 100)
  )) %>% group_by(standard_id, standard_org, aggregation_period, country, city, poll) %>%
    summarise(status=max(status, na.rm=T))

  if(collect){
    excs <- excs %>% collect()
    if(with_location){
      # Getting a location per city
      city_locations <- locations(country=country,city=city, collect=T, with_location=T) %>%
        dplyr::select(country,city,geometry) %>% dplyr::distinct(country, city, .keep_all = TRUE)

      excs <- excs %>% right_join(city_locations)
    }
  }
  return(excs)
}

#' Attach weather information to air quality measurements
#'
#' @param meas dataframe / tibble of measurements
#' @param measurements_averaged_by The freequency at which measurements have been averaged to e.g. hour, day, week, month, year
#' (same aggregation needs to be applied to weather observations)
#' @param aggregate_per_city T/F whether to aggregate information at the city level. Staying at the location level may result in very large dataframe and double counting
#' since the same weather station might be joined to several air quality stations.
#' @param collect T/F Whether to collect results into local tibble (see \code{\link{dbplyr::collect}})
#' @param radius_km Maximum distance to look for weather stations close to air quality ones
#' @param con Driver connection to Postgres. If null, will try the global one
#'
#' @return a tibble (locally collected or not) of measurements left joined to weather data
#'
#' @examples
#' meas_w_weather <- creadb::join_weather_data(meas)
#'
join_weather_data <- function(meas, measurements_averaged_by='day', aggregate_per_city=TRUE, collect=TRUE, radius_km=20, con=NULL){

  # Connecting
  con = if(!is.null(con)) con else connection()

  # Check measurements have been fetched with metadata
  if(!"geometry" %in% colnames(meas)){
    stop("Station geometry missing. Use with_metadata=T when querying measurements.")
  }

  # Find NOAA stations close by
  distinct_cols <- if (aggregate_per_city) c('noaa_station_id') else c('noaa_station_id', 'location_id')
  noaa_stations <- meas %>% dplyr::distinct(location_id, geometry) %>%
                       dplyr::left_join(tbl_safe(con,"noaa_ids_stations"),
                               suffix=c("", "_noaa"),
                               sql_on= sprintf("(st_dwithin(\"LHS\".geometry::geography, \"RHS\".geometry::geography, %f))",radius_km*1000.0)
                               ) %>%
                       dplyr::rename(noaa_station_id=id) %>%
                       dplyr::distinct_at(distinct_cols) %>%
                       dplyr::filter(!is.na(noaa_station_id))

  dates <- meas %>% dplyr::distinct(date, .keep_all = FALSE)


  # Get averaged NOAA observations accordingly
  obs_group_by_cols <- if (aggregate_per_city) c('date') else c('date', 'location_id', 'noaa_station_id')
  obs_averaged  <- tbl_safe(con,"noaa_ids_observations") %>%
    dplyr::right_join(noaa_stations, by=c("station_id"="noaa_station_id")) %>%
    dplyr::mutate(date=DATE_TRUNC(measurements_averaged_by, date)) %>%
    dplyr::rename(noaa_station_id=station_id) %>%
    dplyr::group_by_at(obs_group_by_cols) %>%
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
  group_by_cols <- if (aggregate_per_city) c('date','poll') else c('date', 'poll', 'city', 'location_id')
  join_by_cols <- if (aggregate_per_city) c('date') else c('date', 'location_id')
  result <- meas %>% dplyr::group_by_at(group_by_cols) %>%
    dplyr::summarize(value=mean(value, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(obs_averaged,
                     suffix=c("", "_noaa"),
                     by=join_by_cols
    )

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}
