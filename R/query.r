source('R/setup.r')

filter_sanity_daily <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>%
    # dplyr::filter(avg_day > 0) %>%
    dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(avg_day < 1500 | poll==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}


filter_sanity_raw <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>%
    dplyr::filter(value >= 0) %>%
    #dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(value < 1500 | poll==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}


locations <- function(country=NULL, city=NULL, id=NULL,
                      collect=TRUE,
                      with_geometry=TRUE,
                      with_tz=FALSE,
                      con=NULL){

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
  cols <- c("id", "name", "city", "country")
  cols <- if(with_geometry)  c(cols, "geometry") else cols
  cols <- if(with_tz) c(cols, 'timezone') else cols

  result <- result %>% dplyr::select_at(cols)


  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
    if(with_geometry){
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
#' @param source Source of the data. e.g. cpcb, openaq, openaq-archive
#' @param average_by How to time-average results e.g. 'hour', day', 'week', 'month' or 'year'
#' @param collect T/F Whether to collect results into local tibble (see \code{\link{dbplyr::collect}})
#' @param with_metadata T/F Whether to add additional information columnes (e.g. city, country, location name, geometry). If True, query takes significantly more time
#' @param user_filter Additional filtering function for measurements applied before time aggregation
#' @param aggregate_at_city_level T/F Whether or not to keep measurements at the station level (otherwise, aggregate at city level)
#' #' @param add_noaa_station_ids T/F Whether or not to add an array of noaa_station_ids located within @param noaa_station_radius_km
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
                         source=NULL,
                         average_by='day',
                         collect=TRUE,
                         with_metadata=FALSE,
                         user_filter=NULL,
                         aggregate_at_city_level=TRUE,
                         add_noaa_station_ids=FALSE,
                         noaa_station_radius_km=20,
                         con=NULL) {

  # Accept both NA and NULL
  location_id <- if(!is.null(location_id) && is.na(location_id)) NULL else location_id
  city <- if(!is.null(city) && length(city)==1 && is.na(city)) NULL else city
  country <- if(!is.null(country) && is.na(country)) NULL else country


  # If location_id specified, we have to keep it
  aggregate_at_city_level <- aggregate_at_city_level & is.null(location_id)

  # Note: variable names must be different to column names for dbplyr filters to work
  poll_ <- tolower(poll)
  source_ <- tolower(source)
  city_ <- tolower(city)

  # Connecting
  con = if(!is.null(con)) con else connection()

  # ----------------------
  # Set actions to do
  #-----------------------
  # Explanation: we don't need to perform same averaging and aggregating operations based on user parameters
  # Plus the database contains three measurement tables with different averaging/aggregations pre-computed:
  #     - 'measurements': all raw measurements
  #     - 'measurements_daily': daily average per location (with rolling values for standards)
  #     - 'measurements_hourly_city': hourly value per city (not per location). Mainly used for poll/weather analysis
  # if no time aggregation, we fetched the raw measurements table
  # if hour aggregation, we either fetch raw table or city_hour aggregated one (if need info at city level)
  # if not hour (e.g. day, week, month, year), we fetched from measurements_daily which is already aggregated by day
  table_name <- NULL
  value_col_name <- NULL
  need_time_averaging <- NULL
  need_grouping_before_time_averaging <- NULL
  need_grouping <- NULL
  filter_fn <- NULL
  locs_meas_join_by <- NULL

  if(is.null(average_by)){
    table_name <= 'measurements'
    need_time_averaging <- FALSE
    value_col_name <- 'value'
    filter_fn <- filter_sanity_raw
    need_grouping_before_time_averaging <- FALSE
    need_grouping <- aggregate_at_city_level
    locs_meas_join_by <- c("location_id"="id")
  }else if(average_by=='hour'){
    table_name <- if(aggregate_at_city_level) 'measurements_city_hourly' else 'measurements'
    need_time_averaging <- if(aggregate_at_city_level) FALSE else TRUE
    value_col_name <- 'value'
    filter_fn <- filter_sanity_raw
    need_grouping_before_time_averaging <- FALSE
    need_grouping <- if(aggregate_at_city_level) FALSE else FALSE
    locs_meas_join_by <- if(aggregate_at_city_level) c("city", "country") else c("location_id"="id")
  }else if(average_by=='day'){
    table_name <- 'measurements_daily'
    need_time_averaging <- FALSE
    value_col_name <- 'avg_day'
    filter_fn <- filter_sanity_daily
    need_grouping_before_time_averaging <- FALSE
    need_grouping <- if(aggregate_at_city_level) TRUE else FALSE
    locs_meas_join_by <- c("location_id"="id")
  }else{
    table_name <- 'measurements_daily'
    need_time_averaging <- TRUE
    value_col_name <- 'avg_day'
    filter_fn <- filter_sanity_daily
    # First average per day across locations then within period hence the two groupings
    need_grouping_before_time_averaging <- if(aggregate_at_city_level) TRUE else FALSE
    need_grouping <- if(aggregate_at_city_level) TRUE else FALSE
    locs_meas_join_by <- c("location_id"="id")
  }

  meta_cols <- if(with_metadata){
    if (aggregate_at_city_level) c('country','geometry') else c("name", "location", "location_id", "country","geometry")
  }else{
    if (aggregate_at_city_level) c() else c("location", "location_id")
  }
  group_by_cols <- c('city', 'date', 'poll', 'timezone', meta_cols)
  if(add_noaa_station_ids){
    group_by_cols <- c(group_by_cols, 'noaa_station_ids')
  }


  # ----------------------
  # Perform actions
  #-----------------------
  # Prepare locations
  locs <- locations(country=country,
                    city=city,
                    id=location_id,
                    with_tz=T,
                    collect=F,
                    con = con)

  # If we aggregate at city level, we replace stations geometries with 'city' geometry
  # This is significantly faster than doing it on measurements (with lots of duplicated geometries)
  # The city geometry is mainly used if we want to plot results or find weather stations close by
  # Right now using ST_UNION (vs e.g. an enveloppe): it is better for the accurate look up of weather stations,
  # but not as good for beautiful maps (there will be several points per city)
  if(aggregate_at_city_level){
    locs <- locs %>% left_join(locs %>% group_by(country, city, timezone) %>%
                                 summarise(city_geometry=st_union(geometry)) %>% ungroup()
    ) %>%
      dplyr::mutate(geometry=city_geometry) %>% dplyr::select(-c(city_geometry))
  }

  if(add_noaa_station_ids){
    locs_group_by <- colnames(locs)
    locs <- locs %>% dplyr::left_join(
      tbl_safe(con,"noaa_ids_stations") %>% dplyr::select(noaa_station_id=id, noaa_geometry=geometry),
      sql_on= sprintf("(st_dwithin(\"LHS\".geometry::geography, \"RHS\".noaa_geometry::geography, %f))",noaa_station_radius_km*1000.0)
    ) %>% group_by_at(locs_group_by) %>%
      # Need to order for further group_by with noaa_station_ids not to be impacted
      dplyr::summarise(noaa_station_ids=array_agg(sql('noaa_station_id order by noaa_station_id'))) %>%
      dplyr::ungroup()
  }

  # Take measurements at these locations
  result <- tbl_safe(con, table_name)

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Joining location information
  # If city is defined, we first filter measurements to use index in Postgres
  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(city) == city_), # Single city name
                   result %>% dplyr::filter(tolower(city) %in% city_) # Vector of city names
  )

  result <- result %>%
    dplyr::right_join(locs, by=locs_meas_join_by, suffix = c("_remove", ""))

  # Filtering based on user request
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(poll) == poll_), # Single value
                   result %>% dplyr::filter(tolower(poll) %in% poll_) # Vector
  )

  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date >= date_from)
  )

  result <- switch(toString(length(date_to)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(date <= date_to)
  )

  result <- switch(toString(length(source_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(source) == source_), # Single value
                   result %>% dplyr::filter(tolower(source) %in% source_) # Vector
  )

  result <- filter_fn(result)

  if(!is.null(user_filter)){
    result <- user_filter(result)
  }


  # Apply time (and defacto spatial aggregation if aggregate_at_city_level==T) aggregation
  if(value_col_name!='value'){
    result <- result %>% dplyr::rename(value=all_of(value_col_name))
  }

  if(need_grouping_before_time_averaging){
    result <- result %>%
      dplyr::group_by_at(group_by_cols) %>%
      dplyr::summarize(value=avg(value)) %>%
      dplyr::ungroup()
  }

  if(need_time_averaging){
    result <- result %>% dplyr::mutate(date=DATE_TRUNC(average_by, date))
  }

  if(need_grouping){
    result <- result %>%
      dplyr::group_by_at(group_by_cols) %>%
      dplyr::summarize(value=avg(value)) %>%
      dplyr::ungroup()
  }else{
    result <- result %>%
      dplyr::select_at(c(group_by_cols, 'value'))
  }

  # To homogenize columns
  if(aggregate_at_city_level){
    if(with_metadata){
      result <- result %>% dplyr::mutate(location=NA, location_id=NA, name=NA)
    }else{
      result <- result %>% dplyr::mutate(location=NA, location_id=NA)
    }
  }

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}

# Cached version
m_measurements <- memoise(measurements, cache=fc)

standards <- function(collect=TRUE){
  # Connecting
  con = connection()
  standards  <- tbl_safe(con,"standards")
  if(collect){
    standards <- standards %>% collect()
  }
  return(standards)
}


targets <- function(country=NULL,
                    city=NULL,
                    region=NULL,
                    poll=NULL,
                    organization=NULL,
                    collect=TRUE){

  # Variable names must be different to column names
  country_ <- country
  city_ <- tolower(city)
  region_ <- tolower(region)
  poll_ <- tolower(poll)
  organization_ <- organization

  # Connecting
  con = connection()
  result  <- tbl_safe(con,"targets")

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Apply filters
  result <- switch(toString(length(country_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(country) == tolower(country_) | is.na(country) ), # Single value
                   result %>% dplyr::filter(country %in% country_  | is.na(country) ) # Vector
  )

  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(city) == city_  | is.na(city)), # Single value
                   result %>% dplyr::filter(tolower(city) %in% city_  | is.na(city)) # Vector
  )

  result <- switch(toString(length(region_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(region) == region_ | is.na(region) ), # Single value
                   result %>% dplyr::filter(tolower(region) %in% region_ | is.na(region) ) # Vector
  )

  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(poll) == poll_ | is.na(poll) ), # Single value
                   result %>% dplyr::filter(tolower(poll) %in% poll_ | is.na(poll) ) # Vector
  )

  result <- switch(toString(length(organization_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(organization == organization_), # Single value
                   result %>% dplyr::filter(organization %in% organization_) # Vector
  )

  if(collect){
    result <- result %>% collect()
  }
  return(result)
}


exceedances <- function(country=NULL,
                        city=NULL,
                        poll=NULL,
                        standard_org=NULL,
                        year=lubridate::year(now()),
                        collect=TRUE) {

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  poll_ <- poll
  year_ <- year
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

  result <- switch(toString(length(standard_org_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(standard_org == standard_org_), # Single value
                   result %>% dplyr::filter(standard_org %in% standard_org_) # Vector
  )

  result <- switch(toString(length(year_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(year == year_),
                   result %>% dplyr::filter(year %in% year_) # Vector
  )

  # Cast integer64 to integer. Prevents issues later on
  result <- result %>% dplyr::mutate_if(bit64::is.integer64, as.integer)

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}

scales <- function(poll=NULL){

  # Variable names must be different to column names
  poll_ <- poll

  # Connecting
  con = connection()
  result  <- tbl_safe(con,"scales")

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # Apply filters
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(poll) == poll_ | is.na(poll) ), # Single value
                   result %>% dplyr::filter(tolower(poll) %in% poll_ | is.na(poll) ) # Vector
  )

  result <- result %>% collect()

  pgarray_to_list <- function(column, as_type){
    column = gsub(fixed=T, "{", "", column)
    column = gsub(fixed=T, "}", "", column)
    column = gsub(fixed=T, "\\\"", "", column)
    return(lapply(column, function(x)  as_type(gsub(fixed=T, "\"", "", unlist(strsplit(x, ",", fixed=T), use.names = F)))))
  }

  result$thresholds <- pgarray_to_list(result$thresholds, as.numeric)
  result$labels <- pgarray_to_list(result$labels, as.character)
  result$colours <- pgarray_to_list(result$colours, as.character)

  return(result)
}


#' Attach weather information to air quality measurements
#'
#' @param meas dataframe / tibble of measurements
#' @param measurements_averaged_by The freequency at which measurements have been averaged to e.g. hour, day, week, month, year
#' (same aggregation needs to be applied to weather observations)
#' @param aggregate_at_city_level T/F whether to aggregate information at the city level. Staying at the location level may result in very large dataframe and double counting
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
join_weather_data <- function(meas, measurements_averaged_by='day', aggregate_at_city_level=TRUE, collect=TRUE, con=NULL){

  # Connecting
  con = if(!is.null(con)) con else connection()

  cols <- colnames(meas)
  group_by_cols <- cols

  # Check measurements have been fetched with metadata
  if(!"noaa_station_ids" %in% cols){
    stop("NOAA station ids missing. Use add_noaa_station_ids=T when querying measurements")
  }


  # Adding observations
  rhs_date <- ifelse(measurements_averaged_by!='hour',
                     sprintf("DATE_TRUNC('%s',\"RHS\".date)", measurements_averaged_by),
                     "\"RHS\".date")
  result <- meas %>% dplyr::left_join(
    tbl_safe(con,"noaa_ids_observations"),
    suffix=c("", "_noaa"),
    sql_on= sprintf("(\"RHS\".station_id=ANY(\"LHS\".noaa_station_ids) AND %s=\"LHS\".date)",rhs_date)
  )

  # Averaging per AQ measurement date
  result <- result %>% dplyr::group_by_at(group_by_cols) %>%
    dplyr::summarize(temp_c=mean(temp_c, na.rm=T),
                     slp_hp=mean(slp_hp, na.rm=T),
                     wind_deg=mean(wind_deg, na.rm=T),
                     wind_ms=mean(wind_ms, na.rm=T),
                     sky_code=max(sky_code, na.rm=T),
                     prec_1h_mm=max(prec_1h_mm, na.rm=T),
                     prec_6h_mm=max(prec_6h_mm, na.rm=T),
                     rh_percent=mean(rh_percent, na.rm=T)) %>%
    ungroup()

  # Removing unwanted columns (the less data, the faster the transfer betweeen DB and R)
  result <- result %>% dplyr::select(c(-noaa_station_ids, geometry))

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}


