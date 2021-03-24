filter_sanity_daily <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>%
    # dplyr::filter(avg_day > 0) %>%
    dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(avg_day < 1500 | poll==CO)
  # result <- result %>% dplyr::filter(parameter != 'o3' || value > -9999)
  return(result)
}


filter_sanity_raw <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>%
    # dplyr::filter(value >= 0) %>%
    #dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%a
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(value < 1500 | poll==CO)
  # result <- result %>% dplyr::filter(parameter != 'o3' || value > -9999)
  return(result)
}


processes <- function(con=NULL, collect=T){
  con = if(!is.null(con)) con else connection()
  p <- tbl_safe(con, "processes") %>%
    utils.unnest_json("filter",
                      filter_type = "type") %>%
    utils.unnest_json("agg_spatial",
                      region_type = "region_type",
                      weighting = "weighting") %>%
    utils.unnest_json("agg_temp",
                      period = "period")

  if(collect){
    p <- p %>% dplyr::collect()
  }

  return(p)
}



cities <- function(
  id=NULL,
  name=NULL,
  country=NULL, #ISO2
  source=NULL,
  with_metadata=F,
  with_geometry=F,
  with_source=F, #Cities do not have source. But if with_source=T, then we'll add a row for each source that has this city
  collect=T,
  con=NULL
){

  # Connecting
  con = if(!is.null(con)) con else connection()
  c <- tbl_safe(con, "cities")

  # Filter: id
  if(!is.null(id)){
    c <- c %>% dplyr::filter(id %in% !!tolower(id))
  }

  # Filter: name
  if(!is.null(name)){
    c <- c %>% dplyr::filter(tolower(name) %in% !!tolower(name))
  }

  # Filter: country
  if(!is.null(country)){
    c <- c %>% dplyr::filter(country_id %in% !!toupper(country))
  }

  # Filter: source
  if(!is.null(source) | with_source){
    # Cities don't have a source field, but we look
    # for stations of that source and extract
    source_stations <- stations(source=source, con=con, collect=F)
    c <- c %>% dplyr::inner_join(source_stations %>% dplyr::select(id=city_id, source) %>% dplyr::distinct())
  }

  c <- c %>% dplyr::rename(country=country_id)

  # Keeping only interesting columns
  cols <- c("id", "level", "name", "country")
  cols <- if(with_geometry)  c(cols, "geometry") else cols
  cols <- if(with_metadata) c(cols, "timezone", "name_local", "gadm1_id") else cols
  cols <- if(with_source) c(cols, "source") else cols
  c <- c %>% dplyr::select_at(cols)

  if(with_metadata){
    c <- c %>% left_join(
      gadm1s(con) %>% dplyr::select(gadm1_id, gadm1_name))
  }

  if(collect){
    c <- c %>% dplyr::collect()
    if(with_geometry){
      c <- c %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }
  }

  return(c)
}


stations <- function(
  id=NULL,
  source=NULL,
  city=NULL, #city name
  country=NULL, #ISO2
  type=NULL,
  with_metadata=F,
  with_geometry=F,
  collect=T,
  keep_with_measurements_only=FALSE, #Only keep locations that actually have measurements
  con=NULL
){

  # Connecting
  con = if(!is.null(con)) con else connection()
  s <- tbl_safe(con, "stations")

  # Filter: source
  if(!is.null(source)){
    s <- s %>% dplyr::filter(source==!!tolower(source))
  }

  # Filter: country
  if(!is.null(country)){
    s <- s %>% dplyr::filter(country_id %in% !!toupper(country))
  }

  # Filter: id
  if(!is.null(id)){
    s <- s %>% dplyr::filter(id %in% !!tolower(id))
  }

  # Filter: type
  if(!is.null(type)){
    s <- s %>% dplyr::filter(type==!!tolower(type))
  }

  # Filter: city
  if(!is.null(city)){
   c <- cities(name=city, country=country, con=con, collect=F)
   # Don't use source or it will be circular
   s <- s %>% dplyr::inner_join(c %>% dplyr::select(city_id=id))
  }

  if(keep_with_measurements_only){
    # Only fast enough if location_id is indexed
    s <- s %>% dplyr::inner_join(tbl_safe(con, "measurements") %>% dplyr::distinct(location_id) %>% dplyr::select(id=location_id))
  }

  if(with_metadata){
    s <- s %>% dplyr::inner_join(cities(con=con, collect=F, with_metadata=T) %>% dplyr::select(city_id=id, city_name=name, gadm1_id))
  }

  s <- s %>% dplyr::rename(country=country_id)

  # Keeping only interesting columns
  cols <- c("id", "level", "city_id", "country", "source")
  cols <- if(with_geometry)  c(cols, "geometry") else cols
  cols <- if(with_metadata) c(cols, "name", "timezone", "type", "city_name", "gadm1_id") else cols
  s <- s %>% dplyr::select_at(cols)

  if(with_metadata){
    s <- s %>% left_join(
      gadm1s(con) %>% dplyr::select(gadm1_id, gadm1_name))
  }

  if(collect){
    s <- s %>% dplyr::collect()
    if(with_geometry){
      s <- s %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }
  }

  return(s)
}


gadm1s <- function(con=NULL){
  # Connecting
  con = if(!is.null(con)) con else connection()
  s <- tbl_safe(con, "gadm1") %>%
    dplyr::rename(gadm1_id=gid_1,
                  gadm1_name=name_1) %>%
    dplyr::mutate(gadm1_id=tolower(gadm1_id))
  return(s)
}



# Cached version
# m_measurements <- memoise(measurements, cache=fc)
standards <- function(collect=TRUE){
  # Connecting
  con = connection()
  standards  <- tbl_safe(con,"standards")
  if(collect){
    standards <- standards %>% dplyr::collect()
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
    result <- result %>% dplyr::collect()
  }
  return(result)
}


exceedances <- function(country=NULL,
                        city=NULL,
                        poll=NULL,
                        standard_org=NULL,
                        year=lubridate::year(lubridate::now()),
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

  result <- result %>% dplyr::collect()

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
#' meas_w_weather <- rcrea::join_weather_data(meas)
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
    dplyr::ungroup()

  # Removing unwanted columns (the less data, the faster the transfer betweeen DB and R)
  result <- result %>% dplyr::select(c(-noaa_station_ids, geometry))

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::collect()
  }

  return(result)
}


