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

  if(collect){
    s <- s %>% dplyr::collect()
    if(with_geometry){
      s <- s %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }
  }

  return(s)
}


locations <- function(
  level, #"city" or "station",
  id=NULL, #station_id or city_id depending on level

  # Other indirect search parameters
  country=NULL,
  city=NULL, #name
  type=NULL,
  source=NULL,
  source_city=NULL, #Nested source:[city]

  # Query parameters
  collect=TRUE,
  with_geometry=TRUE,
  with_metadata=FALSE,
  keep_with_measurements_only=FALSE, #Only keep locations that actually have measurements
  con=NULL){

  if(length(setdiff(level, c("station","city")))){
    stop("level should be either 'station' or 'city' or both")
  }

  if("city" %in% level & !is.null(type)){
    warning("type is ignored when level=='city'")
  }

  if(!is.null(source) & !is.null(source_city)){
    warning("Cannot define both source and source_city. Overwriting with source_city")
    source <- NULL
  }

  if(is.null(source_city) & !is.null(source)){
    source_city <- utils.to_source_city(source, city)
    city <- NULL
  }

  result <- NULL
  con = if(!is.null(con)) con else connection()


  if("station" %in% level){
    if(!is.null(source_city)){
      # Some sources were indicated, with or without cities
      for(source_ in names(source_city)){
        r <- stations(id=id, source=source_, city=source_city[[source_]], country=country, type=type,
                           with_metadata=with_metadata, with_geometry=with_geometry, collect=F,
                           con=con)
        result <- if(is.null(result)){r}else{dplyr::union(result, r)}
      }
    }else{
      result <- stations(id=id, source=source, city=city, country=country, type=type,
                    with_metadata=with_metadata, with_geometry=with_geometry, collect=F,
                    con=con)
    }
  }

  if("city" %in% level){
    if(!is.null(source_city)){
      # Some sources were indicated, with or without cities
      for(source_ in names(source_city)){
        r <- cities(id=id, name=source_city[[source_]], country=country,
                    source=source_, with_metadata=with_metadata, with_geometry=with_geometry,
                    with_source=T,
                    collect=F, con=con) %>%
          dplyr::mutate(city_name=name)

        result <- if(is.null(result)){r}else{dplyr::union(result, r)}
      }
    }else{
      r <- cities(id=id, name=city, country=country, source=source,
                  with_metadata=with_metadata, with_source=T,
                  with_geometry=with_geometry, collect=F,
                  con=con) %>%
        dplyr::mutate(city_name=name)

      result <- if(is.null(result)){r}else{dplyr::union(result, r)}
    }
  }

  if(keep_with_measurements_only){
    # Only fast enough if location_id is indexed
    result <- result %>% dplyr::inner_join(tbl_safe(con, "measurements") %>% dplyr::distinct(location_id) %>% dplyr::select(id=location_id))
  }

  if(collect){
    result <- result %>% dplyr::collect()
    if(with_geometry){
      result <- result %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }
  }

  return(result)
}


#' Query Measurements
#'
#' @param country ISO2 code of the country of interest [optional]
#' @param city City as indicated in OpenAQ location
#' @param location_id Identifier of the air quality measurement station (referring to \code{id} in \code{\link{stations}})
#' @param poll Pollutant name (e.g. rcrea::CO, "co", rcrea::PM25, "pm25")
#' @param date_from Beginning date of queried measurements ('yyyy-mm-dd')
#' @param date_to End date of queried measurements ('yyyy-mm-dd')
#' @param source Source of the data. e.g. cpcb, openaq, eea, airkorea, jp, defra
#' @param average_by How to time-average results e.g. 'hour', day', 'week', 'month' or 'year'
#' @param collect T/F Whether to collect results into local tibble (see \code{\link{dbplyr::collect}})
#' @param with_metadata T/F Whether to add additional information columnes (e.g. city, country, location name, geometry). If True, query takes significantly more time
#' @param user_filter Additional filtering function for measurements applied before time aggregation
#' @param aggregate_at_city_level T/F Whether or not to keep measurements at the station level (otherwise, aggregate at city level)
#' @param deweathered If NULL, ignored. If T, only deweathered measurements. If F, only non deweather measurements
#' @return a tibble (locally collected or not) of measurements matching search criteria.
#' @export
#'
#' @examples
#' meas_bj <- rcrea::measurements(city=c('Beijing'), date_from='2018-01-01', average_by='month', with_metadata=T)
#'
measurements <- function(country=NULL,
                         city=NULL,
                         location_id=NULL,
                         location_type=NULL,
                         poll=NULL,
                         date_from='2015-01-01',
                         date_to=NULL,
                         source=NULL,
                         source_city=NULL,
                         process_id=NULL,
                         best_source_only=F,
                         average_by='day',
                         collect=TRUE,
                         user_filter=NULL,
                         with_metadata=FALSE,
                         with_geometry=FALSE,
                         aggregate_level='city', # city or station
                         deweathered=F,
                         population_weighted=F,
                         con=NULL) {


  # Check input
  # Accept both NA, NULL and empty
  location_id <- if(!is.null(location_id) && (length(location_id)==1) && is.na(location_id)) NULL else location_id
  city <- if(!is.null(city) && (length(city)==1) && is.na(city)) NULL else city
  country <- if(!is.null(country) && (length(country)==1) && is.na(country)) NULL else country
  process_id <- if(!is.null(process_id)  && (length(process_id)==1) && is.na(process_id)) NULL else process_id

  if(aggregate_level=="location"){
    aggregate_level <- "station"
  }

  if(!aggregate_level %in% c('station','city')){
    stop("'aggregate_level' should be either 'station' or 'city'")
  }

  if(!is.null(source) & best_source_only){
    warning("Cannot select both a source and set best_source_only=T. Overwriting with best_source_only=F")
    best_source_only=F
  }

  if(!is.null(process_id) & !is.null(deweathered)){
    warning("Deweathered parameter is ignored when process_id is specified")
    deweathered <- NULL
  }

  if(!is.null(process_id) & !is.null(aggregate_level)){
    warning("aggregate_level parameter is ignored when process_id is specified")
    aggregate_level <- NULL # Will be updated lated
  }

  if(!is.null(process_id) & !is.null(average_by)){
    warning("average_by parameter is ignored when process_id is specified")
    average_by <- NULL
  }
  # If location_id specified, we have to keep it
  # aggregate_at_city_level <- aggregate_at_city_level & is.null(location_id)

  # Connecting if required (if con is NULL or invalid)
  con = rcrea::connection(current_connection=con)

  # ------------------------------------------------
  # Look for processes that match user requirements
  #-------------------------------------------------
  procs <- rcrea::processes(con=con, collect=F)

  if(!is.null(aggregate_level)){
    procs <- procs %>% dplyr::filter(aggregate_level==region_type)
  }

  if(!is.null(location_type) && !is.null(aggregate_level) && (aggregate_level!="station")){
    procs <- procs %>%
      dplyr::filter(sql('agg_spatial::text') %like% !!paste0("%\"station_type\": \"",location_type,"\"%"))
  }

  if(!is.null(average_by)){
    procs <- procs %>% dplyr::filter(average_by==period)
  }

  if(!is.null(deweathered)){
    procs <- procs %>% dplyr::filter(
      (is.null(deweather) & !deweathered) |
        (!is.null(deweather) & deweathered))
  }

  if(!is.null(population_weighted)){
    procs <- procs %>% dplyr::filter(
      (is.null(weighting) & !population_weighted) |
        (!is.null(weighting) & population_weighted))
  }

  if(!is.null(process_id)){
    procs <- procs %>% dplyr::filter(id %in% !!process_id)
    aggregate_level <- procs %>% dplyr::distinct(region_type) %>% dplyr::pull()
    if(length(aggregate_level)>1){
      stop("Can only specify process_id with similar aggregation_level")
    }
  }

  # procs %>% dplyr::filter("\"weighting\": \"gpw\"" %in% agg_spatial) %>% dplyr::select(agg_spatial)

  if(nrow(procs %>% dplyr::collect())==0){
    stop("No pre-processing found corresponding to required data. Are you at the right aggregation level (cities don't have population-weighted average) ?")
    #TODO implement calculation from raw in that case
    # return(NULL)
  }

  value_cols <- c("location_id","location_name","process_id","date","poll","unit","source","value","timezone","country")
  meta_cols <- if(with_metadata) c("gadm1_id") else c()

  # Attach geometry
  # If collect, we attach it after, too slow to download otherwise
  if(with_geometry & !collect){
    meta_cols <- c(meta_cols, "geometry")
  }


  # ----------------------
  # Perform actions
  #-----------------------
  # Prepare locations
  locs <- rcrea::locations(
    id=location_id,
    level=aggregate_level,
    source=source,
    city=city,
    source_city=source_city,
    country=country,
    type=location_type,
    with_metadata=T,
    collect=F,
    con = con) %>%

    dplyr::rename(location_id=id, location_name=name)


  # Filtering by location_id (can be station_id or city_id)
  # https://github.com/tidyverse/dbplyr/issues/296
  if(!is.null(location_id) & length(location_id)>0){
    locs <- locs %>% dplyr::filter(location_id %in% !!tolower(location_id))
  }

  # Use best source if asked
  # EEA is best for Europe
  # CPCB is best for India
  # MEE is best for China
  # OpenAQ for the rest
  if(best_source_only){
    locs <- locs %>%
      dplyr::mutate(source_ranking=switch(source,"eea"=1,"mee"=1,"cpcb"=1,"csb"=1,"jp"=1,"airkorea"=1, "openaq"=2, "airvisual"=1, 3)) %>%
      dplyr::group_by(location_id) %>%
      dplyr::filter(source_ranking==min(source_ranking, na.rm=T))
  }

  # Take measurements at these locations
  result <- tbl_safe(con, "measurements")

  if(!is.null(source)){
    result <- result %>% dplyr::filter(source %in% !!source)
  }

  m_l_joining_cols <- ifelse(is.null(source) && is.null(source_city) && aggregate_level=="city",
                             c("location_id"),
                             c("location_id","source"))

  result <- result %>%
    dplyr::right_join(locs,
                      by=m_l_joining_cols, suffix = c("_remove", ""))

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # First take data with the selected processing(s)
  result <- procs %>% dplyr::select(process_id=id) %>% dplyr::left_join(result, by=c("process_id"))

  # Filtering based on user request
  if(!is.null(poll)){
    result <- result %>% dplyr::filter(poll %in% !!poll)
  }

  if(!is.null(date_from)){
    result <- result %>% dplyr::filter(date >= date_from)
  }

  if(!is.null(date_to)){
    result <- result %>% dplyr::filter(date <= date_to)
  }

  if(!is.null(process_id)){
    result <- result %>% dplyr::filter(process_id %in% !!process_id)
  }

  if(!is.null(user_filter)){
    result <- user_filter(result)
  }

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::select(all_of(c(value_cols, meta_cols))) %>% dplyr::collect()

    if(with_geometry){
      result <- result %>% dplyr::left_join(locs %>% dplyr::select(location_id, geometry) %>% collect())
      result <- result %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }

    # Localize time
    # We can't use purrr:map since it won't deal with datetime objects
    # hence the rowwise
    if(nrow(result)>0){
      result <- result %>% dplyr::rowwise() %>% tidyr::replace_na(list('timezone'='UTC')) %>%
        dplyr::mutate(date=lubridate::force_tz(date,tzone=timezone))
    }
  }

  return(result %>% dplyr::ungroup())
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


