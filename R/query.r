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
    # dplyr::filter(value >= 0) %>%
    #dplyr::filter(!is.na(location_id)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(!is.na(poll))  %>%
    dplyr::filter(value < 1500 | poll==CO)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}


processes <- function(con=NULL){
  con = if(!is.null(con)) con else connection()
  tbl_safe(con, "processes") %>%
    utils.unnest_json("filter",
                      filter_type = "type") %>%
    utils.unnest_json("agg_spatial",
                      region_type = "region_type",
                      weighting = "weighting") %>%
    utils.unnest_json("agg_temp",
                      period = "period")
}

locations <- function(country=NULL,
                      city=NULL,
                      id=NULL,
                      source=NULL,
                      collect=TRUE,
                      keep_only_for_dashboard=F,
                      with_geometry=TRUE,
                      with_meta=FALSE,
                      with_tz=NULL, #Deprecated
                      con=NULL){

  if(!is.null(with_tz)){
    with_meta = with_tz
    warning('with_tz argument has been replaced by with_meta')
  }

  # Variable names must be different to column names
  country_ <- tolower(country)
  city_ <- tolower(city)
  source_ <- tolower(source)
  id_ <- tolower(id)

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

  result <- switch(toString(length(source_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(source) == source_),
                   result %>% dplyr::filter(tolower(source) %in% source_)
  )

  result <- switch(toString(length(id_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(id) == id_), # Single station id
                   result %>% dplyr::filter(tolower(id) %in% id_) # Vector of station ids
  )

  if(keep_only_for_dashboard){
    result <- result %>% dplyr::filter(show_in_dashboard==TRUE)
  }

  # Keeping only interesting columns
  cols <- c("id", "name", "city", "country", "country_name", "gid_1", "name_1", "gid_2", "name_2")
  cols <- if(with_geometry)  c(cols, "geometry") else cols
  cols <- if(with_meta) c(cols, 'timezone', "last_scraped_data", "source", "last_updated") else cols

  result <- result %>% dplyr::select_at(cols)


  # Whether to collect the query i.e. actually run the query
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
#' @param source Source of the data. e.g. cpcb, openaq, openaq-archive
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
                         poll=NULL,
                         date_from='2015-01-01',
                         date_to=NULL,
                         source=NULL,
                         process_id=NULL,
                         best_source_only=F,
                         average_by='day',
                         collect=TRUE,
                         user_filter=NULL,
                         with_metadata=FALSE,
                         with_geometry=FALSE,
                         aggregate_level='city',
                         deweathered=F,
                         population_weighted=F,
                         con=NULL) {


  # Check input
  # Accept both NA and NULL
  location_id <- if(!is.null(location_id) && is.na(location_id)) NULL else location_id
  city <- if(!is.null(city) && length(city)==1 && is.na(city)) NULL else city
  country <- if(!is.null(country) && is.na(country)) NULL else country
  process_id <- if(!is.null(process_id) && is.na(process_id)) NULL else process_id

  if(aggregate_level=="location"){
    aggregate_level <- "station"
  }

  if(!aggregate_level %in% c('station','city','gadm1','gadm2','country')){
    stop("'aggregate_level' should be either 'location/station','city','gadm1','gadm2' or 'country'")
  }

  if(!is.null(source) & best_source_only){
    warning("Cannot select both a source and set best_source_only=T. Overwriting with best_source_only=F")
    best_source_only=F
  }

  if(!is.null(process_id) & !is.null(deweathered)){
    warning("Deweathered parameter is ignored when process_id is specified")
    deweathered <- NULL
  }

  # If location_id specified, we have to keep it
  # aggregate_at_city_level <- aggregate_at_city_level & is.null(location_id)

  # Note: variable names must be different to column names for dbplyr filters to work
  poll_ <- tolower(poll)
  source_ <- tolower(source)
  city_ <- tolower(city)
  country_ <- tolower(country)
  location_id_ <- tolower(location_id)
  process_id_ <- process_id #Index of process_id is not lowered

  # Connecting
  con = if(!is.null(con)) con else connection()

  # ------------------------------------------------
  # Look for processes that match user requirements
  #-------------------------------------------------
  procs <- processes(con) %>%
    dplyr::filter(region_type==aggregate_level,
                  average_by==period)

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

  procs %>% dplyr::filter("\"weighting\": \"gpw\"" %in% agg_spatial) %>% dplyr::select(agg_spatial)


  if(nrow(procs %>% dplyr::collect())==0){
    stop("No pre-processing found corresponding to required data. Are you at the right aggregation level (cities don't have population-weighted average) ?")
    #TODO implement calculation from raw in that case
    # return(NULL)
  }

  value_cols <- c("date","poll","unit","region_id","process_id","source","timezone","value")
  meta_cols <- if(with_metadata){
    switch(aggregate_level,
           "station" = c("region_name","country"),
           "city" = c("region_name","country"),
           "gadm1" = c("region_name","country"),
           "gadm2" = c("region_name","country",'gid_1', 'name_1'),
           "country" = c("region_name","country"),
           c("region_name","country"))
  }else{c()}

  if(with_geometry){
    meta_cols <- c(meta_cols, "geometry")
  }


  # ----------------------
  # Perform actions
  #-----------------------
  # Prepare locations
  locs <- locations(country=country,
                    city=city,
                    with_meta=T,
                    collect=F,
                    source= source_,
                    con = con) %>%
    dplyr::rename(location_id=id, location=name)

  loc_id_col <- region_id_col <- switch(aggregate_level,
                                        "station" = "id",
                                        "city" = "city",
                                        "gadm1" = "gid_1",
                                        "gadm2" = "gid_2",
                                        "country" = "country"
  )

  # Filtering by region_id (can be location_ids or gids...)
  # https://github.com/tidyverse/dbplyr/issues/296
  if(!is.null(location_id_) & length(location_id_)>0){
    quo <- switch(toString(length(location_id_)),
                  "0" = NULL,
                  "1" = dplyr:::apply_filter_syms(dplyr::any_vars(lower(.) == location_id_), syms(loc_id_col)),
                  quo <- dplyr:::apply_filter_syms(dplyr::any_vars(lower(.) %in% location_id_), syms(loc_id_col))
    )
    if(!is.null(quo)){
      locs <- locs %>% dplyr::filter(!!dbplyr::partial_eval(quo, loc_id_col))
    }
  }


  # Group locations by aggregation_level
  locs_group_by <- switch(aggregate_level,
                          "station" = NULL,
                          "city" = c("country", "city","source"),
                          "gadm1" = c("country", "gid_1", "name_1","source"),
                          "gadm2" = c("country", "gid_1", "name_1", "gid_2", "name_2","source"),
                          "country" = c("country","source"),
                          NULL
  )

  region_id_col <- switch(aggregate_level,
                          "station" = "location_id",
                          "city" = "city",
                          "gadm1" = "gid_1",
                          "gadm2" = "gid_2",
                          "country" = "country"
  )

  region_name_col <- switch(aggregate_level,
                            "station" = "location",
                            "city" = "city",
                            "gadm1" = "name_1",
                            "gadm2" = "name_2",
                            "country" = "country")

  if(!is.null(locs_group_by)){
    locs <- locs %>% dplyr::group_by_at(locs_group_by) %>%
      dplyr::summarise(geometry=ST_Union(geometry),
                       timezone= sql("mode() within group (order by timezone)")) %>%
      dplyr::ungroup()
  }

  locs <- locs %>%
    dplyr::mutate_at(region_id_col, .funs = list(region_id = ~(.))) %>%
    dplyr::mutate_at(region_name_col, .funs = list(region_name = ~(.)))

  # Use best source if asked
  # EEA is best for Europe
  # CPCB is best for India
  # MEE is best for China
  # OpenAQ for the rest
  if(!length(source_) & best_source_only){
    locs <- locs %>%
      dplyr::mutate(source_ranking=switch(source,"eea"=1,"mee"=1,"cpcb"=1,"openaq"=2, 3)) %>%
      dplyr::group_by(region_id) %>%
      dplyr::filter(source_ranking==min(source_ranking, na.rm=T))
  }

  # Take measurements at these locations
  result <- tbl_safe(con, "measurements_new")
  result <- switch(toString(length(source_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(tolower(source) == source_), # Single value
                   result %>% dplyr::filter(tolower(source) %in% source_) # Vector
  )

  result <- result %>%
    dplyr::mutate(region_id=tolower(region_id)) %>%
    dplyr::right_join(locs %>% dplyr::mutate(region_id=tolower(region_id)),
                      by=c("region_id","source"), suffix = c("_remove", ""))

  # R package uses 'poll' whilst db is using 'pollutant'
  result <- result %>% dplyr::rename(poll = pollutant)

  # First take data with the selected processing(s)
  result <- procs %>% dplyr::select(process_id=id) %>% dplyr::left_join(result, by=c("process_id"))

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

  result <- switch(toString(length(process_id_)),
                   "0" = result, # NULL
                   "1" = result %>% dplyr::filter(process_id == process_id_), # Single value
                   result %>% dplyr::filter(process_id %in% process_id_) # Vector
  )



  if(!is.null(user_filter)){
    result <- user_filter(result)
  }

  # Whether to collect the query i.e. actually run the query
  if(collect){
    result <- result %>% dplyr::select(all_of(c(value_cols, meta_cols))) %>% dplyr::collect()
    # Localize time
    # We can't use purrr:map since it won't deal with datetime objects
    # hence the rowwise
    if(nrow(result)>0){
      result <- result %>% dplyr::rowwise() %>% tidyr::replace_na(list('timezone'='UTC')) %>%
        dplyr::mutate(date=lubridate::force_tz(date,tzone=timezone))
    }
  }

  return(result)
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


