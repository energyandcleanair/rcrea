
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
                         aggregate_level='city', # city, station, gadm1, (based on stations), gadm2 (based on stations) or country (based on stations)
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

  if(!aggregate_level %in% c('station', 'city', 'gadm1', 'gadm2', 'country')){
    stop("'aggregate_level' should be either 'station', 'city', 'gadm1', 'gadm2', 'country'")
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

  if(!is.null(process_id)){
    procs <- procs %>% dplyr::filter(id %in% !!process_id)
    aggregate_level <- procs %>% dplyr::distinct(region_type) %>% dplyr::pull()
    if(length(aggregate_level)>1){
      stop("Can only specify process_id with similar aggregation_level")
    }
  }

  # Aggregation level
  # Database is filled with station-level data AND pre-aggregated city-level data
  # For GADM and country, we query at station level and aggregate accordingly
  process_region_type <- dplyr::recode(aggregate_level,
                              "city"="city",
                              .default="station") #for GADM1, GADM2, Country, we use stations

  need_aggregation <- process_region_type != aggregate_level
  aggregation_col <- dplyr::recode(aggregate_level,
                                      "gadm2"="gadm2_id",
                                      "gadm1"="gadm1_id",
                                      "country"="country")
  aggregation_col_name <- dplyr::recode(aggregate_level,
                                      "gadm2"="gadm2_name",
                                      "gadm1"="gadm1_name",
                                      "country"="country")

  loc_filter_col <- dplyr::recode(aggregate_level,
                              "station"="location_id",
                              "city"="location_id",
                              "gadm2"="gadm2_id",
                              "gadm1"="gadm1_id",
                              "country"="country")

  procs <- procs %>% dplyr::filter(region_type==process_region_type)


  if(!is.null(location_type) && !is.null(aggregate_level) && (aggregate_level!="station")){
    procs <- procs %>%
      dplyr::filter(sql('agg_spatial::text') %like% !!paste0("%\"station_type\": \"",location_type,"\"%"))
  }

  if(!is.null(average_by)){
    procs <- procs %>%
      dplyr::filter(period==!!average_by,
                    period_fn=="avg") # We exclude 8h_max or 1h_max aggregations by default
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

  # procs %>% dplyr::filter("\"weighting\": \"gpw\"" %in% agg_spatial) %>% dplyr::select(agg_spatial)

  if(nrow(procs %>% dplyr::collect())==0){
    stop("No pre-processing found corresponding to required data. Are you at the right aggregation level (cities don't have population-weighted average) ?")
    #TODO implement calculation from raw in that case
    # return(NULL)
  }

  value_cols <- c("location_id","location_name","process_id","date","poll","unit","source","value","timezone","country")
  meta_cols <- if(with_metadata) c("gadm1_id","gadm1_name") else c()

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
    level=process_region_type,
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
  # or gadm id if aggregate level asks so
  # https://github.com/tidyverse/dbplyr/issues/296
  if(!is.null(location_id) & length(location_id)>0){
    if(loc_filter_col=="country"){
      locs <- locs %>% filter_at(vars(all_of(loc_filter_col)), all_vars(tolower(.) %in% !!tolower(location_id)))
    }else{
      # This one uses db indexes. And location_id, gadm_ids are (should be) all lowercases in the db
      locs <- locs %>% filter_at(vars(all_of(loc_filter_col)), all_vars(. %in% !!tolower(location_id)))
    }

  }

  # Use best source if asked
  # EEA is best for Europe
  # CPCB is best for India
  # MEE is best for China
  # OpenAQ for the rest
  if(best_source_only){
    locs <- locs %>%
      dplyr::mutate(source_ranking=switch(source,"eea"=1,"mee"=1,"cpcb"=1,"csb"=1,"jp"=1,"airkorea"=1,"aurn"=1,"defra"=1,"openaq_government"=1,"openaq"=2,"openaq_community"=2,"openaq_research"=2,"airvisual"=2, 3)) %>%
      dplyr::group_by(location_id) %>%
      dplyr::filter(source_ranking==min(source_ranking, na.rm=T))
  }

  # Take measurements at these locations
  result <- tbl_safe(con, "measurements")

  if(!is.null(source)){
    result <- result %>% dplyr::filter(source %in% !!source)
  }

  # m_l_joining_cols <- intersect(c("location_id","source"),
  #                               names(locs))

  result <- result %>%
    dplyr::right_join(locs,
                      # by=m_l_joining_cols,
                      suffix = c("_remove", ""))

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


  if(need_aggregation){
    group_cols <- c(setdiff(c(value_cols,meta_cols), c("location_id","location_name","value")))
    names(group_cols) <- group_cols
    group_cols <- c(group_cols, c("location_id"=aggregation_col, "location_name"=aggregation_col_name))

    result <- result %>%
      group_by_at(group_cols) %>%
      summarise(value=mean(value, na.rm=T))
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
