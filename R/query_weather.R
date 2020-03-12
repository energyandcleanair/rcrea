source('R/setup.r')
require(worldmet)

#' Attach NOAA ISD weather information to air quality measurements
#'
#' @param meas dataframe / tibble of measurements
#' @param measurements_averaged_by The freequency at which measurements have been averaged to e.g. hour, day, week, month, year
#' (same aggregation needs to be applied to weather observations)
#' @param collect T/F Whether to collect results into local tibble (see \code{\link{dbplyr::collect}})
#' @param radius_km Maximum distance to look for weather stations close to air quality ones
#' @param con Driver connection to Postgres. If null, will try the global one
#'
#' @return a tibble (locally collected or not) of measurements left joined to weather data
#'
#' @examples
#' meas_w_weather <- creadb::join_weather_data(meas)
#'
weather.isd.join.using_worldmet <- function(meas, measurements_averaged_by='day', aggregate_at_city_level=T, radius_km=20){

  if(!aggregate_at_city_level){
    stop("aggregate_at_city_level=F not yet supported")
  }

  # Collect if not already collected
  if(is.na(nrow(meas))){
    message("Collecting measurements")
    meas <- meas %>% collect()
    message("Done")
  }

  # # Remind R that measurements date is local
  # if(measurements_averaged_by=='day'){
  #   meas <- meas %>% mutate(date_local=purrr::map2_chr(date, timezone, ~strftime(lubridate::force_tz(.x,.y))) %>% lubridate::ymd())
  # }else{
  #   # no real need
  #   meas <-  meas %>% mutate(date_local=purrr::map2_chr(date, timezone, ~strftime(.x, "%Y-%m-%d %H:%M:%S")) %>% lubridate::ymd_hms())
  # }
  # a<-meas[1,]

  # Get stations close to locations
  city = unique(meas$city)
  locs <- locations(city=city, collect=T, with_tz=T)
  locs <- locs %>% mutate(noaa_stations = purrr::map(.x=geometry, ~rnoaa::isd_stations_search(lat = sf::st_coordinates(.x)[2],
                                                                               lon = sf::st_coordinates(.x)[1],
                                                                               radius = radius_km) %>% select(usaf, wban)
  ))
  locs <- locs %>% group_by(city, timezone) %>% summarize(noaa_stations=list(unique(bind_rows(noaa_stations))))
  locs <- locs %>% tidyr::unnest(noaa_stations)

  locs_weather <- locs %>% mutate(weather = purrr::map2(.x=usaf, .y=wban, purrr::possibly(~worldmet::importNOAA(
                                                                                          code = paste(.x, .y, sep="-"),
                                                                                          year = lubridate::year(min(meas$date)):lubridate::year(max(meas$date)),
                                                                                          hourly = (measurements_averaged_by=='hour'),
                                                                                          precip = TRUE,
                                                                                          PWC = FALSE,
                                                                                          parallel = F,
                                                                                          quiet = FALSE,
                                                                                          path = NA
                                                                                        ),
                                                                                        otherwise = NA)
  ))

  locs_weather <- locs_weather %>% select(city, timezone, weather) %>% filter(!is.na(weather)) %>% tidyr::unnest(weather)

  # Create local date time to merge with measurements that are in local time
  locs_weather <- locs_weather %>% dplyr::mutate(date_local=purrr::map2_chr(date, timezone, ~as.character(lubridate::with_tz(.x,.y))) %>% lubridate::ymd_hms())
  locs_weather <- locs_weather %>% mutate(date_local=lubridate::floor_date(date_local,measurements_averaged_by))

  # Aggregate per city
  locs_weather <- locs_weather %>% dplyr::group_by(city, date_local) %>%
    dplyr::summarize(air_temp=mean(air_temp, na.rm=T),
                     atmos_pres=mean(atmos_pres, na.rm=T),
                     wd=mean(wd, na.rm=T),
                     ws=mean(ws, na.rm=T),
                     ceil_hgt=mean(ceil_hgt),
                     visibility=mean(visibility, na.rm=T),
                     precip=mean(precip, na.rm=T),
                     RH=mean(RH, na.rm=T)) %>%
    ungroup()


  meas_weather <- meas %>% left_join(locs_weather, by = c("city" = "city", "date" = "date_local"))
  return(meas_weather)

}

weather.isd.join <- function(meas, measurements_averaged_by='day', collect=TRUE, con=NULL){

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
  result <- result %>%
    dplyr::mutate(sky_code = as.numeric(sky_code)) %>%
    dplyr::group_by_at(group_by_cols) %>%
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
  result <- result %>% dplyr::select(c(-noaa_station_ids, -geometry))

  # Whether to collect the query i.e. actually run the query
  if(collect){
    message("Collecting results")
    result <- result %>% dplyr::collect()
    message("Done")
  }

  return(result)
}


weather.ghcnd.join <- function(meas, weather_radius_km=50){

  # Collect if not already collected
  if(is.na(nrow(meas))){
    message("Collecting measurements")
    meas <- meas %>% collect()
    message("Done")
  }

  #TODO add countries
  cities <- unique(meas$city)
  locs <- locations(city=cities) %>%
    group_by(city) %>%
    summarize(city_geometry=sf::st_centroid(sf::st_union(geometry))) %>%
    ungroup()

  # Get nearest GHCND station
  message("Collecting GHCND stations. May take a while if not in cache")
  # set up cache
  # if(!exists("ghcnd.m.ghcnd_stations")){
  # }

  ghcnd.stations <- ghcnd.m.ghcnd_stations()
  message("Done")

  locs <- locs %>% mutate(longitude = purrr::map_dbl(city_geometry, ~sf::st_coordinates(.x)[[1]]),
                  latitude =  purrr::map_dbl(city_geometry, ~sf::st_coordinates(.x)[[2]]),
                  id=row_number())

  # we take all stations within 50km and will average across them
  # to maximize changes of getting precipitation data
  find_station_ <- function(id, latitude, longitude){
    rnoaa::meteo_nearby_stations(tibble(id, latitude, longitude),
                                 station_data = ghcnd.stations,
                                 var="PRCP",
                                 year_min=2013,
                                 radius=weather_radius_km)[[1]]
  }

  locs_ghcnd <- locs %>% mutate(ghcnd = pmap(list(id, latitude, longitude), ~find_station_(...))) %>%
    select(-c(latitude, longitude, id)) %>%
    tidyr::unnest(cols=c(ghcnd)) %>% rename(ghcnd_id=id)

  # Add weather data and unnest
  get_data_ <- function(ghcnd_id){
    rnoaa::meteo_tidy_ghcnd(ghcnd_id,var="PRCP",date_min=min(meas$date))
  }
  locs_weather <- locs_ghcnd %>% mutate(weather = pmap(list(ghcnd_id),~get_data_(...)))  %>%
    tidyr::unnest(cols=weather) %>%
    select(city, date, prcp) %>%
    rename(day=date)

  locs_weather <- locs_weather %>% group_by(city, day) %>% summarize(prcp=mean(prcp, na.rm = T))


  # Merge measurements and weather
  meas_weather <- meas %>% mutate(day=lubridate::date(date))
  meas_weather <- meas_weather %>% left_join(locs_weather, by=c('city', 'day')) %>% select(-c(day))

  return(meas_weather)
}
