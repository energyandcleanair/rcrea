source('R/setup.r')
# require(worldmet)
# require(sirad)

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
#' meas_w_weather <- rcrea::join_weather_data(meas)
#'
weather.isd.join.using_worldmet <- function(meas, measurements_averaged_by='day', aggregate_at_city_level=T, radius_km=20){

  if(!aggregate_at_city_level){
    stop("aggregate_at_city_level=F not yet supported")
  }

  # Collect if not already collected
  if(is.na(nrow(meas))){
    message("Collecting measurements")
    meas <- meas %>% collect()
    # Localize time
    meas <- meas %>% dplyr::rowwise() %>% dplyr::mutate(date=lubridate::force_tz(date,tzone=timezone))
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

  locs_weather <- locs_weather %>% dplyr::select(city, timezone, weather) %>% filter(!is.na(weather)) %>% tidyr::unnest(weather)

  # Create local date time to merge with measurements that are in local time
  locs_weather <- locs_weather %>%
    dplyr::rowwise() %>%
    dplyr::mutate(date_local=lubridate::with_tz(date, tzone=timezone))
  locs_weather <- locs_weather %>%
    dplyr::mutate(date_local=lubridate::floor_date(date_local,measurements_averaged_by))

  # Aggregate per city
  locs_weather <- locs_weather %>% dplyr::group_by(city, date_local) %>%
    dplyr::summarize(air_temp=mean(air_temp, na.rm=T),
                     atmos_pres=mean(atmos_pres, na.rm=T),
                     wd=mean(wd, na.rm=T),
                     ws=mean(ws, na.rm=T),
                     ceil_hgt=mean(ceil_hgt, na.rm=T),
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
  # We bring noaa measurements to local timezone using measurements timezone
  rhs_date <- sprintf("DATE_TRUNC('%s',timezone('UTC',\"RHS\".date_utc) AT TIME ZONE \"LHS\".timezone)", measurements_averaged_by)
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
  result <- result %>% dplyr::select(c(-noaa_station_ids))
  if ('geometry' %in% colnames(result)){
    result <- result %>% dplyr::select(c(-geometry))
  }

  # Whether to collect the query i.e. actually run the query
  if(collect){
    message("Collecting results")
    result <- result %>% dplyr::collect()
    result <- result %>% dplyr::rowwise() %>% dplyr::mutate(date=lubridate::force_tz(date,tzone=timezone))
    message("Done")
  }

  return(result)
}


weather.ghcnd.join <- function(meas, weather_radius_km=50){

  # Collect if not already collected
  if(is.na(nrow(meas))){
    message("Collecting measurements")
    meas <- meas %>% dplyr::collect()
    message("Done")
  }

  #TODO add countries
  cities <- unique(meas$city)
  locs <- locations(city=cities) %>%
    dplyr::group_by(city) %>%
    dplyr::summarize(city_geometry=sf::st_centroid(sf::st_union(geometry))) %>%
    ungroup()

  # Get nearest GHCND station
  message("Collecting GHCND stations. May take a while if not in cache")
  # set up cache
  # if(!exists("ghcnd.m.ghcnd_stations")){
  # }

  ghcnd.stations <- ghcnd.m.ghcnd_stations()
  message("Done")

  locs <- locs %>% dplyr::mutate(longitude = purrr::map_dbl(city_geometry, ~sf::st_coordinates(.x)[[1]]),
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

  locs_ghcnd <- locs %>% mutate(ghcnd = purrr::pmap(list(id, latitude, longitude), ~find_station_(...))) %>%
    select(-c(latitude, longitude, id)) %>%
    tidyr::unnest(cols=c(ghcnd)) %>% dplyr::rename(ghcnd_id=id)

  # Add weather data and unnest
  get_data_ <- function(ghcnd_id){
    rnoaa::meteo_tidy_ghcnd(ghcnd_id,var="PRCP",date_min=min(meas$date))
  }
  locs_weather <- locs_ghcnd %>% mutate(weather = pmap(list(ghcnd_id),~get_data_(...)))  %>%
    tidyr::unnest(cols=weather) %>%
    dplyr::select(city, date, prcp) %>%
    dplyr::rename(day=date)

  locs_weather <- locs_weather %>% group_by(city, day) %>% summarize(precip_ghcnd=mean(prcp, na.rm = T))


  # Merge measurements and weather
  meas_weather <- meas %>% mutate(day=lubridate::date(date))
  meas_weather <- meas_weather %>% left_join(locs_weather, by=c('city', 'day')) %>% select(-c(day))

  return(meas_weather)
}

weather.sirad.join <- function(meas){

  # Collect if not already collected
  if(is.na(nrow(meas))){
    message("Collecting measurements")
    meas <- meas %>% dplyr::collect()
    message("Done")
  }

  # Check measurements are hourly ones
  if(length(unique(lubridate::hour(meas$date)))!=24){
    stop("Sunshine data only works with hourly measurements")
  }

  #TODO add countries
  cities <- unique(meas$city)
  locs <- locations(city=cities, with_tz = T) %>%
    group_by(city, timezone) %>%
    dplyr::summarize(city_geometry=sf::st_centroid(sf::st_union(geometry))) %>%
    ungroup()

  # Get hourly solar radiation at each day and hour
  locs_sunshine <- locs %>% tidyr::crossing(doy_mst=seq(1:365)) %>%
    mutate(longitude=purrr::map_dbl(city_geometry, ~sf::st_coordinates(.x)[[1]])) %>%
    mutate(latitude=purrr::map_dbl(city_geometry, ~sf::st_coordinates(.x)[[2]])) %>%
    mutate(sunshine=purrr::map2(doy_mst, latitude, ~ tibble(hour_mst=seq(0:23), sunshine=sirad::extrat(.x, sirad::radians(.y))$ExtraTerrestrialSolarRadiationHourly))) %>%
    tidyr::unnest(sunshine)


  # Convert Mean Solar Time (MST) to local time
  # sirad::extrat gives results in Mean Solar Time
  # since local time never perfectly matches MST, we need to add an offset
  mst_to_local_datetime <- function(doy_mst, hour_mst, timezone, longitude){

    # solartime needs to get the timezone in number of hours offset
    hour_local_vs_utc <- function(doy_mst, timezone){
      td <- lubridate::force_tz(strptime(paste(doy_mst,10,sep="-"),"%j-%H"),tzone='UTC') -
        lubridate::force_tz(strptime(paste(doy_mst,10,sep="-"),"%j-%H"),tzone=timezone)
      return(as.double(td, units='hours'))
    }
    # solar_to_local_hour = time difference in hours to be added to local winter time to get solar time
    solar_to_local_hour <- solartime::computeSolarToLocalTimeDifference(longitude,
                                                 hour_local_vs_utc(doy_mst, timezone),
                                                 doy_mst)
    if(abs(solar_to_local_hour)>3){
      warning("There seem to be some issue with the timezone. Difference between local and solar time is too high")
    }

    dttm_mst <- lubridate::force_tz(strptime(paste(doy_mst,hour_mst,sep="-"),"%j-%H"), tzone=timezone)
    dttm_local <- dttm_mst - lubridate::dhours(solar_to_local_hour)
    return(as.POSIXct(dttm_local))
  }


  locs_sunshine <- locs_sunshine %>% dplyr::rowwise() %>%
    dplyr::mutate(dttm_local=mst_to_local_datetime(doy_mst, hour_mst, timezone, longitude)) %>%
    dplyr::mutate(doy=purrr::map_dbl(dttm_local,lubridate::yday),
           hour=purrr::map_dbl(dttm_local,lubridate::hour))

  # Merge with measurements
  meas_sunshine <- meas %>%
    mutate(doy=purrr::map_dbl(date,lubridate::yday),
                  hour=purrr::map_dbl(date,lubridate::hour)) %>%
    left_join(locs_sunshine %>% select(city, doy, hour, sunshine)) %>%
    select(-c(doy, hour))

  return(meas_sunshine)
}
