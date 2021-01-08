
violations.guidelines <- function(){
  jsonlite::fromJSON(system.file("extdata", "standards.json", package = "rcrea")) %>%
    mutate(standard_id=row_number())

}


violations <- function(source,
                       date_from,
                       date_to,
                       city=NULL,
                       level="city",
                       country=NULL,
                       location_type=NULL,
                       poll=NULL,
                       orgs=c("EU", "WHO")){

  # Locations
  l <- rcrea::locations(source=source,
                        country=country,
                        city=city,
                        level=level,
                        with_metadata=T)

  # Standards / Guidelines from json file
  guidelines <- violations.guidelines() %>% filter(organization %in% !!orgs)

  # Hourly measurements
  # This takes quite some time. Trying to limit to required pollutants
  poll.hour <- guidelines %>%
    dplyr::filter(stringr::str_detect(aggregation_period,'hour')) %>%
    distinct(pollutant) %>% pull()

  if(!is.null(poll)){
    poll.hour %<>% intersect(poll)
  }

  print("1/5 - Getting hourly data")
  m.hour <- rcrea::measurements(source=source,
                                country=country,
                                aggregate_level = level,
                                city=city,
                                location_type=location_type,
                                average_by = "hour",
                                poll=poll.hour,
                                date_from=date_from,
                                date_to=date_to) %>%
    mutate(frequency="1-hour",
           aggregation_function="mean") %>%
    filter(!is.na(date)) # TODO Check why some dates are NA

  # 8-hour running average (mainly for O3)
  print("2/5 - Rolling average to 8-hours")
  m.8hour <- rcrea::utils.running_average(m.hour,
                                       average_width = 8,
                                       average_by = "hour") %>%
    mutate(frequency="8-hour",
           aggregation_function="mean")

  # Daily averages
  print("3/5 - Getting daily data")
  m.day <- rcrea::measurements(source=source,
                                country=country,
                                city=city,
                                aggregate_level = level,
                                location_type=location_type,
                                average_by = "day",
                                poll=poll,
                                date_from=date_from,
                                date_to=date_to) %>%
    mutate(frequency="24-hour",
           aggregation_function="mean") %>%
    filter(!is.na(date)) # TODO Check why some dates are NA

  # Yearly averages (from daily ones)
  print("4/5 - Aggregating by year")
  m.year <- m.day %>%
    mutate(date=lubridate::floor_date(date, unit='year')) %>%
    group_by_at(setdiff(names(.), "date")) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(frequency="year",
           aggregation_function="mean")




  # Joining altogether
  print("5/5 - Comparing with guidelines")
  m <- bind_rows(m.hour, m.8hour, m.day, m.year) %>%
    filter(!is.na(date)) %>%
    dplyr::inner_join(guidelines %>%
                select(poll=pollutant,
                       aggregation_function,
                       threshold, unit, exceedance_allowed_per_year,
                       frequency=aggregation_period,
                       aggregation_function,
                       organization, standard_id),
                by=c("poll","unit","frequency","aggregation_function"))


  # Keeping violations only
  m.violations <- m %>%
    mutate(exceedance=value>threshold) %>%
    filter(exceedance) %>%
    # One max per day / per standard only
    group_by(location_id, location_name, process_id, date=lubridate::date(date),
             poll, unit, source, country, frequency, aggregation_function, threshold,
             exceedance_allowed_per_year, organization, standard_id) %>%
    summarise(value=max(value, na.rm=T),
              exceedance=max(exceedance, na.rm=T)) %>%
    group_by(year=lubridate::year(date), location_id, standard_id) %>%
    arrange(date) %>%
    mutate(exceedance_this_year=row_number()) %>%
    mutate(violation=exceedance_this_year>exceedance_allowed_per_year)


  meta_cols <- if(level=="city") c("location_id"="id", "gadm1_id") else c("location_id"="id", "city_id", "city_name", "gadm1_id")

  m.violations %>%
           left_join(l %>% select_at(meta_cols)) %>%
    ungroup()
}

