
violations.guidelines <- function(){
  jsonlite::fromJSON(system.file("extdata", "standards.json", package = "rcrea")) %>%
    mutate(standard_id=row_number())

}


violations <- function(source, city,
                       date_from, date_to,
                       frequency=c("hour","day","year"),
                       level="city",
                       country=NULL,
                       location_type=NULL,
                       poll=NULL,
                       orgs=c("EU", "WHO")){

  l <- rcrea::locations(source=source,
                        country=country,
                        city=city,
                        level=level,
                        with_metadata = T)

  m.hour <- rcrea::measurements(source=source,
                                country=country,
                                aggregate_level = level,
                                city=city,
                                location_type=location_type,
                                average_by = "hour",
                                poll=poll,
                                date_from=date_from,
                                date_to=date_to) %>%
    mutate(frequency="1-hour",
           aggregation_function="mean")

  m.8hour <- rcrea::utils.running_average(m.hour,
                                       average_width = 8,
                                       average_by = "hour") %>%
    mutate(frequency="8-hour",
           aggregation_function="mean")

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
           aggregation_function="mean")

  m.year <- m.day %>%
    mutate(date=lubridate::floor_date(date, unit='year')) %>%
    group_by_at(setdiff(names(.), "date")) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(frequency="year",
           aggregation_function="mean")


  guidelines <- violations.guidelines()

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

  count_violations <- function(group){
    group %>%
      filter(value > threshold) %>%
      nrow()
  }

  m.violations <- m %>%
    group_by(location_id, process_id, poll, unit, source,
             year=lubridate::year(date), frequency,
             standard_id, exceedance_allowed_per_year) %>%
    summarise(n_violations=count_violations(dplyr::cur_data())) %>%
    ungroup()

  m.violations %>% left_join(l)

}

