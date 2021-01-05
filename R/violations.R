
violations.guidelines <- function(){
  jsonlite::fromJSON(system.file("extdata", "standards.json", package = "rcrea")) %>%
    mutate(standard_id=row_number())

}


violations <- function(source, city,
                       date_from, date_to,
                       frequency=c("hour","day","year"),
                       country=NULL,
                       location_type=NULL,
                       poll=NULL,
                       orgs=c("EU", "WHO")){

  m.hour <- rcrea::measurements(source=source,
                                country=country,
                                city=city,
                                location_type=location_type,
                                average_by = "hour",
                                poll=poll,
                                date_from=date_from,
                                date_to=date_to) %>%
    mutate(frequency="1-hour")

  m.day <- rcrea::measurements(source=source,
                                country=country,
                                city=city,
                                location_type=location_type,
                                average_by = "day",
                                poll=poll,
                                date_from=date_from,
                                date_to=date_to) %>%
    mutate(frequency="24-hour")

  m.year <- m.day %>%
    mutate(date=lubridate::floor_date(date, unit='year')) %>%
    group_by_at(setdiff(names(.), "date")) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(frequency="year")


  guidelines <- violations.guidelines()

  m <- bind_rows(m.hour, m.day, m.year) %>%
    dplyr::inner_join(guidelines %>%
                select(poll=pollutant,
                       threshold, unit, exceedance_allowed_per_year,
                       frequency=aggregation_period,
                       aggregation_function,
                       organization, standard_id))

  count_violations <- function(group){
    group %>%
      filter(value > threshold) %>%
      nrow()
  }

  m %>%
    group_by(location_id, process_id, poll, unit, source,
             year=lubridate::year(date), frequency,
             standard_id, exceedance_allowed_per_year) %>%
    summarise(n_violations=count_violations(dplyr::cur_data()))

}

