

health.build.scenarios <- function(m,
                                   process_anomaly="anomaly_gbm_lag1_city_mad",
                                   process_observation="city_day_mad"){

  m.observation <- m %>%
    dplyr::filter(process_id==process_observation) %>%
    dplyr::mutate(scenario="observation") %>%
    dplyr::select(poll, unit, date, source, location_id, value)

  m.full <- m %>%
    dplyr::filter(process_id==process_anomaly) %>%
    dplyr::mutate(unit=stringr::str_replace(unit, paste0(utils.Delta()," "), "")) %>%
    dplyr::left_join(m.observation,
              by=c("poll","unit","date","source","location_id"),
              suffix=c(".anomaly", ".observation")
              ) %>%
    dplyr::mutate(value.counterfactual=value.observation-value.anomaly)

}


health.impact <- function(meas, date_from="2020-01-01", date_to="2020-12-31"){

  required_cols <- c("value.counterfactual","value.observation", "country", "geometry", "pop")
  polls <- c("NO2" = "no2", "PM2.5"="pm25", "PM10"="pm10", "O3" ="o3", "O3_8h"="o3_8h")

  if(length(setdiff(c(required_cols), names(meas)))){
    stop(paste("Missing columns:", paste(setdiff(c(required_cols), names(meas)), collapse=",")))
  }

  na_pop <- unique(meas$location_id[is.na(meas$pop)])
  if(length(na_pop)){
    warning(paste0("Some cities do not have a population number: ", paste(na_pop, collapse=","),
                  ". They will be excluded from calculations."))
    meas <- meas[!is.na(meas$pop),]

  }

  # Capitalize city names
  # meas$location_id <- tools::toTitleCase(meas$location_id)

  # ISO2 -> ISO3
  meas$iso3 <- countrycode::countrycode(sourcevar = meas$country, origin="iso2c", destination="iso3c")

  meas <- meas %>% dplyr::mutate(longitude = purrr::map_dbl(sf::st_centroid(geometry), ~sf::st_coordinates(.x)[[1]]),
                                 latitude =  purrr::map_dbl(sf::st_centroid(geometry), ~sf::st_coordinates(.x)[[2]]))


  # Calculate average concentrations and anomalies during COVID

  # ppm to µg/m3
  unit_factor <- data.frame(unit="ppm", poll=c("no2","o3"), unit_factor=c(1.88e3, 1.96e3))

  m.mean <- meas %>%
    # dplyr::filter(date > '2020-03-11' | (iso3 == 'CHN' & date > '2020-01-23')) %>%
    dplyr::filter(date >= date_from,
           date <= date_to) %>%
    dplyr::group_by(poll, location_id, unit, iso3, latitude, longitude, pop) %>%
    dplyr::summarise(
      year.ratio=as.numeric(max(date)-min(date))/365,
      value.observation=mean(value.observation, na.rm=T),
      value.counterfactual=mean(value.counterfactual, na.rm=T)) %>%
    dplyr::ungroup() %>%
    #  Average anomalies on year
    dplyr::mutate(value.observation=value.counterfactual+(value.observation-value.counterfactual)*year.ratio) %>%
    dplyr::select(-c(year.ratio)) %>%
    # mutate(avg_2020 = avg_2019 + anomaly * days/365) %>%
    tidyr::pivot_longer(cols=c(value.observation, value.counterfactual),
                        names_to="scenario", names_prefix="value.", values_to="value") %>%
    dplyr::left_join(unit_factor) %>%
    dplyr::mutate(value = value * ifelse(is.na(unit_factor), 1, unit_factor)) %>%
    dplyr::select(-unit, -unit_factor) %>%
    tidyr::spread(poll, value) %>%
    dplyr::left_join(tibble::tibble(no2=NA, pm25=NA, pm10=NA, o3=NA)) %>%
    dplyr::mutate(pm25 = ifelse(is.na(pm25), pm10*.7, pm25)) %>%
    dplyr::rename(all_of(polls)) %>%
    dplyr::select(scenario, city_name=location_id, ISO3=iso3, population=pop,
        names(polls))


  #convert O3 mean to O3 MDA8
  if(! "O3_8h" %in% names(m.mean)) m.mean$O3_8h = m.mean$O3 * 1.2

  #calculate results
  # source(file.path("R", "health", 'counter.R'))

  costs <- m.mean %>% plyr::ddply(plyr::.(scenario), get.costs) %>% dplyr::distinct()

  chgs <- costs %>% dplyr::rename(city = city_name) %>% tidyr::gather(var, val, contains('_')) %>%
    tidyr::spread(scenario, val) %>% dplyr::mutate(avoided = counterfactual - observation)

  #filter out entries not included in totals
  chgs <- chgs %>% dplyr::filter(!(Outcome == 'Deaths' & Cause=='COPD' & Pollutant == 'PM2.5'), Cause != 'LBW') %>%
    dplyr::filter(!(Outcome=='YLLs' & grepl('number', var)),
           !(grepl('Asthma.Prev', Outcome) & grepl('number', var))) %>%
    dplyr::mutate(Outcome = dplyr::recode(Outcome, Asthma.Prev.1to18='Asthma.Inci.1to18', YLLs='Deaths'))


  # write_csv(health_summary, file.path("results","data","health_summary.csv"))

  #add labels in human language
  readr::read_csv(system.file("extdata", "dict.csv", package = "rcrea")) -> dict

  chgs <- chgs %>% dplyr::rename(Outcome_short = Outcome, Cause_short = Cause) %>%
    dplyr::left_join(dict %>% dplyr::rename(Cause_short = Code, Cause = Long.name)) %>%
    dplyr::left_join(dict %>% dplyr::rename(Outcome_short = Code, Outcome = Long.name)) %>%
    dplyr::select(-contains('_short')) %>%
    dplyr::mutate(Outcome = dplyr::recode(Outcome, 'years lived with disability'='disability'))

  #print breakdown of impacts by outcome for all cities
  health_details <- chgs %>%
    dplyr::filter(var %in% c('number_central', 'cost.USD_central')) %>%
    dplyr::group_by(location_id=city, population, Outcome, Cause, Pollutant, var) %>%
    dplyr::summarise_at('avoided', sum, na.rm=T) %>%
    tidyr::spread(var, avoided) %>%
    dplyr::mutate(cost.mlnUSD = cost.USD_central/1e6) %>%
    dplyr::select(-cost.USD_central)


 return(health_details)
}

health.simplify <- function(health_details){
  #print out headline numbers by city
  health_details %>%
    dplyr::mutate(deaths=ifelse(Outcome=="deaths", number_central, 0)) %>%
    dplyr::group_by(location_id, population) %>%
    dplyr::summarise_at(c("cost.mlnUSD","deaths"), sum, na.rm=T) %>%
    dplyr::mutate(cost.mlnUSD.perpax=cost.mlnUSD/population,
           deaths.perpax=deaths/population)
}
