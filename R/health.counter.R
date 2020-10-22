

# #read input data
gemm_RRs <- readr::read_csv(system.file("extdata", "gemm_risk_tables.csv", package = "rcrea"))
other_RRs <- readr::read_csv(system.file("extdata", "CRFs.csv", package = "rcrea"))
vals <- readr::read_csv(system.file("extdata", "valuation.csv", package = "rcrea"))
epi <- readr::read_csv(system.file("extdata", "epi.csv", package = "rcrea"))
GDPconv <- readr::read_csv(system.file("extdata", "GDP.csv", package = "rcrea"))

#correct an error in an earlier version of CRFs.csv with an incorrect entry for Exposure
other_RRs$Exposure[other_RRs$Exposure=='o3.ff'] <- 'O3_8h'

#read names of causes for which GEMM risk functions should be used
gemm_causes <- unique(gemm_RRs$Cause)

#read names of outcomes to be calculated using GEMM functions
gemm_outcomes <- epi %>% dplyr::select_if(is.numeric) %>% names %>% grep('_', ., value=T)

names(other_RRs) <- gsub("RR_", "", names(other_RRs))
estimates <- c('low', 'central', 'high')

#read global GDP per capita for calculations
world.gdp <- GDPconv$GDP.PPP.2011USD[GDPconv$ISO3=='WLD']


#function to return the risk ratio for any outcome and vector of concentrations
#NB all concentration data has to be given as ug/m3!
#(or the Units.multiplier column in CRFs.csv file must be modified accordingly)

get.RR <- function(.concentration, .pollutant, .outcome) {

  .cause <- gsub("_.*", "", .outcome)
  rr.out <- data.frame(conc=.concentration)

  #PM2.5 death risk ratios from GEMM risk model
  if(.cause %in% gemm_causes & .pollutant == 'PM2.5') {
    gemm_RRs %>% dplyr::filter(Cause == .cause) -> rr.in

    for(estimate in estimates)
      approx(rr.in$PM2.5, rr.in[[estimate]], .concentration)$y -> rr.out[[estimate]]

  } else {
    other_RRs %>% dplyr::filter(Incidence == .outcome, Exposure == .pollutant) -> rr.in
    x = pmax(0, .concentration * rr.in$Units.multiplier - rr.in$Counterfact) / rr.in$Conc.change

    for(estimate in estimates)
      exp(log(rr.in[[estimate]]) * x) -> rr.out[[estimate]]
  }
  rr.out %>% dplyr::select(-conc) %>% return()
}


#function to calculate health impacts and costs for a list of cities
#input parameter is a data.frame with the columns: city_name, ISO3, population, PM2.5, NO2, O3_8h
get.costs <- function(cities) {

  ### build a data.frame with all input data

  #add city-specific data
  cities %>% dplyr::inner_join(epi) -> epi.in

  #add national data for cities without data
  epi %>% dplyr::filter(city_name=='National') %>%
    dplyr::select(-city_name) -> epi.country

  cities %>% dplyr::filter(!(city_name %in% epi.in$city_name)) %>%
    dplyr::inner_join(epi.country) %>%
    dplyr::bind_rows(epi.in) -> epi.in

  ### calculate health impacts
  #data.frame to hold results
  hia <- epi.in %>% dplyr::select(ISO3, city_name)

  #loop through outcomes calculated with GEMM functions
  for(outcome in gemm_outcomes) {
    get.RR(.concentration=epi.in$PM2.5, .pollutant='PM2.5', outcome) -> rr
    (1 - 1/rr) * epi.in[[outcome]] / 1e5 * epi.in$population -> cases
    cases %>% purrr::set_names(paste0(outcome, '..PM2.5..', names(.))) %>% dplyr::bind_cols(hia, .) -> hia
  }

  #loop through other outcomes
  for(i in 1:nrow(other_RRs)) {
    pollutant = other_RRs$Exposure[i]
    outcome = other_RRs$Incidence[i]

    get.RR(.concentration=epi.in[[pollutant]],
          .pollutant=pollutant,
          outcome) -> rr

    (1 - 1/rr) * epi.in[[outcome]] / 1e5 * epi.in$population -> cases
    cases %>% purrr::set_names(paste0(outcome, '..', pollutant, '..', names(.))) %>% dplyr::bind_cols(hia, .) -> hia
  }

  #convert into narrow form
  hia %>% tidyr::gather(Outcome, number, contains('..')) %>%
    tidyr::separate(Outcome, c('Outcome', 'Pollutant', 'estimate'), '\\.\\.') %>%
    tidyr::separate(Outcome, c('Cause', 'Outcome'), '_') %>%
    dplyr::mutate(Outcome = ifelse(is.na(Outcome), Cause, Outcome)) ->
    hia2

  #add valuation and GDP data
  hia2$Outcome.Valuation <- hia2$Outcome %>% gsub('\\.[0-9a-z]+', '', .)
  hia2 %<>% dplyr::left_join(vals %>% dplyr::rename(Outcome.Valuation = Outcome)) %>%
    dplyr::left_join(GDPconv) %>% dplyr::select(-Outcome.Valuation)

  #calculate costs in PPP-adjusted dollars
  hia2$cost.IntlDollars <- hia2$number * hia2$Valuation.2011.IntlDollars *
    (hia2$GDP.PPP.2011USD / world.gdp)^hia2$Elasticity

  #convert to current USD and local currency
  hia2$cost.USD = hia2$cost.IntlDollars * hia2$GDP.currUSD / hia2$GDP.PPP.2011USD
  hia2$cost.LCU = hia2$cost.IntlDollars * hia2$GDP.currLCU / hia2$GDP.PPP.2011USD

  #massage data into output form
  hia2 %>% dplyr::select(-cost.IntlDollars) %>%
    tidyr::gather(var, val, number, cost.USD, cost.LCU) %>%
    tidyr::unite(var, var, estimate) %>%
    tidyr::spread(var, val) %>%
    dplyr::left_join(epi %>% dplyr::select(ISO3, country, population) %>% dplyr::distinct()) %>% #add country names and population
    dplyr::select(city_name, ISO3, country, population, Cause, Outcome, Pollutant, tidyselect::starts_with('number'), tidyselect::starts_with('cost'))

}


#get costs for three different periods: past 24 hours, year-to-date and full_year
main <- function(city_name, ISO3, population,
                 PM2.5, NO2, O3_8h, #one-year (365-day) rolling average concentrations
                 PM2.5_ytd, NO2_ytd, O3_8h_ytd, #year-to-date average concentrations
                 PM2.5_24h, NO2_24h, O3_8h_24h, #past 24-hour average concentrations
                 days_to_date=NULL, #number of days included in the year-to-date concentration data; if NULL, assumes until yesterday
                 return.list=F){ #should the results be returned in a list, with results for different periods as list elements (alternative is in a data frame, with column Period separating the results for the three periods)

  cities=data.frame(city_name = city_name,
                    ISO3 = ISO3,
                    population = population,
                    PM2.5 = PM2.5,
                    NO2 = NO2,
                    O3_8h = O3_8h)

  #get health impacts and costs for the past 365 days, using one-year-average concentrations
  costs <- get.costs(cities)

  #if number of days in YTD data not given, compute days since start of year until yesterday
  if(is.null(days_to_date)) {
    first_of_year = Sys.Date() %>% substr(1, 4) %>% paste0("-01-01") %>% as.Date
    days_to_date = as.numeric(Sys.Date() - first_of_year)
  }

  #data.frame used to calculate 24-hour and YTD data from one-year results
  cities %>% dplyr::select(city_name, ISO3) -> multipliers

  #calculate percentage of total exposure in past 365 days taking place since start of year
  multipliers$PM2.5..ytd <- PM2.5_ytd * (days_to_date / 365 / PM2.5)
  multipliers$NO2..ytd <- NO2_ytd * (days_to_date / 365 / NO2)
  multipliers$O3_8h..ytd <- O3_8h_ytd * (days_to_date / 365 / O3_8h)

  #calculate percentage of total exposure in past 365 days taking place during past 24 hours
  multipliers$PM2.5..24h <- PM2.5_24h / (365 * cities$PM2.5)
  multipliers$NO2..24h <- NO2_24h / (365 * cities$NO2)
  multipliers$O3_8h..24h <- O3_8h_24h / (365 * cities$O3_8h)

  #add the multipliers to the costs data.frame
  multipliers %>% tidyr::gather(Pollutant, mult, -ISO3, -city_name) %>%
    tidyr::separate(Pollutant, c('Pollutant', 'Period'), '\\.\\.') %>%
    dplyr::full_join(costs, .) -> costs.periods

  #do the multiplication, remove the multiplier column, return data
  costs.periods %<>%
    dplyr::mutate_if(is.numeric, multiply_by, costs.periods$mult) %>%
    dplyr::select(-mult) %>% dplyr::arrange(Period) %>%
    dplyr::bind_rows(costs %>% dplyr::mutate(Period='full_year'))

  if(return.list) costs.periods %<>% split.data.frame(costs.periods$Period)

  return(costs.periods)
}
