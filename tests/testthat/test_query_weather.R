library(testthat)
library(lubridate)
library(DBI)


# Querying weather
test_that("Querying ISD weather works", {

  meas<- m_measurements(city='Delhi',
                      poll=PM25,
                      date_from='2019-01-01',
                      average_by = 'hour',
                      aggregate_at_city_level = T,
                      add_noaa_station_ids = T,
                      noaa_station_radius_km = 20,
                      collect = F)


  t <- system.time({
    result <- weather.isd.join(meas,
                                     measurements_averaged_by = 'hour',
                                     collect = T)
  })

  expect_lt(t['elapsed'],30)
  expect_gt(length(unique(result$wind_deg)), 1)
  expect_gt(length(unique(result$rh_percent)), 1)
  expect_gt(length(unique(result$prec_6h_mm)), 1)
  expect_gt(length(unique(result$slp_hp)), 1)
  expect_gt(length(unique(result$temp_c)), 1)
  expect_gt(length(unique(result$sky_code)), 1)
})

# Querying weather
test_that("Querying GHCND precipitation works", {

  city <- c('Delhi','Kolkata')
  meas<- measurements(city=city,
                      poll=PM25,
                      date_from='2019-01-01',
                      average_by = 'hour',
                      aggregate_at_city_level = T,
                      collect = F)


  t <- system.time({
    result <- weather.ghcnd.join(meas)
  })

  expect_lt(t['elapsed'],300)
  expect_gt(length(unique(result$prcp)), 1)
  expect_equal(length(unique(result$city)), length(city))

  # Test values are different across cities
  mean <- unique((result %>% group_by(city) %>% summarize(mean=mean(prcp, na.rm=T)))$mean)
  expect_equal(length(mean),length(city))
  expect_false(anyNA(mean))

})

# Precipitation queries
test_that("Various precipitation data sources", {

  require(worldmet)
  delhi_ids <-worldmet::importNOAA(
    code = "421820-99999",
    year = 2017:2019,
    hourly = TRUE,
    precip = TRUE,
    PWC = FALSE,
    parallel = TRUE,
    quiet = FALSE,
    path = NA
  )

  # https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:IN022023000/detail
  delhi_ghcnd <- meteo_tidy_ghcnd(stationid='IN022023000',
                                       keep_flags = FALSE,
                                       var = "all",
                                       date_min = '2017-01-01',
                                       date_max = '2019-12-31'
                                  )

  delhi_gfs <- getPoint(c(28.7041, 77.1025), vars = "swflx", start = '2017-01-01', end='2019-12-31', service = 'gfs')

  plot_data <- bind_rows(delhi_ids %>% select(date, prcp=precip_12) %>% mutate(source='ids'),
            delhi_ghcnd %>% select(date, prcp) %>% mutate(source='ghcnd') %>% mutate(date=lubridate::as_datetime(date))
            )

  ggplot(plot_data,aes(x=date,y=prcp,colour=source)) + geom_point()

})


# AQ Weather
test_that("AQ Weather collecting measurements with weather works", {

  t <- system.time({result <- aq_weather.collect(city=c('Delhi','Kolkata'),
                                                 poll=PM25,
                                                 date_from='2019-01-01',
                                                 average_by = 'hour',
                                                 aggregate_at_city_level = T,
                                                 collect=T)})

  expect_lt(t['elapsed'],30)
  expect_gt(length(unique(result$wind_deg)), 1)
  expect_gt(length(unique(result$rh_percent)), 1)
  expect_gt(length(unique(result$prec_6h_mm)), 1)
  expect_gt(length(unique(result$slp_hp)), 1)
  expect_gt(length(unique(result$temp_c)), 1)
  expect_gt(length(unique(result$sky_code)), 1)


  # Add precipitation data from

})
