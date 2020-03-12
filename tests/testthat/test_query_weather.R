library(testthat)
library(lubridate)
library(DBI)


# Querying weather
test_that("Querying ISD weather works", {

  con = connection()
  meas_test <- measurements(city='Delhi',
                      poll=PM25,
                      date_from='2019-07-01',
                      date_to='2019-07-02',
                      average_by = 'hour',
                      aggregate_at_city_level = T,
                      add_noaa_station_ids = T,
                      noaa_station_radius_km = 20,
                      collect = F,
                      con = con)

  # Version using NOAA data stored in CREA DB
  t <- system.time({
    result_db <- weather.isd.join(meas_test,
                                     measurements_averaged_by = 'hour',
                                     collect = T,
                                     con = con)
  })

  expect_lt(t['elapsed'],30)
  expect_gt(length(unique(result_db$wind_deg)), 1)
  expect_gt(length(unique(result_db$rh_percent)), 1)
  expect_gt(length(unique(result_db$prec_6h_mm)), 1)
  expect_gt(length(unique(result_db$slp_hp)), 1)
  expect_gt(length(unique(result_db$temp_c)), 1)
  expect_gt(length(unique(result_db$sky_code)), 1)


  # Version using NOAA data got from Worldmet package
  t <- system.time({
    result_worldmet <- weather.isd.join.using_worldmet(meas_test,
                                  measurements_averaged_by = 'hour',
                                  radius_km = 20)
  })

  expect_lt(t['elapsed'],300)
  expect_gt(length(unique(result_db$atmos_pres)), 1)
  expect_gt(length(unique(result_db$RH)), 1)
  expect_gt(length(unique(result_db$air_temp)), 1)
  # expect_gt(length(unique(result_db$precip)), 1)
  expect_gt(length(unique(result_db$ceil_ght)), 1)

  # For timezone confirmation: we check on a single day with manually fetched data
  # Station 421820
  # Time is UTC in this data
  # 2019 06 30 13   390   230 -9999   350    21 -9999 -9999 -9999
  # 2019 06 30 15   374   220  9945   320    10     5 -9999 -9999
  # 2019 06 30 18   346   210  9964    20    10     5 -9999 -9999
  # 2019 06 30 21   332   203  9962   250    31     4 -9999 -9999
  # 2019 07 01 00   316   226  9973   270    10     2 -9999 -9999
  # 2019 07 01 02   330   220 -9999   220    21     0 -9999 -9999
  # 2019 07 01 03   338   228  9986   200    10     0 -9999 -9999 <-- PEAK PRESSURE AT 3AM UTC -> 8.30AM IST
  # 2019 07 01 04   350   250 -9999   230    26     0 -9999 -9999
  # 2019 07 01 05   370   240 -9999    50    10     0 -9999 -9999
  # 2019 07 01 06   386   229  9972   270    21     0 -9999 -9999
  # 2019 07 01 07   390   230 -9999    50    31     0 -9999 -9999
  # 2019 07 01 08   390   220 -9999    60    15     0 -9999 -9999
  # 2019 07 01 09   410   235  9948    70    21     2 -9999 -9999 <-- PEAK TEMP AT 9AM UTC -> 2.30PM IST
  # 2019 07 01 10   400   220 -9999    50    31 -9999 -9999 -9999
  # 2019 07 01 11   410   240 -9999    40    36     0 -9999 -9999 <-- PEAK TEMP AT 11AM UTC -> 4.30PM IST
  # 2019 07 01 12   404   238  9920    50    31     0 -9999 -9999
  # 2019 07 01 13   390   260 -9999    40    10 -9999 -9999 -9999
  # 2019 07 01 15   368   220  9945   230    36     6 -9999 -9999
  # 2019 07 01 18   342   219  9965     0     0     5 -9999 -9999
  # 2019 07 01 21   320   220  9968   270    31     4 -9999 -9999

  official_dates <- lubridate::force_tz(lubridate::ymd_h(c("2019-06-30 15",
                                                          "2019-06-30 18",
                                                          "2019-06-30 21",
                                                          "2019-07-01 00",
                                                          "2019-07-01 03",
                                                          "2019-07-01 06",
                                                          "2019-07-01 09",
                                                          "2019-07-01 12",
                                                          "2019-07-01 15",
                                                          "2019-07-01 18"
                                                          )),tzone="UTC")
  official_temp <- c(37.4, 34.6, 33.2, 31.6, 33.8, 38.6, 41, 40.4, 36.8, 34.2)
  official_pres <- c(994.5, 996.4, 996.2, 997.3, 998.6, 997.2, 994.8, 992.0, 994.5, 996.5)

  result_official <- tibble(date=official_dates, air_temp=official_temp, atmos_pres=official_pres, source='official')


  expect_equal(attr(result_worldmet$date[[1]],'tzone'),'Asia/Kolkata')
  expect_equal(attr(result_db$date[[1]],'tzone'),'Asia/Kolkata')

  date_max_temp_worldmet <- (result_worldmet %>% arrange(desc(air_temp)) %>% slice(1))$date
  date_max_temp_db <- (result_db %>% arrange(desc(temp_c)) %>% slice(1))$date
  expect_equal(date_max_temp_worldmet, date_max_temp_db)
  expect_equal(date_max_temp_worldmet, as.POSIXct("2019-07-01 14:00:00",tz='Asia/Kolkata'))

  # Compare two sources
  plot_data <- bind_rows(
    result_official,
    result_db %>% select(date, precip=prec_6h_mm, air_temp=temp_c, atmos_pres=slp_hp) %>% mutate(source='db'),
    result_worldmet %>% select(date, precip, air_temp, atmos_pres) %>% mutate(source='worldmet')
    ) %>% tidyr::gather("variable", "value", -c(date, source))

  ggplot(plot_data %>% filter(date>='2019-07-01',date<='2019-07-02'), aes(x=date, y=value, colour=source)) + facet_wrap(~variable, scales="free_y") + geom_point(data=plot_data %>% filter(source!='official')) + geom_line(data=plot_data %>% filter(source=='official'))

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

  delhi_gfs <- meteoForecast::getPointDays(c(28.7041, 77.1025), vars = "swflx", start = '2017-01-01', end='2019-12-31', service = 'gfs')

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
