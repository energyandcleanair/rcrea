library(testthat)
library(lubridate)
library(DBI)

# Testing philosophy
# To save time on regular testings, we use 'random' pollutants and rather late date_from
# Indeed, query time is almost proportional to number of rows returned

test_that("reconnection works", {

  con <- connection()
  expect_true(DBI::dbIsValid(con), "Invalid connection")
  meas1 <- measurements(city='Delhi', date_from="2020-01-01", collect=F)

  # Test whether reconnection works automatically and is shared across queries
  DBI::dbDisconnect(con)
  expect_false(DBI::dbIsValid(con), "Connection should be invalid")
  meas2 <- measurements(location_id='IN-107', poll=creadb::CO, date_from="2020-01-01", collect=T) # Queries first locations and then measurements
  expect_gt(nrow(meas2),0)
})
#
# test_that("RPostgres faster than RPostgresQL", {
#
#   library(RPostgres)
#   library(RPostgreSQL)
#
#   con_postgres <- DBI::dbConnect(RPostgres::Postgres(), dbname = CONN_DBNAME,
#                                     host = CONN_HOST,
#                                     port = strtoi(CONN_PORT),
#                                     user = CONN_USER,
#                                     password = CONN_PASSWORD)
#
#   time_postgres <- system.time(measurements(con=con_postgres, date_from='2019-01-01', city='Delhi', poll=creadb::CO, collect=T))
#   DBI::dbDisconnect(con_postgres)
#
#   con_postgresql <- DBI::dbConnect(RPostgres::Postgres(), dbname = CONN_DBNAME,
#                                  host = CONN_HOST,
#                                  port = strtoi(CONN_PORT),
#                                  user = CONN_USER,
#                                  password = CONN_PASSWORD)
#
#   time_postgresql <- system.time(measurements(con=con_postgresql, date_from='2019-01-01', city='Delhi', poll=creadb::CO, collect=T))
#   DBI::dbDisconnect(con_postgresql)
#
#   expect_lt(time_postgres['elapsed'], time_postgresql['elapsed'])
#
# })

test_that("query return locations", {

  browser() # For debug

  locs_unknown <- locations(country='XXX')
  expect_equal(nrow(locs_unknown), 0)

  locs_india <- locations(country='IN')
  expect_gt(nrow(locs_india), 0)

  locs_delhi <- locations(city='Delhi')
  expect_gt(nrow(locs_delhi), 0)

  locs_delhi_china <- locations(country='CN', city='Delhi')
  expect_equal(nrow(locs_delhi_china), 0)

})

test_that("query return targets", {

  browser() # For debug

  targets_unkown <- targets(country='XXX')
  expect_equal(nrow(targets_unkown), 0)


  targets_india <- targets(country='IN')
  expect_gt(nrow(targets_india), 0)

  targets_delhi <- targets(city='Delhi')
  expect_gt(nrow(targets_delhi), 0)

})

test_that("query return measurements", {

  browser() # For debug

  # Filtering
  meas_unknown <- measurements(city='XXX')
  expect_equal(nrow(meas_unknown), 0)

  meas_unkown <- measurements(city='Mumbai', user_filter=function(x){x %>% dplyr::filter(avg_day<=1000 | poll==CO)})

  meas_delhi <- measurements(city='Delhi', poll=creadb::CO, date_from='2019-01-01')
  expect_gt(nrow(meas_delhi), 0)
  expect_equal(tolower(unique(meas_delhi$city)), 'delhi')
  expect_gt(length(unique(meas_delhi$location)), 0)
  expect_gt(length(unique(meas_delhi$poll)), 0)

  meas_delhi_lower <- measurements(city='delhi', poll=creadb::CO, date_from='2019-01-01')
  expect_equal(nrow(meas_delhi_lower), nrow(meas_delhi))

  meas_delhi_jaipur <- measurements(city=c('Delhi','Jaipur'), poll=creadb::CO, date_from='2019-01-01')
  expect_gt(nrow(meas_delhi_jaipur), nrow(meas_delhi))

  meas_delhi_china <- measurements(country='CN', city='Delhi')
  expect_equal(nrow(meas_delhi_china), 0)

  # Location id
  meas_delhi <- measurements(city='Delhi', poll=creadb::CO, date_from='2020-01-01', average_by='year', keep_location_id = T)
  length(unique(meas_delhi$location_id)) >= 23 # 23 stations with CO data  in Delhi at the time of writing
  meas_delhi <- measurements(city='Delhi', poll=creadb::CO, date_from='2020-01-01', average_by='year', keep_location_id = F)
  length(unique(meas_delhi$location_id)) == 1

  # Time aggregation
  meas_delhi_day <- measurements(city='Delhi', average_by='day', poll=creadb::PM10, collect=T)
  expect_equal(length(unique(lubridate::day(meas_delhi_day$date))), 31)
  expect_equal(length(unique(lubridate::month(meas_delhi_day$date))), 12)

  meas_delhi_month <- measurements(city='Delhi', average_by='month', poll=creadb::PM10)
  expect_equal(unique(lubridate::day(meas_delhi_month$date)), 1)
  expect_equal(length(unique(lubridate::month(meas_delhi_month$date))), 12)

  meas_delhi_year <- measurements(city='Delhi', average_by='year', poll=creadb::PM10)
  expect_equal(unique(day(meas_delhi_year$date)), 1)
  expect_equal(unique(month(meas_delhi_year$date)), 1)

  # Columns independent from aggregation
  average_bys <- c("hour", "day", "week", "month", "year")
  with_metadatas <- c(F,T)

  for(with_metadata in with_metadatas){
    lengths <- c()
    for(average_by in average_bys){
      lengths <- lengths %>% c(length(colnames(measurements(city='Delhi', average_by=average_by, with_metadata=with_metadata, collect=F))))
    }
    expect_equal(length(unique(lengths)), 1)
  }

  # Metadata reduction: check there are fewer columns without metadata
  meas_light <- measurements(city='Delhi', average_by='year', with_metadata = F, collect=F)
  meas_full <- measurements(city='Delhi', average_by='year', with_metadata = T, collect=F)
  expect_lt(length(colnames(meas_light)), length(colnames(meas_full)))
})

test_that("query return standard exceedances", {

  browser() # For debug

  # Filtering
  exc_unknown <- exceedances(city='XXX')
  expect_equal(nrow(exc_unknown), 0)

  exc_delhi <- exceedances(city='Delhi', year=2020, poll=creadb::PM25)
  expect_gt(nrow(exc_delhi), 0)
  expect_equal(tolower(unique(exc_delhi$city)), 'delhi')
  expect_gt(length(unique(exc_delhi$location)), 0)
  expect_gt(length(unique(exc_delhi$poll)), 0)

  exc_delhi_lower <- exceedances(city='delhi', year=2020, poll=creadb::PM25)
  expect_equal(nrow(exc_delhi_lower), nrow(exc_delhi))

  exc_delhi_jaipur <- exceedances(city=c('Delhi','Jaipur'), year=2020, poll=creadb::PM25)
  expect_gt(nrow(exc_delhi_jaipur), nrow(exc_delhi))

  exc_delhi_china <- exceedances(country='CN', city='Delhi')
  expect_equal(nrow(exc_delhi_china), 0)
})

test_that("Weather data is properly joined", {

  city=c('Beijing','北京市')
  poll <- creadb::PM25
  training_average_by <- 'hour'

  meas <- measurements(city=city,
                       date_from = '2015-01-01',
                       collect=F,
                       poll=poll,
                       average_by=training_average_by,
                       with_metadata = T)

  for (aggregate_per_city in c(T,F)){
    meas_weather <- join_weather_data(meas,
                                      measurements_averaged_by=training_average_by,
                                      aggregate_per_city=aggregate_per_city,
                                      collect=F,
                                      radius_km=20)

    # Check city aggregation (or lack of)
    expect_equal("city" %in% colnames(meas_weather), !aggregate_per_city)
    expect_equal("noaa_station_id" %in% colnames(meas_weather), !aggregate_per_city)
    expect_equal("location_id" %in% colnames(meas_weather), !aggregate_per_city)
  }



  nrow(meas_weather)
  a<-meas %>% summarize(n())
})
