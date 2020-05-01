library(testthat)
# library(lubridate)
# library(DBI)

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
  meas2 <- measurements(location_id='IN-107', poll=rcrea::CO, date_from="2020-01-01", collect=T) # Queries first locations and then measurements
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
#   time_postgres <- system.time(measurements(con=con_postgres, date_from='2019-01-01', city='Delhi', poll=rcrea::CO, collect=T))
#   DBI::dbDisconnect(con_postgres)
#
#   con_postgresql <- DBI::dbConnect(RPostgres::Postgres(), dbname = CONN_DBNAME,
#                                  host = CONN_HOST,
#                                  port = strtoi(CONN_PORT),
#                                  user = CONN_USER,
#                                  password = CONN_PASSWORD)
#
#   time_postgresql <- system.time(measurements(con=con_postgresql, date_from='2019-01-01', city='Delhi', poll=rcrea::CO, collect=T))
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

  meas_delhi <- measurements(city='Delhi', poll=rcrea::CO, date_from='2019-01-01')
  expect_gt(nrow(meas_delhi), 0)
  expect_equal(tolower(unique(meas_delhi$city)), 'delhi')
  # expect_gt(length(unique(meas_delhi$location)), 0)
  expect_equal(length(unique(meas_delhi$poll)), 1)

  meas_delhi_lower <- measurements(city='delhi', poll=rcrea::CO, date_from='2019-01-01')
  expect_equal(nrow(meas_delhi_lower), nrow(meas_delhi))

  meas_delhi_jaipur <- measurements(city=c('Delhi','Jaipur'), poll=rcrea::CO, date_from='2019-01-01')
  expect_gt(nrow(meas_delhi_jaipur), nrow(meas_delhi))

  meas_delhi_china <- measurements(country='CN', city='Delhi')
  expect_equal(nrow(meas_delhi_china), 0)

  # Location id
  meas_delhi <- measurements(city='Delhi', poll=rcrea::CO, date_from='2020-01-01', average_by='year', aggregate_level='location')
  length(unique(meas_delhi$location_id)) >= 23 # 23 stations with CO data  in Delhi at the time of writing
  meas_delhi <- measurements(city='Delhi', poll=rcrea::CO, date_from='2020-01-01', average_by='year', aggregate_level='location')
  length(unique(meas_delhi$location_id)) == 1

  # Time aggregation
  meas_delhi_day <- measurements(city='Delhi', average_by='day', poll=rcrea::PM10, collect=T)
  expect_equal(length(unique(lubridate::day(meas_delhi_day$date))), 31)
  expect_equal(length(unique(lubridate::month(meas_delhi_day$date))), 12)

  meas_delhi_month <- measurements(city='Delhi', average_by='month', poll=rcrea::PM10)
  expect_equal(unique(lubridate::day(meas_delhi_month$date)), 1)
  expect_equal(length(unique(lubridate::month(meas_delhi_month$date))), 12)

  meas_delhi_year <- measurements(city='Delhi', average_by='year', poll=rcrea::PM10)
  expect_equal(unique(lubridate::day(meas_delhi_year$date)), 1)
  expect_equal(unique(lubridate::month(meas_delhi_year$date)), 1)

  # Different aggregations / averaging
  average_bys <- c("hour", "day", "week", "month", "year")
  with_metadatas <- c(F,T)
  aggregate_levels <- c('location','city','gadm2','gadm2','country')

  for(with_metadata in with_metadatas){
    lengths <- c()
    for(average_by in average_bys){
      for(aggregate_level in aggregate_levels){
        tryCatch({
          meas <- measurements(city='Delhi',
                               date_from='2020-04-10',
                               average_by=average_by,
                               with_metadata=with_metadata,
                               aggregate_level=aggregate_level,
                               collect=F)

          new_col_names <- colnames(meas)

          # Key columns not missing
          expect_true('unit' %in% new_col_names)
          expect_true('source' %in% new_col_names)

          # some results (not yet with gadms...)
          # expect_gt(nrow(meas), 0)

        }, error=function(err){
          testthat::fail(message=paste("Query failed",
                     "average_by:", average_by,
                     "aggregate_level:", aggregate_level,
                     "with_metadata:", with_metadata,
                     err
                     ))
        })
      }
    }
  }


  # Metadata reduction: check there are fewer columns without metadata
  meas_light <- measurements(city='Delhi', average_by='year', with_metadata = F, collect=F)
  meas_full <- measurements(city='Delhi', average_by='year', with_metadata = T, collect=F)
  expect_lt(length(colnames(meas_light)), length(colnames(meas_full)))

  # Querying measurements with noaa station ids
  meas_wo_noaa <- measurements(city='Delhi',
                               add_noaa_station_ids = T, collect=F)
  expect_true('noaa_station_ids' %in% colnames(meas_wo_noaa))

})

test_that("query return standard exceedances", {

  browser() # For debug

  # Filtering
  exc_unknown <- exceedances(city='XXX')
  expect_equal(nrow(exc_unknown), 0)

  exc_delhi <- exceedances(city='Delhi', year=2020, poll=rcrea::PM25)
  expect_gt(nrow(exc_delhi), 0)
  expect_equal(tolower(unique(exc_delhi$city)), 'delhi')
  expect_gt(length(unique(exc_delhi$poll)), 0)

  exc_delhi_lower <- exceedances(city='delhi', year=2020, poll=rcrea::PM25)
  expect_equal(nrow(exc_delhi_lower), nrow(exc_delhi))

  exc_delhi_jaipur <- exceedances(city=c('Delhi','Jaipur'), year=2020, poll=rcrea::PM25)
  expect_gt(nrow(exc_delhi_jaipur), nrow(exc_delhi))

  exc_delhi_china <- exceedances(country='CN', city='Delhi')
  expect_equal(nrow(exc_delhi_china), 0)
})


test_that("measurements time aggregation", {

  meas_hour_day <- measurements(
    location_id='IN-82',
    poll=PM25,
    date_from='2019-08-01 00:00',
    date_to='2019-08-01 23:00',
    average_by = 'hour',
    aggregate_level = 'location',
    collect = T)

  expect_gt(nrow(meas_hour_day), 20) #should be 24 but not every day is complete
  expect_lte(nrow(meas_hour_day), 24) #should be 24 but not every day is complete

  meas_day <- measurements(
    location_id='IN-82',
    poll=PM25,
    date_from='2019-08-01',
    date_to='2019-08-01',
    average_by = 'day',
    aggregate_level =  'location',
    collect = T)

  expect_equal(nrow(meas_day),1)

})


test_that("measurements have a properly set timezone", {

  # AQ from CPCB directly: Date is local
  # Station: North Campus, DU, Delhi - IMD
  # Note: our meas df is averaged per city
  # S.No	From Date	To Date	                  PM2.5 (ug/m3)	NO (ug/m3)	NO2 (ug/m3)	NOx (ppb)	PM10 (ug/m3)
  # 1	01-Aug-2019 - 00:00	01-Aug-2019 - 01:00	30.95	6.88	7.68	14.56	78.86
  # 2	01-Aug-2019 - 01:00	01-Aug-2019 - 02:00	38.81	7.03	7.53	14.57	39.78
  # 3	01-Aug-2019 - 02:00	01-Aug-2019 - 03:00	39.12	8.13	7.96	16.09	62.34
  # 4	01-Aug-2019 - 03:00	01-Aug-2019 - 04:00	41.7	8.85	8.27	17.13	59.1
  # 5	01-Aug-2019 - 04:00	01-Aug-2019 - 05:00	27.58	9.48	8.66	18.14	52.85
  # 6	01-Aug-2019 - 05:00	01-Aug-2019 - 06:00	25.1	8.44	8.39	16.83	92.77
  # 7	01-Aug-2019 - 06:00	01-Aug-2019 - 07:00	20.54	8.58	8.52	17.09	86.89
  # 8	01-Aug-2019 - 07:00	01-Aug-2019 - 08:00	25.04	7.96	8.4	16.35	83.66
  # 9	01-Aug-2019 - 08:00	01-Aug-2019 - 09:00	36.66	6.62	7.61	14.22	92.78
  # 10	01-Aug-2019 - 09:00	01-Aug-2019 - 10:00	36.1	5.09	6.81	11.9	17.14
  # 11	01-Aug-2019 - 10:00	01-Aug-2019 - 11:00	49.68	4.37	6.46	10.83	43.72
  # 12	01-Aug-2019 - 11:00	01-Aug-2019 - 12:00	46.49	4.05	6.38	10.42	51.29
  # 13	01-Aug-2019 - 12:00	01-Aug-2019 - 13:00	40.86	4.02	6.53	10.54	37.03
  # 14	01-Aug-2019 - 13:00	01-Aug-2019 - 14:00	33.5	4.17	6.8	10.97	80.62
  # 15	01-Aug-2019 - 14:00	01-Aug-2019 - 15:00	32.02	4.25	7.03	11.29	95.34
  # 16	01-Aug-2019 - 15:00	01-Aug-2019 - 16:00	30.71	6.1	8.12	14.22	95.74
  # 17	01-Aug-2019 - 16:00	01-Aug-2019 - 17:00	5.59	5.78	7.51	13.29	47.5
  # 18	01-Aug-2019 - 17:00	01-Aug-2019 - 18:00	26.65	5.23	6.92	12.15	34.57
  # 19	01-Aug-2019 - 18:00	01-Aug-2019 - 19:00	21.39	5.42	7.12	12.54	15.65
  # 20	01-Aug-2019 - 19:00	01-Aug-2019 - 20:00	26.94	6.22	7.62	13.84	34.86
  # 21	01-Aug-2019 - 20:00	01-Aug-2019 - 21:00	25.2	6.8	8.01	14.81	46.3
  # 22	01-Aug-2019 - 21:00	01-Aug-2019 - 22:00	22.06	7.49	8.47	15.96	64.45
  # 23	01-Aug-2019 - 22:00	01-Aug-2019 - 23:00	19.26	8.6	8.85	17.45	103.95
  # 24	01-Aug-2019 - 23:00	02-Aug-2019 - 00:00	19.9	10.49	9.48	19.96	93.02

  official_dates <- tibble::tibble(lubridate::force_tz(lubridate::ymd_h(c("2019-08-01 00",
                                                           "2019-08-01 01",
                                                           "2019-08-01 02",
                                                           "2019-08-01 03",
                                                           "2019-08-01 04")),
                                        tzone='Asia/Kolkata'))

  official_pm25 <- c(30.95,38.81,39.12,41.7,27.58)


  meas_test_location <- measurements(
                            location_id='IN-82',
                            poll=PM25,
                            date_from='2019-08-01',
                            date_to='2019-08-02',
                            average_by = 'hour',
                            aggregate_level='location',
                            collect = T) %>% dplyr::arrange(date)

  meas_dates <- meas_test_location[1:5,'date']
  meas_pm25 <- meas_test_location[1:5,'value']
  expect_true(all(official_dates == meas_dates))
  expect_true(all(abs((meas_pm25-official_pm25)/official_pm25)<0.04)) # max 4% error
  expect_equal(nrow(meas_test_location), 24)

})
