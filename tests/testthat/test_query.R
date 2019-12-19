library(testthat)
library(lubridate)

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

test_that("query return measurements", {

  browser() # For debug

  # Filtering
  meas_unknown <- measurements(city='XXX')
  expect_equal(nrow(meas_unknown), 0)

  meas_unkown <- measurements(user_filter=function(x){x %>% filter(avg_day<=1000 | pollutant==CO)})

  meas_delhi <- measurements(city='Delhi')
  expect_gt(nrow(meas_delhi), 0)
  expect_equal(tolower(unique(meas_delhi$city)), 'delhi')
  expect_gt(length(unique(meas_delhi$location)), 0)
  expect_gt(length(unique(meas_delhi$poll)), 0)

  meas_delhi_lower <- measurements(city='delhi')
  expect_equal(nrow(meas_delhi_lower), nrow(meas_delhi))

  meas_delhi_jaipur <- measurements(city=c('Delhi','Jaipur'))
  expect_gt(nrow(meas_delhi_jaipur), nrow(meas_delhi))

  meas_delhi_china <- measurements(country='CN', city='Delhi')
  expect_equal(nrow(meas_delhi_china), 0)

  # Time aggregation
  meas_delhi_day <- measurements(city='Delhi', average_by='day')
  expect_equal(length(unique(day(meas_delhi_day$date))), 31)
  expect_equal(length(unique(month(meas_delhi_day$date))), 12)

  meas_delhi_month <- measurements(city='Delhi', average_by='month')
  expect_equal(unique(day(meas_delhi_month$date)), 1)
  expect_equal(length(unique(month(meas_delhi_month$date))), 12)

  meas_delhi_year <- measurements(city='Delhi', average_by='year')
  expect_equal(unique(day(meas_delhi_year$date)), 1)
  expect_equal(unique(month(meas_delhi_year$date)), 1)

})

test_that("query return standard exceedances", {

  browser() # For debug

  # Filtering
  exc_unknown <- exceedances(city='XXX')
  expect_equal(nrow(exc_unknown), 0)

  exc_delhi <- exceedances(city='Delhi')
  expect_gt(nrow(exc_delhi), 0)
  expect_equal(tolower(unique(exc_delhi$city)), 'delhi')
  expect_gt(length(unique(exc_delhi$location)), 0)
  expect_gt(length(unique(exc_delhi$poll)), 0)

  exc_delhi_lower <- exceedances(city='delhi')
  expect_equal(nrow(exc_delhi_lower), nrow(exc_delhi))

  exc_delhi_jaipur <- exceedances(city=c('Delhi','Jaipur'))
  expect_gt(nrow(exc_delhi_jaipur), nrow(exc_delhi))

  exc_delhi_china <- exceedances(country='CN', city='Delhi')
  expect_equal(nrow(exc_delhi_china), 0)


})

test_that("Measurements are correct", {

  meas_sirifort <- measurements(location='Sirifort, Delhi - CPCB',average_by='year')


})
