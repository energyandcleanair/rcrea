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
  meas_delhi_month <- measurements(city='Delhi', average_by='month')
  expect_equal(unique(day(meas_delhi_month$date)), 1)
  expect_equal(length(unique(month(meas_delhi_month$date))), 12)

  meas_delhi_year <- measurements(city='Delhi', average_by='year')
  expect_equal(unique(day(meas_delhi_year$date)), 1)
  expect_equal(unique(month(meas_delhi_year$date)), 1)

})
