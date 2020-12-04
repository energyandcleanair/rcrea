library(testthat)

test_that("plot_measurements do not trigger errors", {

  browser() # For debug

  meas_delhi_mumbai<- rcrea::measurements(city=c('Delhi','Mumbai'),
                                          date_from='2019-01-01',
                                          poll=c(rcrea::PM25, rcrea::PM10),
                                          with_metadata=T,
                                          aggregate_level='city')

  # Time series per location
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, subplot_by='location_id'), NA)
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, subplot_by='location_name'), NA)

  # Time series per pollutant (average per city is taken by default)
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, subplot_by='poll'), NA)
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, subplot_by='poll', color_by="location_name"), NA)

  # Time series of PM2.5 per location (yearly running average)
  # expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, running_days=365, subplot_by='location'), NA)

  # Time series of PM2.5 per city (30-days running average)
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, running_days=30, subplot_by='location_id'), NA)

  # Monthly average
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, average_by='month', subplot_by='location_id'), NA)

  # Yearly average
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, average_by='year', subplot_by='location_id'), NA)

  # Heatmap per city with monthly data
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, subplot_by='location_id', type='heatmap', average_by='month'), NA)

  # Heatmap per city weekly values
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, subplot_by='location_id', type='heatmap'), NA)

  # Evolution over years
  expect_error(rcrea::plot_measurements(meas_delhi_mumbai, poll=rcrea::PM25, color_by='year', subplot_by='location_id', average_by='month'), NA)

})

