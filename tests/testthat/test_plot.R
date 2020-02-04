library(testthat)
library(lubridate)

test_that("plots do not trigger errors", {

  browser() # For debug

  meas_delhi_mumbai<- measurements(city=c('Delhi','Mumbai'), date_from='2019-01-01', poll=c(creadb::PM25, creadb::PM10))

  # Time series per location
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, subplot_by='location'), NA)

  # Time series per pollutant (average per city is taken by default)
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, subplot_by='poll'), NA)

  # Time series of PM2.5 per location (yearly running average)
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, running_days=365, subplot_by='location'), NA)

  # Time series of PM2.5 per city (yearly running average)
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, running_days=365, subplot_by='city'), NA)

  # Monthly average
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, average_by='month', subplot_by='city'), NA)

  # Yearly average
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, average_by='year', subplot_by='city'), NA)

  # Heatmap per location with monthly data
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, subplot_by='location', type='heatmap', average_by='month'), NA)

  # Heatmap per city weekly values
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, subplot_by='city', type='heatmap'), NA)

  # Evolution over years
  expect_error(plt <- plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, color_by='year', subplot_by='city', average_by='month'), NA)

})
