library(testthat)
library(lubridate)


test_that("Querying measurements with weather works", {
  t <- system.time({result <- measurements_with_weather(city='Delhi',
                                                   poll=PM25,
                                                   date_from='2020-01-01',
                                                   average_by = 'hour',
                                                   aggregate_at_city_level = T,
                                                   weather_radius_km = 20,
                                                   collect=T)})

  expect_lt(t['elapsed'],30)
  expect_gt(length(unique(result$wind_deg)), 1)
  expect_gt(length(unique(result$rh_percent)), 1)
  expect_gt(length(unique(result$prec_6h_mm)), 1)
  expect_gt(length(unique(result$slp_hp)), 1)
  expect_gt(length(unique(result$temp_c)), 1)
  expect_gt(length(unique(result$sky_code)), 1)


})
# test_that("Gradient boosted model works", {
#
#   result <- predict_aq_from_weather(city='Delhi',
#                                     poll=creadb::PM25,
#                                     training_prediction_cut=lubridate::as_date("2017-10-01"),
#                                     training_average_by='hour',
#                                     training_average_by_width=3,
#                                     weather_radius_km=20
#                           )
# })
