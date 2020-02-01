library(testthat)
library(lubridate)

test_that("Gradient boosted model works", {

  result <- predict_aq_from_weather(city='Delhi',
                                    poll=creadb::PM25,
                                    training_prediction_cut=lubridate::as_date("2017-10-01"),
                                    training_average_by='hour',
                                    training_average_by_width=3,
                                    weather_radius_km=20
                          )
})
