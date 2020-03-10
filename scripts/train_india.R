library(creadb)
library(ggplot2)
library(dplyr)
library(gbm)
library(tidyr)
library(lubridate)
library(zoo)

city <- c('Delhi', 'Jaipur', 'Mumbai', 'Kolkata')
poll <- c(PM25, PM10, SO2, NO2, CO)

training_prediction_cut <- lubridate::as_date("2020-01-01")

# On what rolling average do we want to train the model
training_average_by='hour'
training_average_by_widths=c(1,3,8)

# On what rolling average do we want to plot results
plotting_average_by='day'
plotting_average_by_width=30


for(training_average_by_width in training_average_by_widths){
  print(paste("Training average width:",training_average_by_width))
  meas_weather <- aq_weather.m.collect(city=city, poll=poll, average_by=training_average_by, weather_radius_km = 50)

  weather_vars <- vars(temp_c, slp_hp, wind_deg, wind_ms, sky_code, prec_1h_mm, prcp)
  meas_weather$wind_deg_factor = factor(meas_weather$wind_deg %/% 45)

  models <- aq_weather.default_models()[c('gbm','rpart')]

  formulas <- c(value ~ temp_c + wind_deg_factor + wind_ms + slp_hp + rh_percent + sky_code,
                value ~ temp_c + wind_deg_factor + wind_ms + slp_hp + rh_percent + sky_code + prec,
                value ~ temp_c + wind_deg + wind_ms + slp_hp + rh_percent + sky_code,
                value ~ temp_c + wind_deg + wind_ms + slp_hp + rh_percent + sky_code)

  for(formula in formulas){
    print(paste("Formula:", paste(formula, collapse='')))
    result <- aq_weather.predict(meas_weather = meas_weather,
                                 formula=formula,
                                 training_prediction_cut = training_prediction_cut,
                                 training_average_by = training_average_by,
                                 training_average_by_width = training_average_by_width,
                                 models = models)

    aq_weather.plot(result,
                    plotting_average_by = plotting_average_by,
                    plotting_average_by_width = plotting_average_by_width,
                    training_prediction_cut = training_prediction_cut,
                    subtitle=paste(paste(city, collapse=','),
                                   paste("Training:",training_average_by_width, training_average_by),
                                   paste(formula,collapse=''),
                                   paste("Plotting:",plotting_average_by_width, plotting_average_by),sep="\n"),
                    filename = paste0('export_',
                                      format(Sys.time(), "%Y-%m-%d_%H:%M"),'.pdf')
    )

    print("Done")
  }
}
