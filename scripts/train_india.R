library(creadb)
library(ggplot2)
library(dplyr)
library(gbm)
library(tidyr)
library(lubridate)
library(zoo)

all_results <- NULL
# Top 10 cities with the most measurements
city <- c('Delhi', 'Bengaluru', 'Hyderabad', 'Lucknow', 'Chennai', 'Mumbai', 'Jaipur', 'Chandrapur', 'Chandrapur', 'Kolkata')
poll <- c(PM25, PM10, SO2, NO2, CO, O3, NO, NOX)

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
  meas_weather$wind_deg_factor = factor(meas_weather$wind_deg %/% 45)
  meas_weather$sky_code_factor <- factor(meas_weather$sky_code, ordered = TRUE)


  models <- aq_weather.default_models()[c('gbm','rpart')]

  formulas <- c(value ~ temp_c + wind_deg_factor + wind_ms + slp_hp + rh_percent + sky_code,
                value ~ temp_c + wind_deg_factor + wind_ms + slp_hp + rh_percent + sky_code_factor,
                value ~ temp_c + wind_deg_factor + wind_ms + slp_hp + rh_percent + sky_code_factor + prcp,
                value ~ temp_c + wind_deg_factor*wind_ms + slp_hp + rh_percent + sky_code_factor,
                value ~ temp_c + wind_deg_factor*wind_ms + slp_hp + rh_percent + sky_code_factor, + prcp)

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
                                      format(Sys.time(), "%Y-%m-%d_%H:%M:%S"),'.pdf'))

    # Store result
    result_rich <- result
    result_rich$training_average_by=training_average_by
    result_rich$training_average_by_width=training_average_by_width
    result_rich$formula=paste(formula, collapse=' ')
    if(is.null(all_results)){
      all_results <- result_rich
    }else{
      all_results <- bind_rows(all_results,
                               result_rich)
    }
  }
}

ggplot(all_results %>% mutate(label=paste("R2:",sprintf("%.2f",rsq))), aes(x = formula, y = training_average_by_width, fill = rsq, label=label)) +
  facet_grid(city + poll ~ model_name, scales = "free") +
  geom_tile() + geom_text()
