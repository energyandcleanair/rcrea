source('R/setup.r')


predict_aq_from_weather <- function(city,
                     poll,
                     training_prediction_cut,
                     training_average_by='hour',
                     training_average_by_width=3,
                     weather_radius_km=20){


  # Variables
  weather_vars <- vars(temp_c, slp_hp, wind_deg, wind_ms, sky_code, prec_1h_mm, prec_6h_mm, rh_percent)
  id_vars <- vars(date)

  # Get measurements
  meas <- creadb::measurements(city=city,
                               date_from = '2015-01-01',
                               collect=F, #to save time (only do collection at last step)
                               poll=poll,
                               average_by=training_average_by,
                               with_metadata = T)

  # Attach weather observations and aggregate by date
  meas_weather <- creadb::join_weather_data(meas,
                                            measurements_averaged_by=training_average_by,
                                            aggregate_per_city=T,
                                            collect=T,
                                            radius_km=weather_radius_km) #max distance between weather station and air quality stations

  gbm.data.raw <- meas_weather

  # Make date axis homogeneous i.e. a row for every day / month / year
  dates <- seq(min(gbm.data.raw$date), max(gbm.data.raw$date), by=training_average_by)
  gbm.data.raw <- merge(gbm.data.raw, data.frame(date=dates), all=TRUE)

  # Apply rolling mean for num (and most common element for other types e.g. sky_code)
  mean_fn <- function(x){
    if(is.numeric(x)){
      return(mean(x, na.rm = T))
    }else{
      return(names(sort(table(x), decreasing = T, na.last = T)[1]))
    }
  }
  train_roll_fn <- function(var) rollapply(var, width=training_average_by_width, FUN=mean_fn, align='right', fill=NA)
  gbm.data.rolled <- gbm.data.raw %>% arrange(date) %>%
    mutate_at(weather_vars, train_roll_fn) %>%
    mutate(value=train_roll_fn(value))

  # Remove rows with no weather observation
  gbm.data.rolled <-
    gbm.data.rolled %>% filter_at(c(vars("value"), weather_vars), any_vars(!is.na(.)))

  # Separate in training and prediction data
  training_data <- gbm.data.rolled %>% filter(date < training_prediction_cut)
  predict_data <- gbm.data.rolled %>% filter(date >= training_prediction_cut)

  # Train model
  gbm.fit <- gbm::gbm(
    formula = value ~ temp_c + factor(wind_deg) + wind_ms + slp_hp + rh_percent + prec_6h_mm, #+ factor(sky_code),
    data = training_data,
    cv.folds = 5,
    verbose = FALSE
  )

  # Shape results
  measured <- gbm.data.rolled %>% select(date,value) %>%
    dplyr::mutate(measured=value) %>%
    tidyr::gather("type","value", measured)

  predict_data$predicted <- predict.gbm(gbm.fit, predict_data)
  predicted <- predict_data %>%
    select(date, predicted)%>%
    tidyr::gather("type","value",predicted)

  training_data$fitted <- predict.gbm(gbm.fit, training_data)
  fitted <- training_data %>%
    select(date, fitted) %>%
    tidyr::gather("type", "value", fitted)

  result <- rdbind(measured, predicted, fitted)

  return(result)
}
