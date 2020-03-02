source('R/setup.r')




#---------------------
# Prediction functions
#---------------------
model_glm <- function(training_data){
  # Train model
  gbm.fit <- gbm::gbm(
    formula = value ~ temp_c + factor(wind_deg) + wind_ms + slp_hp + rh_percent, #+ prec_6h_mm, #+ factor(sky_code),
    data = training_data,
    cv.folds = 5,
    verbose = FALSE
  )
  return(gbm.fit)
}


#---------------------
# Run predictions
#---------------------
#' Predict Air Quality From Weather
#'
#' @param city
#' @param poll
#' @param training_prediction_cut
#' @param training_average_by
#' @param training_average_by_width
#' @param weather_radius_km
#'
#' @return
#' @export
#'
#' @examples
predict_aq_from_weather <- function(
  meas_weather,
  training_prediction_cut,
  training_average_by='hour',
  training_average_by_width=3,
  weather_radius_km=20,
  models=c(model_glm)
){

  city_ <- city
  poll_ <- poll
  date_from_ <- date_from

  #----------------
  # Prepare data
  #----------------
  weather_vars <- vars(temp_c, slp_hp, wind_deg, wind_ms, sky_code, prec_1h_mm, prec_6h_mm, rh_percent)
  id_vars <- vars(date)

  res.data.raw <- meas_weather

  # Make date axis homogeneous i.e. a row for every day / month / year per city and pollutant
  date_grid <- res.data.raw %>% dplyr::group_by(city, poll) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::mutate(date=list(seq(date_min, date_max, by=training_average_by))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))

  res.data.raw <- merge(res.data.raw, date_grid, all=TRUE)

  # Group & nest measurements by city and pollutant
  # res.data.raw <- res.data.raw %>% dplyr::group_by(city,poll) %>% tidyr::nest()


  # Rolling mean for training
  mean_fn <- function(x){
    if(is.numeric(x)){
      return(mean(x, na.rm = T))
    }else{
      return(names(sort(table(x), decreasing = T, na.last = T)[1]))
    }
  }
  train_roll_fn <- function(var) zoo::rollapply(var, width=training_average_by_width, FUN=mean_fn, align='right', fill=NA)
  res.data.rolled <- res.data.raw %>% group_by(city, poll) %>% arrange(date) %>%
    mutate_at(vars(-city, -poll, -date), train_roll_fn)

  # Remove rows with no weather observation
  res.data.rolled <-
    res.data.rolled %>% filter_at(vars(-city, -poll, -date, -value), any_vars(!is.na(.)))

  # Separate in training and prediction data
  res.data.final <- res.data.rolled %>% tidyr::nest() %>%
    mutate(training = map(data, ~ filter(.x, date < training_prediction_cut))) %>%
    mutate(predicting = map(data, ~ filter(.x, date >= training_prediction_cut)))
  # assert("Check nesting is correct", colnames(res.data.final) == c('city','poll','data','training','predicting'))


  #----------------
  # Train models
  #----------------
  res.data.final <- res.data.final %>% expand_grid(model=models)
  res.data.final <- res.data.final %>% mutate(model_fitted=map(training, model))
  res.data.final$trainin

  # Shape results
  measured <- res.data.rolled %>% dplyr::select(date,value) %>%
    dplyr::mutate(measured=value) %>%
    tidyr::gather("type","value", measured)

  predict_data$predicted <- gbm::predict.gbm(res.fit, predict_data)
  predicted <- predict_data %>%
    dplyr::select(date, predicted)%>%
    tidyr::gather("type","value",predicted)

  training_data$fitted <- predict.gbm(res.fit, training_data)
  fitted <- training_data %>%
    dplyr::select(date, fitted) %>%
    tidyr::gather("type", "value", fitted)

  result <- rbind(measured, predicted, fitted)

  return(result)
}

#-------------------
# Plot results
#-------------------
roll_plot <- function(raw){
  result <- raw %>% select(date,type,value) %>%
  dplyr::mutate(date=lubridate::round_date(date, unit = plotting_average_by)) %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(value=mean(value, na.rm = T)) %>% dplyr::ungroup() %>%
  dplyr::arrange(date) %>% dplyr::group_by(type) %>%
  dplyr::mutate(value=rollapply(value, width=plotting_average_by_roll,
                                FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA)) %>%
  dplyr::ungroup()

  return(result)
}

