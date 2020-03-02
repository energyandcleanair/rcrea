source('R/setup.r')


#-------------------------------------
# Get measurements with weather data
#-------------------------------------
#' Measurements with weather information attached
#'
#' @param city
#' @param poll
#' @param date_from
#' @param average_by
#' @param weather_radius_km Maximum distance between weather station and air quality
#'
#' @return tibble with hourly
#' @export
#'
#' @examples
aq_weather.collect <- function(city,
                                poll,
                                date_from='2015-01-01',
                                average_by='hour',
                                aggregate_at_city_level=T,
                                weather_radius_km=20,
                                collect=T){

  # Get measurements
  meas <- measurements(city=city,
                       date_from=date_from,
                       collect=F, #to save time (only do collection at last step)
                       poll=poll,
                       average_by=average_by,
                       aggregate_at_city_level=aggregate_at_city_level,
                       add_noaa_station_ids = T,
                       noaa_station_radius_km = weather_radius_km,
                       with_metadata = T)

  # Attach weather observations and aggregate by date
  meas_weather <- join_weather_data(meas,
                                    measurements_averaged_by=average_by,
                                    aggregate_at_city_level=aggregate_at_city_level,
                                    collect=collect)
  return(meas_weather)
}



#---------------------
# Prediction functions
#---------------------
aq_weather.default_models <- function(){

  model_gbm <- function(training_data){
    gbm.fit <- gbm::gbm(
      formula = value ~ temp_c + wind_deg + wind_ms + slp_hp + rh_percent, #+ prec_6h_mm, #+ factor(sky_code),
      data = training_data,
      cv.folds = 5,
      verbose = FALSE
    )
    return(gbm.fit)
  }

  model_rpart <- function(training_data){
    rpart.fit <- rpart::rpart(
      formula = value ~ temp_c + wind_deg + wind_ms + slp_hp + rh_percent, #+ prec_6h_mm, #+ factor(sky_code),
      data = training_data
    )
    return(rpart.fit)
  }

  models <- list(gbm=model_gbm, rpart=model_rpart)
  return(models)
}

#---------------------
# Run predictions
#---------------------
#' Predict Air Quality From Weather
#'
#' @param meas_weather
#' @param training_prediction_cut
#' @param training_average_by
#' @param training_average_by_width
#' @param weather_radius_km
#'
#' @return
#' @export
#'
#' @examples
aq_weather.predict <- function(
  meas_weather,
  training_prediction_cut,
  training_average_by='hour',
  training_average_by_width=3,
  models=aq_weather.default_models()
){

  #----------------
  # Prepare data
  #----------------
  weather_vars <- vars(temp_c, slp_hp, wind_deg, wind_ms, sky_code, prec_6h_mm, rh_percent)
  id_vars <- vars(date)

  result <- meas_weather

  # Make date axis homogeneous i.e. a row for every day / month / year per city and pollutant
  date_grid <- result %>% dplyr::group_by(city, poll) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::mutate(date=list(seq(date_min, date_max, by=training_average_by))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))

  result <- merge(result, date_grid, all=TRUE)

  # Rolling mean for training
  mean_fn <- function(x){
    if(is.numeric(x)){
      res <- mean(x, na.rm = T) # it sometimes returns NaN but models expect only NA
      return(if(is.na(res)) NA else res)
    }else{
      return(names(sort(table(x), decreasing = T, na.last = T)[1]))
    }
  }
  train_roll_fn <- function(var) zoo::rollapply(var, width=training_average_by_width, FUN=mean_fn, align='right', fill=NA)
  result <- result %>% group_by(city, poll) %>% arrange(date) %>%
    mutate_at(vars(-city, -poll, -date), train_roll_fn)

  # Remove rows with no (full) weather observation
  keep_only_full_observations <- TRUE
  # NOT CLEAN but putting it here for the moment
  result$prec_6h_mm[is.na(result$prec_6h_mm)] <- 0
  if(keep_only_full_observations){
    result <- result %>% dplyr::filter_at(c(weather_vars, vars(value)), all_vars(!is.na(.)))
  }else{
    result <- result %>% dplyr::filter_at(c(weather_vars, vars(value)), any_vars(!is.na(.)))
  }

  # Separate in training and prediction data and nest data by city and poll (still grouped by these two columns)
  result <- result %>% tidyr::nest() %>%
    dplyr::mutate(training = purrr::map(data, ~ filter(.x, date < training_prediction_cut))) %>%
    dplyr::mutate(predicting = purrr::map(data, ~ filter(.x, date >= training_prediction_cut)))
  # assert("Check nesting is correct", colnames(res.data.final) == c('city','poll','data','training','predicting'))

  # Remove those without training data
  no_training <- result %>% filter(purrr::map_int(training, nrow) == 0)
  if(nrow(no_training)>0){
    warning("Some combinations have no training data")
    result <- result %>% filter(purrr::map_int(training, nrow) > 0)
  }

  #----------------
  # Train models
  #----------------
  # Adding models to tibble (each model is applied to each city, poll combination)
  model_names <- if (!is.null(names(models))) names(models) else seq_along(models)
  models_df <- tibble(model_name=paste0("model_",names(models)), model=models)
  result <- result %>% tidyr::crossing(models_df)
  result <- result %>% mutate(model_fitted=purrr::map2(training, model, ~(.y(.x))))

  #----------------
  # Predict
  #----------------
  result <- result %>% mutate(training=purrr::map2(training, model_fitted, ~ .x %>% mutate(fitted=predict(.y, .x))))
  result <- result %>% mutate(predicting=purrr::map2(predicting, model_fitted, ~ .x %>% mutate(predicted=predict(.y, .x))))
  return(result)
}

#-------------------
# Plot results
#-------------------
aq_weather.plot <- function(result,
                            plotting_average_by='day',
                            plotting_average_by_width=30,
                            training_prediction_cut=NULL){

  #----------------
  # Shape results
  #----------------
  # 1-gather (i.e. wide -> narrow)
  # 2-rolling average
  roll_plot <- function(raw){
    result <- raw %>% dplyr::select(city, poll, model_name, date, type, value) %>%
      dplyr::mutate(date=lubridate::round_date(date, unit = plotting_average_by)) %>%
      dplyr::group_by(city, poll, model_name, date, type) %>%
      dplyr::summarise(value=mean(value, na.rm = T)) %>% dplyr::ungroup() %>%
      dplyr::group_by(city, poll, model_name, type) %>% dplyr::arrange(date) %>%
      dplyr::mutate(value=zoo::rollapply(value, width=plotting_average_by_width,
                                    FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA)) %>%
      dplyr::ungroup()

    return(result)
  }

  fitted <- result %>% dplyr::select(city, poll, model_name, training) %>%
    tidyr::unnest(c(training)) %>% dplyr::select(city, poll, model_name, date, value, fitted) %>%
    tidyr::gather("type", "value", -c(city, poll, date, model_name)) %>% roll_plot()

  predicted <- result %>% dplyr::select(city, poll, model_name, predicting) %>%
    tidyr::unnest(c(predicting)) %>% dplyr::select(city,poll,model_name, date, value, predicted) %>%
    tidyr::gather("type", "value", -c(city,poll,date,model_name)) %>% roll_plot()

  combined <- rbind(fitted, predicted)
  plot <- ggplot(combined, aes(x=date, y=value)) + geom_line(aes(colour=type)) + facet_wrap(~ city + poll + model_name )

  if(!is.null(training_prediction_cut)){
    plot <- plot +
      geom_vline(xintercept=as.POSIXct(training_prediction_cut), linetype="dotted", color="blue")
  }
  return(plot)
}

