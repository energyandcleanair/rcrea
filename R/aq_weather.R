source('R/setup.r')

require(rnoaa)
require(e1071)
require(memoise)
require(standardize)
require(Metrics)
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
                                weather_radius_km=20){

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

  # Attach weather observations from ISD and aggregate by time & city
  meas_weather <- weather.isd.join(meas,
                                  measurements_averaged_by=average_by,
                                  aggregate_at_city_level=aggregate_at_city_level,
                                  collect=T)

  # Attach precipitation from GHCND (ISD precipitation data seems very sparse)
  meas_weather <- weather.ghcnd.join(meas_weather, weather_radius_km=weather_radius_km)

  return(meas_weather)
}

if(!exists("aq_weather.m.collect")){
  aq_weather.m.collect <- memoise(aq_weather.collect, cache=fc)
}


#---------------------
# Prediction functions
#---------------------
aq_weather.default_models <- function(){

  model_gbm <- function(training_data, formula){
    gbm.fit <- gbm::gbm(
      formula = formula, #+ prec_6h_mm, #+ factor(sky_code),
      data = training_data,
      cv.folds = 5,
      verbose = FALSE
    )
    return(gbm.fit)
  }

  model_rpart <- function(training_data, formula){
    rpart.fit <- rpart::rpart(
      formula = formula, #+ prec_6h_mm, #+ factor(sky_code),
      data = training_data
    )
    return(rpart.fit)
  }

  model_svr <- function(training_data, formula){
    tuneResult <- e1071::tune(e1071::svm, formula,
                              data = training_data,
                              ranges = list(epsilon = seq(0,1,0.2), cost= 2^(2:5)))
    return(tuneResult$best.model)
  }

  models <- list(gbm=model_gbm, rpart=model_rpart, svr=model_svr)
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
  formula,
  training_prediction_cut,
  training_average_by='hour',
  training_average_by_width=3,
  models=aq_weather.default_models(formula)
){

  #----------------
  # Prepare data
  #----------------
  weather_vars <- vars(all.vars(formula)[-1])

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
      return(utils.most_frequent_value(x))
    }
  }
  train_roll_fn <- function(var) zoo::rollapply(var, width=training_average_by_width, FUN=mean_fn, align='right', fill=NA)
  result <- result %>% group_by(city, poll) %>% arrange(date) %>%
    mutate_at(vars(-city, -poll, -date), train_roll_fn)
  #TODO check sky_code value is preserved (and not mixed with levels)

  # Remove rows with no (full) weather observation
  keep_only_full_observations <- TRUE
  if(keep_only_full_observations){
    result <- result %>% dplyr::filter_at(c(weather_vars, vars(value)), all_vars(!is.na(.)))
  }else{
    result <- result %>% dplyr::filter_at(c(weather_vars, vars(value)), any_vars(!is.na(.)))
  }

  # Standardize data
  result_std <- standardize(formula, result, groups=c(date))
  result_std_data <- result_std$data
  result_std_data$date <- result$date
  result_std_data$city <- result$city
  result_std_data$poll <- result$poll


  # Separate in training and prediction data and nest data by city and poll (still grouped by these two columns)
  result_std_data <- result_std_data %>% group_by(city, poll) %>% tidyr::nest() %>%
    dplyr::mutate(training = purrr::map(data, ~ filter(.x, date < training_prediction_cut))) %>%
    dplyr::mutate(predicting = purrr::map(data, ~ filter(.x, date >= training_prediction_cut)))

  # Remove those without training data
  no_training <- result_std_data %>% filter(purrr::map_int(training, nrow) == 0)
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
  result_std_data <- result_std_data %>% tidyr::crossing(models_df)
  result_std_data <- result_std_data %>% mutate(model_fitted=purrr::map2(training, model, ~(.y(.x, formula))))

  #----------------
  # Predict
  #----------------
  result_std_data <- result_std_data %>% mutate(training=purrr::map2(training, model_fitted, ~ .x %>% mutate(fitted=predict(.y, .x %>% select_at(weather_vars)))))
  result_std_data <- result_std_data %>% mutate(predicting=purrr::map2(predicting, model_fitted, ~ .x %>% mutate(predicted=predict(.y, .x %>% select_at(weather_vars)))))

  #---------------
  # Post compute
  #---------------
  result_std_data <- result_std_data %>% mutate(training=purrr::map(training, ~ .x %>% mutate(residuals=fitted-value)))
  result_std_data <- result_std_data %>% mutate(predicting=purrr::map(predicting, ~ .x %>% mutate(residuals=predicted-value)))
  result_std_data <- result_std_data %>% mutate(rmse=purrr::map_dbl(training, ~ Metrics::rmse(.x$value, .x$fitted)))
  return(result_std_data)
}

#-------------------
# Plot results
#-------------------
aq_weather.plot <- function(result,
                            plotting_average_by='day',
                            plotting_average_by_width=30,
                            training_prediction_cut=NULL,
                            subtitle=NULL,
                            filename=NULL
                            ){

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

  if(!is.null(subtitle)){
    plot <- plot + labs(subtitle=subtitle)
  }


  if(!is.null(filename)){
    ggsave(
      filename,
      plot = plot,
      device = "pdf",
      # path = file.path(getwd(),"results"),
      scale = 3,
      width = 20,
      height = 10,
      units = "cm",
      dpi = 320,
      limitsize = FALSE,
    )
  }


  return(plot)
}

aq_weather.plot_residuals <- function(result){
  fitted <- result %>% dplyr::select(city, poll, model_name, training) %>%
    tidyr::unnest(c(training)) %>% dplyr::select(city, poll, model_name, date, residuals) %>%
    tidyr::gather("type", "residuals", -c(city, poll, date, model_name))

  predicted <- result %>% dplyr::select(city, poll, model_name, predicting) %>%
    tidyr::unnest(c(predicting)) %>% dplyr::select(city,poll,model_name, date, residuals) %>%
    tidyr::gather("type", "residuals", -c(city,poll,date,model_name))

  combined <- rbind(fitted, predicted)
  plot <- ggplot(combined, aes(x=date, y=residuals)) + geom_line(aes(colour=type)) + facet_wrap(~ city + poll + model_name )

  if(!is.null(training_prediction_cut)){
    plot <- plot +
      geom_vline(xintercept=as.POSIXct(training_prediction_cut), linetype="dotted", color="blue")
  }
  return(plot)
}

