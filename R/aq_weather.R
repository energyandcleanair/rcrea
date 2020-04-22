source('R/setup.r')

# require(rnoaa)
# require(e1071)
# require(memoise)
# require(standardize)
# require(Metrics)
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
                               use_worldmet=F,
                               aggregate_at_city_level=T,
                               weather_radius_km=20){

  con <- connection()
  # Get measurements
  meas <- measurements(city=city,
                       date_from=date_from,
                       collect=F, #to save time (only do collection at last step)
                       poll=poll,
                       average_by=average_by,
                       aggregate_at_city_level=aggregate_at_city_level,
                       add_noaa_station_ids = T,
                       noaa_station_radius_km = weather_radius_km,
                       with_metadata = T,
                       con = con)

  # Attach weather observations from ISD and aggregate by time & city
  if(use_worldmet){
    meas_weather <- weather.isd.join.using_worldmet(meas,
                                     measurements_averaged_by=average_by,
                                     aggregate_at_city_level=aggregate_at_city_level,
                                     radius_km=weather_radius_km)

  }else{
    meas_weather <- weather.isd.join(meas,
                                     measurements_averaged_by=average_by,
                                     con = con,
                                     collect=T)

  }

  # Adding sunshine
  meas_weather <- weather.sirad.join(meas_weather)

  # Attach precipitation from GHCND (ISD precipitation data seems very sparse)
  # meas_weather <- weather.ghcnd.join(meas_weather, weather_radius_km=weather_radius_km)

  return(meas_weather)
}


#---------------------
# Prediction functions
#---------------------
aq_weather.default_models <- function(){

  model_gbm <- function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm::gbm(
      formula = formula,
      data = training_data,
      cv.folds = 3,
      n.trees = 5000,
      #n.cores = 1, # parallel doesn't work on Ubuntu compute engine
      verbose = FALSE
    )
    print("Done")
    return(gbm.fit)
  }

  model_rpart <- function(training_data, formula){
    print("Training rpart")
    rpart.fit <- rpart::rpart(
      formula = formula,
      data = training_data,
      method = "anova"
    )
    print("Done")
    return(rpart.fit)
  }

  model_svr <- function(training_data, formula){
    print("Training svr")
    tuneResult <- e1071::tune(e1071::svm, formula,
                              data = training_data,
                              ranges = list(epsilon = seq(0,1,0.5), cost= 4))
    print("Done")
    return(tuneResult$best.model)
  }

  models <- list(gbm=model_gbm, rpart=model_rpart, svr=model_svr)
  return(models)
}

aq_weather.default_models_predict <- function(){

  gbm_predict <- function(model_fitted, data){
    return(predict(model_fitted, data))
  }

  rpart_predict <- function(model_fitted, data){
    return(predict(model_fitted, data))
  }

  svr_predict <- function(model_fitted, data){
    return(predict(model_fitted, data))
  }

  models_predict <- list(gbm=gbm_predict, rpart=rpart_predict, svr=svr_predict)
  return(models_predict)
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
  training_average_by='hour',
  training_average_by_width=3,
  training_average_value_only=F,
  models=aq_weather.default_models(),
  test_frac=0.1, #10% for testing purposes
  do_standardize=F #Not required for GBM and rpart,
){

  #----------------
  # Prepare data
  #----------------
  formula_vars <- vars(all.vars(formula))
  weather_vars <- vars(all.vars(formula)[-1])

  result <- meas_weather %>% select_at(c('city', 'poll', 'date', all.vars(formula)))

  # Make date axis homogeneous i.e. a row for every day / month / year per city and pollutant
  if(training_average_by_width>1){
    date_grid <- result %>% dplyr::group_by(city, poll) %>%
      dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
      dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by=training_average_by))) %>%
      dplyr::select(-c(date_min, date_max)) %>%
      tidyr::unnest(cols=c(date))

    result <- merge(result, date_grid, by = c('city','poll', 'date'), all=TRUE)

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
    if(training_average_value_only){
      result <- result %>% dplyr::group_by(city, poll) %>% dplyr::arrange(date) %>%
        dplyr::mutate_at(vars(value), train_roll_fn)
    }else{
      result <- result %>% dplyr::group_by(city, poll) %>% dplyr::arrange(date) %>%
        dplyr::mutate_at(vars(-city, -poll, -date), train_roll_fn)
      #TODO check sky_code value is preserved (and not mixed with levels)
    }
  }else{
    result <- result %>% group_by(city, poll) %>% arrange(date)
  }

  # Remove rows with no (full) weather observation
  keep_only_full_observations <- TRUE
  if(keep_only_full_observations){
    result <- result %>% dplyr::filter_at(c(weather_vars, vars(value)), all_vars(!is.na(.)))
  }else{
    result <- result %>% dplyr::filter_at(c(weather_vars, vars(value)), any_vars(!is.na(.)))
  }

  # Standardize data (no need when no svr)
  if(do_standardize){
    result_std <- standardize::standardize(formula, result)
    result_std_data <- result_std$data
    result_std_data$date <- result$date
    result_std_data$city <- result$city
    result_std_data$poll <- result$poll
  }else{
    result_std_data <- result
  }


  # Separate in training and testing data and nest data by city and poll (still grouped by these two columns)
  # TODO
  # result_std_data <- result_std_data %>% group_by(city, poll) %>% tidyr::nest() %>%
  #   dplyr::mutate(training = purrr::map(data, ~ filter(.x, date < training_prediction_cut))) %>%
  #   dplyr::mutate(testing = purrr::map(data, ~ filter(.x, date >= training_prediction_cut)))
  result_std_data <- result_std_data %>% group_by(city, poll) %>% mutate(id = row_number()) %>%
    tidyr::nest() %>%
    dplyr::mutate(training = purrr::map(data, ~ dplyr::sample_frac(.x, 1-test_frac))) %>%
    dplyr::mutate(testing = purrr::map2(data, training, ~ anti_join(.x, .y, by='id')))


  # Remove those without training data
  no_training <- result_std_data %>% filter(purrr::map_int(training, nrow) == 0)
  if(nrow(no_training)>0){
    warning("Some combinations have no training data")
    result_std_data <- result_std_data %>% filter(purrr::map_int(training, nrow) > 0)
  }

  #----------------
  # Train models
  #----------------
  # Adding models to tibble (each model is applied to each city, poll combination)
  model_names <- if (!is.null(names(models))) names(models) else seq_along(models)
  models_df <- tibble(model_name=names(models), model=models)
  result_std_data <- result_std_data %>% tidyr::crossing(models_df)
  result_std_data <- result_std_data %>% mutate(model_fitted=purrr::map2(training, model, purrr::possibly(~.y(.x %>% select_at(formula_vars), formula), otherwise = NA, quiet = FALSE)))

  #----------------
  # Predict
  #----------------
  # need to add  %>% select_at(weather_vars) for svr
  result_std_data <- result_std_data %>% mutate(training=purrr::map2(training, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(testing=purrr::map2(testing, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

  # Also predict on whole dataset for plotting purposes
  result_std_data <- result_std_data %>% mutate(data=purrr::map2(data, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

  #---------------
  # Post compute
  #---------------
  rsq <- function(x,y) cor(x, y) ^ 2
  result_std_data <- result_std_data %>% mutate(training=purrr::map(training, purrr::possibly(~ .x %>% mutate(residuals=predicted-value), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(testing=purrr::map(testing,  purrr::possibly(~ .x %>% mutate(residuals=predicted-value), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(rmse=purrr::map_dbl(training, purrr::possibly(~ Metrics::rmse(.x$value, .x$predicted), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(rmse_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::rmse(.x$value, .x$predicted), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(mae=purrr::map_dbl(training, purrr::possibly(~ Metrics::mae(.x$value, .x$predicted), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(mae_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::mae(.x$value, .x$predicted), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(rsq=purrr::map_dbl(training, purrr::possibly(~ rsq(.x$value, .x$predicted), otherwise = NA)))
  result_std_data <- result_std_data %>% mutate(rsq_test=purrr::map_dbl(testing, purrr::possibly(~ rsq(.x$value, .x$predicted), otherwise = NA)))
  return(result_std_data)
}

#-------------------
# Plot results
#-------------------
aq_weather.plot <- function(result,
                            plotting_average_by='day',
                            plotting_average_by_width=30,
                            title=NULL,
                            subtitle=NULL,
                            filename=NULL
                            ){

  #----------------
  # Shape results
  #----------------
  # 1-gather (i.e. wide -> narrow)
  # 2-rolling average
  roll_plot <- function(raw){
    result <- raw %>% dplyr::select(city, poll, model_name, rmse, date, type, value) %>%
      dplyr::mutate(date=lubridate::floor_date(date, unit = plotting_average_by)) %>%
      dplyr::group_by(city, poll, model_name, rmse, date, type) %>%
      dplyr::summarise(value=mean(value, na.rm = T)) %>% dplyr::ungroup() %>%
      dplyr::group_by(city, poll, model_name, rmse, type) %>% dplyr::arrange(date) %>%
      dplyr::mutate(value=zoo::rollapply(value, width=plotting_average_by_width,
                                    FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA)) %>%
      dplyr::ungroup()

    return(result)
  }


#
#   trained <- result %>% dplyr::select(city, poll, model_name, rmse, training) %>% filter(!is.na(training)) %>%
#     tidyr::unnest(c(training)) %>% dplyr::select(city, poll, model_name, rmse, date, value, predicted) %>%
#     tidyr::gather("type", "value", -c(city, poll, date, model_name, rmse)) %>% roll_plot()
#
#   tested <- result %>% dplyr::select(city, poll, model_name, rmse, testing) %>% filter(!is.na(testing)) %>%
#     tidyr::unnest(c(testing)) %>% dplyr::select(city, poll, model_name, rmse, date, value, predicted) %>%
#     tidyr::gather("type", "value", -c(city,poll,date,model_name, rmse)) %>% roll_plot()
  # combined <- rbind(trained, tested)

  plot_data <- result %>% dplyr::select(city, poll, model_name, rmse, data) %>% filter(!is.na(data)) %>%
    tidyr::unnest(c(data)) %>% dplyr::select(city, poll, model_name, rmse, date, value, predicted) %>%
    tidyr::gather("type", "value", -c(city,poll,date,model_name, rmse)) %>% roll_plot()

  plot <- ggplot(plot_data, aes(x=date, y=value)) +
    geom_line(aes(colour=type))

  if(length(unique(result$model_name))>1){
    plot <- plot + facet_grid(city + poll ~ model_name, scales="free_y")
    ratio_h_w <- nrow(unique(result[,c("poll","city")]))/length(unique(result$model_name))

  }else{
    plot <- plot + facet_grid(poll ~ city, scales="free_y")
    ratio_h_w <- length(unique(result$poll))/length(unique(result$city))
  }

  if(!is.null(title)){
    plot <- plot + labs(title=title)
  }

  if(!is.null(subtitle)){
    plot <- plot + labs(subtitle=subtitle)
  }


  # Adding rmse label
  plot <- plot + geom_text(data=result %>% mutate(label=paste0("R2: ",sprintf("%.2f",rsq),"\n","R2 (test): ",sprintf("%.2f",rsq_test))),
   aes(x = as.POSIXct('2015-01-01'), y = -Inf, label=label),
   hjust   = 0,
   vjust   = -1,
  size=3)

  plot <- plot + theme_light()

  if(!is.null(filename)){
    ggsave(
      filename,
      plot = plot,
      device = "pdf",
      # path = file.path(getwd(),"results"),
      scale = 2,
      width = 20,
      height = ratio_h_w * 20 / 2,
      units = "cm",
      dpi = 320,
      limitsize = FALSE,
    )
  }


  return(plot)
}

aq_weather.plot_residuals <- function(result){
  trained <- result %>% dplyr::select(city, poll, model_name, training) %>%
    mutate(training=purrr::map(training, ~dplyr::select(.x, date, residuals))) %>%
    tidyr::unnest(c(training)) %>% dplyr::select(city, poll, model_name, date, residuals) %>%
    tidyr::gather("type", "residuals", -c(city, poll, date, model_name))

  tested <- result %>% dplyr::select(city, poll, model_name, testing) %>%
    mutate(testing=purrr::map(testing, ~dplyr::select(.x, date, residuals))) %>%
    tidyr::unnest(c(testing)) %>% dplyr::select(city,poll,model_name, date, residuals) %>%
    tidyr::gather("type", "residuals", -c(city,poll,date,model_name))

  combined <- rbind(trained, tested)
  # Residuals as a function of the hour of the day
  combined$hour <- factor(lubridate::hour(combined$date), ordered=T)
  plot <- ggplot(combined, aes(x=hour, y=residuals)) + geom_boxplot() + facet_grid(poll ~ city, scales='free_y')
  plot

  # Residuals as a function of the day of the year
  combined$doy <- factor(lubridate::yday(combined$date), ordered=T)
  plot <- ggplot(combined, aes(x=doy, y=residuals)) + geom_boxplot(outlier.shape=NA) + facet_grid(poll ~ city, scales='free_y')
  plot


  # if(!is.null(training_prediction_cut)){
  #   plot <- plot +
  #     geom_vline(xintercept=as.POSIXct(training_prediction_cut), linetype="dotted", color="blue")
  # }
}

