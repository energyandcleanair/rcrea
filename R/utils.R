utils.most_frequent_value <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


utils.add_lag <- function(meas, cols, hour_lags){

  # First ensure it is 'hour-complete'
  print("Completing hours before 'lagging'")
  date_grid <- meas %>% dplyr::group_by(city, poll) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by='hour'))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))

  meas_full <- merge(meas, date_grid, by = c('city','poll','date'), all=TRUE)

  group_cols <- c('city', 'poll')
  result <- meas_full %>%
    group_by_at(vars(all_of(group_cols))) %>% arrange(date)
    for(hour_lag in hour_lags){
      print(paste("Adding ", hour_lag,"hour lag"))
      my_lag <- list(function(x) dplyr::lag(x, n=hour_lag))
      names(my_lag) <- paste(hour_lag) #will be appended to column name by mutate_at
      result <- result %>% mutate_at(cols,my_lag)
    }


  return(result)
}

#' Year-on-year variations
#'
#' @param meas Measurements tibble
#' @param mode Either 'absolute' or 'relative'
#'
#' @return A tibble of measurements with value replaced by y-o-y variation
#' @export
#'
#' @examples
utils.yoy <- function(meas, mode="absolute"){
  meas$year <- lubridate::year(meas$date)
  meas$ydate <- meas$date
  lubridate::year(meas$ydate) <- 0

  res <- meas %>% dplyr::group_by_at(setdiff(names(meas), c("value", "year", "date"))) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(value = ifelse(year-dplyr::lag(year)==1,
                                 switch(mode,
                                        "absolute"=value-dplyr::lag(value),
                                        "relative"=(value-dplyr::lag(value))/dplyr::lag(value)
                                 ),
                                 NA)
    ) %>%
    dplyr::ungroup()

  if("unit" %in% colnames(res)){
    res <- res %>%
      dplyr::mutate(
        unit = switch(mode,
                      "absolute"= paste('Δ', meas$unit),
                      "relative"= "-"
        )
      )
  }
  return(res)
}

utils.rolling_average <- function(meas, average_by, average_width, vars_to_avg, group_by_cols, min_values=NULL){

  if(average_width==0){
    return(meas)
  }

  meas <- meas %>% dplyr::mutate(date=lubridate::floor_date(date,average_by))
  date_grid <- meas %>% dplyr::group_by_at(group_by_cols) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by=average_by) %>% trunc(units=average_by))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))

  meas <- merge(meas, date_grid, by = c(group_by_cols, "date"), all=TRUE)


  # Rolling mean
  mean_fn <- function(x, filter_min_values=T){
    if(filter_min_values
       && !is.null(min_values)
       && sum(!is.na(x)) < min_values
    ){
      return(NA)
    }
    if(is.numeric(x)){
      res <- mean(x, na.rm = T) # it sometimes returns NaN but models expect only NA
      return(if(is.na(res)) NA else res)
    }else{
      return(utils.most_frequent_value(x))
    }
  }

  train_roll_fn <- function(var) zoo::rollapply(var, width=average_width, FUN=mean_fn, align='right', fill=NA)
  # first average per date
  meas <- meas %>% dplyr::group_by_at(c(group_by_cols, "date")) %>%
    dplyr::summarise_at(vars_to_avg, mean_fn, filter_min_values=F)

  # then rolling average
  meas <- meas %>% dplyr::group_by_at(group_by_cols) %>% dplyr::arrange(date) %>%
    dplyr::mutate_at(vars_to_avg, train_roll_fn)
  return(meas)
}

utils.unnest_json <-function(.data,.json_col, ...){
  # build character vector whose names are cols to be created and values columns
  # to be extracted
  dots <- sapply(as.list(substitute(list(...)))[-1], as.character)
  .json_col <- as.character(substitute(.json_col))
  query0  <- sprintf("%s::json ->>'%s' as %s", .json_col, dots, names(dots))
  query <- sprintf("SELECT *, %s FROM (%s) AS PREV",
                   paste(query0, collapse = ", "),
                   dbplyr::sql_render(.data))
  dplyr::tbl(.data$src$con, dbplyr::sql(query))
}


