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
      result <- result %>% dplyr::mutate_at(cols,my_lag)
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

#' Fill time series with NAs at regular interval
#'
#' @param x
#' @param average_by
#' @param vars_to_avg
#' @param group_by_cols
#'
#' @return
#' @export
#'
#' @examples
utils.fill_ts <- function(x,
                            average_by="day",
                            vars_to_avg="value",
                            group_by_cols=NULL){
  if(!"date" %in% x){
    stop("Data should contain a date column")
  }

  if(is.null(group_by_cols)){
    group_by_cols <- setdiff(colnames(meas), c(vars_to_avg, "date"))
  }

  x <- x %>% dplyr::mutate(date=lubridate::floor_date(date,average_by))
  date_grid <- x %>% dplyr::group_by_at(group_by_cols) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by=average_by) %>% trunc(units=average_by))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))

  x <- merge(x, date_grid, by = c(group_by_cols, "date"), all=TRUE)
  return(x)
}

utils.rolling_average <- function(meas,
                                  average_by,
                                  average_width,
                                  vars_to_avg,
                                  group_by_cols=NULL,
                                  min_values=NULL){

  if(average_width==0){
    return(meas)
  }

  if(is.null(group_by_cols)){
    group_by_cols <- setdiff(colnames(meas), c(vars_to_avg, "date"))
  }

  meas <- utils.fill_ts(meas,
                        average_by=average_by,
                        vars_to_avg=vars_to_avg,
                        group_by_cols=group_by_cols)


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



utils.add_lockdown <- function(meas){

  if(! "country" %in% colnames(meas)){
    warning("Missing country information in measurements. Can't add Lockdown information")
    return(meas)
  }

  lockdown <- read.csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vTKMedY9Mzy7e81wWU95Ent79Liq7UwbUz0qTQbkSeAmFCPfqIVNbl1zs99bUOgsJUJbz53GxvBfeiP/pub?gid=0&single=true&output=csv'))
  lockdown$movement <- strptime(lockdown$movement_national,"%Y%m%d")
  lockdown$school <- strptime(lockdown$school,"%Y%m%d")
  lockdown$workplace <- strptime(lockdown$workplace,"%Y%m%d")
  lockdown$movement0 <- lockdown$movement
  lockdown$school0 <- lockdown$school
  lockdown$workplace0 <- lockdown$workplace

  lockdown$partial_restriction <- pmin(lockdown$school,lockdown$workplace, na.rm=T)
  lockdown$partial_restriction0 <- lockdown$partial_restriction

  lubridate::year(lockdown$movement0) <- 0
  lubridate::year(lockdown$school0) <- 0
  lubridate::year(lockdown$workplace0) <- 0
  lubridate::year(lockdown$partial_restriction0) <- 0

  lockdown$school_workplace <- pmin(lockdown$school, lockdown$workplace, na.rm=T)
  lockdown$school_workplace0 <- pmin(lockdown$school0, lockdown$workplace0, na.rm=T)

  lockdown$first_measures <- pmin(lockdown$partial_restriction, lockdown$movement, na.rm=T)
  lockdown$first_measures0 <- pmin(lockdown$partial_restriction0, lockdown$movement0, na.rm=T)

  lockdown$iso2 <- countrycode::countrycode(lockdown$iso3, origin='iso3c', destination='iso2c')

  meas %>% dplyr::left_join(lockdown, by=c("country"="iso2"))
}
