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
                      "absolute"= paste(utils.Delta(), meas$unit),
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
  if(!"date" %in% names(x)){
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
  meas <- meas %>%
    dplyr::group_by_at(group_by_cols) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(vars_to_avg, train_roll_fn) %>%
    dplyr::ungroup()
  return(meas)
}

utils.running_average <- function(m,
                                  average_width,
                                  average_by="day",
                                  vars_to_avg="value",
                                  group_by_cols=NULL,
                                  min_values=NULL){
  return(utils.rolling_average(m,
                               average_by = average_by,
                               average_width = average_width,
                               vars_to_avg = vars_to_avg,
                               group_by_cols = group_by_cols,
                               min_values = min_values))
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

#' Get lockdown and internal movement restriction stages in certain regions
#'
#' @param region_ids either iso2s of countries or for subnational level, ISO2_STATEID e.g. US_CA
#'
#' @return
#' @export
#'
#' @examples
utils.lockdown_stages <- function(region_ids=NULL){



  oxgrt <- read.csv(url("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  oxgrt$date <- strptime(oxgrt$Date,"%Y%m%d")
  oxgrt$CountryCode <- countrycode::countrycode(oxgrt$CountryCode, "iso3c", "iso2c")

  d <- oxgrt %>%
    dplyr::filter(
      (CountryCode %in% region_ids & Jurisdiction=="NAT_TOTAL") |
      (RegionCode %in% region_ids & Jurisdiction=="STATE_TOTAL")) %>%
    dplyr::mutate(region_id=ifelse(Jurisdiction=="NAT_TOTAL",CountryCode,RegionCode)) %>%
    dplyr::mutate(
      # This is where we define phases
      lockdown=C6_Stay.at.home.requirements,
      restriction=C7_Restrictions.on.internal.movement) %>%
    dplyr::select(region_id, date, lockdown, restriction) %>%
    tidyr::pivot_longer(c(lockdown, restriction), names_to="indicator", values_to="level")

  d %>% dplyr::group_by(region_id, indicator) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(phase=cumsum(c(1, diff(level) != 0))) %>%
    dplyr::filter(level>0) %>%
    dplyr::group_by(region_id, indicator, level, phase) %>%
    dplyr::summarise(date_from=min(date), date_to=max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(phase))
}

utils.add_lockdown <- function(meas){

  if(! "country" %in% colnames(meas)){
    warning("Missing country information in measurements. Can't add Lockdown information")
    return(meas)
  }
  lockdown <-readr::read_csv(system.file("extdata", "lockdowns.csv", package = "rcrea")) %>%
  # lockdown <- read.csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vTKMedY9Mzy7e81wWU95Ent79Liq7UwbUz0qTQbkSeAmFCPfqIVNbl1zs99bUOgsJUJbz53GxvBfeiP/pub?gid=0&single=true&output=csv'))
    dplyr::rename(source_lockdown=source)
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

utils.to_source_city <- function(source, city){
  lapply(source, function(x) city) %>% setNames(source)
}

utils.add_city_pop <- function(m){
  m.pop <- m %>%
    dplyr::distinct(location_id, location_name, country, geometry) %>%
    dplyr::left_join(
      tibble::tibble(maps::world.cities) %>%
        sf::st_as_sf(coords=c("long", "lat"), crs=4326) %>%
        tibble::tibble() %>%
        dplyr::mutate(country=countrycode::countrycode(country.etc, origin="country.name","destination"="iso2c"),
                      location_name=name) %>%
        dplyr::select(location_name, country, pop, geometry.city=geometry),
      by=c("location_name", "country")
    )

  dist <- function(g1,g2){
    sf::st_distance(sf::st_centroid(g1), g2)[1] %>% as.numeric()
  }

  # Only keep the closest cities within 100km
  m.pop %>%
    dplyr::mutate(distance=purrr::map2_dbl(geometry, geometry.city, dist)) %>%
    dplyr::group_by(location_id) %>%
    dplyr::arrange(distance) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(distance<=1E5) %>%
    dplyr::select(location_id, country, pop) %>%
    dplyr::right_join(m, c("location_id", "country"))

}


utils.Delta <- function(){
  stringi::stri_unescape_unicode("\u0394")
}


#' Keep top n values, and group the remaining ones as "others"
#'
#' @param data
#' @param group_col
#' @param value_col
#' @param n
#' @param others
#' @param factor
#'
#' @return
#' @export
#'
#' @examples
utils.keep_top_n <- function(data, group_col, value_col="value", n = 10, others = "Others", factor=T) {

  # Determine the top n groups based on the sum of the value column
  top_n <- data %>%
    group_by_at(group_col) %>%
    summarise(value_sum = sum(!!sym(value_col), na.rm = TRUE)) %>%
    arrange(desc(value_sum)) %>%
    head(n) %>%
    pull(!!sym(group_col))

  # Replace non-top n groups with "others" and make it a factor with ordered levels
  result <- data %>%
    mutate(!!group_col := ifelse(!!sym(group_col) %in% top_n, !!sym(group_col), others))

  if(factor){
    result <- result %>%
      mutate(!!group_col := factor(!!sym(group_col), levels = unique(c(top_n, others))))
  }

  return(result)
}
