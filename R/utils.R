utils.most_frequent_value <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


utils.add_lag <- function(meas, cols, hour_lags){

  group_cols <- c('city', 'poll')
  result <- meas %>%
    group_by_at(vars(all_of(group_cols))) %>% arrange(date)
    for(hour_lag in hour_lags){
      print(paste("Adding ", hour_lag,"hour lag"))
      my_lag <- list(function(x) dplyr::lag(x, n=hour_lag))
      names(my_lag) <- paste(hour_lag) #will be appended to column name by mutate_at
      result <- result %>% mutate_at(cols,my_lag)
    }

  return(result)
}
