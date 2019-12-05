source('R/setup.r')

library(dbplyr)
library(dplyr)


filter_sanity <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% filter(value > 0) %>% filter(!is.na(location_id))
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}

locations <- function(country=NULL, city=NULL, collect=TRUE){

  # Variable names must be different to column names
  country_ <- country
  city_ <- city

  # Get whole table
  con = connection()
  result <- dplyr::tbl(con, "locations")

  # Apply filters
  result <- switch(toString(length(country_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(country == country_), # Single country name
                   result %>% filter(country %in% country_) # Vector of country names
  )

  result <- switch(toString(length(city_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(city == city_), # Single city name
                   result %>% filter(city %in% city_) # Vector of city names
  )

  if(collect){
    result <- result %>% collect()
  }

  return(result)

}


measurements <- function(country=NULL,
                         city=NULL,
                         location_id=NULL,
                         poll=NULL,
                         date_from='2018-01-01',
                         average_by='day',
                         collect=TRUE) {

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  poll_ <- poll
  location_id_ <- location_id

  # Get wide measurements table first
  con = connection()
  # result <- dplyr::tbl(con, "measurements_daily")
  # wide_tbl_query <- dbplyr::sql("SELECT * FROM measurements LEFT JOIN
  #                         (SELECT id as id_location, name, names, city, cities, country FROM locations) as locations
  #                       ON ARRAY[measurements.location] <@ (locations.names)");
  # result <- tbl(con, wide_tbl_query)
  tryCatch({
    result <- dplyr::tbl(con, "measurements_daily")
  },error = function(e) {
    con <- connection(reconnect = TRUE)
    result <- dplyr::tbl(con, "measurements_daily")
  })

  # Apply filters
  result <- switch(toString(length(country_)),
        "0" = result, # NULL
        "1" = result %>% filter(tolower(country) == tolower(country_)), # Single value
        result %>% filter(country %in% country_) # Vector
  )

  city_ = tolower(city_)
  result <- switch(toString(length(city_)),
         "0" = result, # NULL
         "1" = result %>% filter(tolower(city) == city_), # Single value
         result %>% filter(tolower(city) %in% city_) # Vector
  )

  poll_ <- tolower(poll_)
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(pollutant) == poll_), # Single value
                   result %>% filter(tolower(pollutant) %in% poll_) # Vector
  )

  result <- switch(toString(length(location_id_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(location_id == location_id_), # Single value
                   result %>% filter(location_id %in% location_id_) # Vector
  )


  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% filter(date >= date_from)
  )


  result <- filter_sanity(result)

  result <- result %>% rename(poll = pollutant)

  # Apply time and location aggregation
  # measurements_daily is already aggregated by day, so we only aggregate further if <> 'day'
  if(average_by != 'day'){
    result <- result %>% group_by(city, location, location_id, date=DATE_TRUNC(average_by, date), poll) %>%
                      summarize(value=avg(value)) %>% ungroup()
  }

  if(collect){
    result <- result %>% collect()
  }

  return(result)
}


#
# measurements <- function(city){
#
#   con = connection()
#
#   query_str <- sprintf("SELECT DATE_TRUNC('day', date) AS date, parameter, avg(value) AS value, unit, city from measurements
#                       LEFT JOIN locations ON ARRAY[measurements.location] <@ locations.names
#                       WHERE LOWER(city)=LOWER('%s')
#                       GROUP BY DATE_TRUNC('day', date), parameter, unit, city", city)
#
#   df <- DBI::dbGetQuery(con, query_str)
#   return(df)
# }
#

