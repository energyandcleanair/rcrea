source('R/setup.r')

library(dbplyr)
library(dplyr)


filter_sanity <- function(result){
  # Filters out measurements that are obviously wrong
  result <- result %>% filter(value >= 0)
  # result <- result %>% filter(parameter != 'o3' || value > -9999)
  return(result)
}

locations <- function(country=NULL, city=NULL, collect=TRUE){

  # Variable names must be different to column names
  country_ <- country
  city_ <- city

  # Get whole table
  con = connection()
  result <- tbl(con, "locations")

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
                         location=NULL,
                         poll=NULL,
                         date_from='2018-01-01',
                         average_by='day',
                         collect=TRUE) {

  # Variable names must be different to column names
  country_ <- country
  city_ <- city
  poll_ <- poll
  location_ <- location

  # Get wide measurements table first
  con = connection()
  # wide_tbl_query <- dbplyr::sql("SELECT * FROM measurements LEFT JOIN
  #                         (SELECT id as id_location, name, names, city, cities, country FROM locations) as locations
  #                       ON ARRAY[measurements.location] <@ (locations.names)");
  # result <- tbl(con, wide_tbl_query)
  result <- dplyr::tbl(con, "measurements")

  # Apply filters

  result <- switch(toString(length(country_)),
        "0" = result, # NULL
        "1" = result %>% filter(tolower(country) == tolower(country_)), # Single country name
        result %>% filter(country %in% country_) # Vector of country names
  )

  city_ = tolower(city_)
  result <- switch(toString(length(city_)),
         "0" = result, # NULL
         "1" = result %>% filter(tolower(city) == city_), # Single city name
         result %>% filter(tolower(city) %in% city_) # Vector of city names
  )

  poll_ <- tolower(poll_)
  result <- switch(toString(length(poll_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(parameter) == poll_), # Single city name
                   result %>% filter(tolower(parameter) %in% poll_) # Vector of city names
  )

  location_ <- tolower(location_)
  result <- switch(toString(length(location_)),
                   "0" = result, # NULL
                   "1" = result %>% filter(tolower(location) == location_), # Single city name
                   result %>% filter(tolower(location) %in% location_) # Vector of city names
  )


  result <- switch(toString(length(date_from)),
                   "0" = result, # NULL
                   "1" = result %>% filter(date >= date_from),
  )


  result <- filter_sanity(result)

  result <- result %>% rename(poll = parameter)

  # Apply time aggregation
  result <- result %>% group_by(city, location, date=DATE_TRUNC(average_by, date), poll, unit) %>%
                      summarize(value=avg(value)) %>% ungroup()

  if(collect){
    result = tryCatch({
      result %>% collect()
    },error = function(e) {
      con <- connection(reconnect = TRUE)
      return(measurements(country,
                          city,
                          location,
                          poll,
                          date_from,
                          average_by,
                          collect))
    })
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

