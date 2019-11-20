source('R/setup.r')

locations <- function(country = NULL){

  con = connection()

  filters <- list(country=country)
  filters <- Filter(Negate(is.null), filters)

  where_str <- pg_filters_to_str(filters)
  query_str <- paste("SELECT * from locations ", where_str)
  df <- dbGetQuery(con,query_str)
  return(df)
}


measurements <- function(country = NULL) {

  con = connection()

  filters <- list(country=country)
  filters <- Filter(Negate(is.null), filters)

  where_str <- pg_filters_to_str(filters)
  query_str <- paste("SELECT * from measurements left join locations on ARRAY[measurements.location] <@ (locations.names)", where_str)
  df <- dbGetQuery(con, query_str)
  return(df)
}



df_measurements <- measurements(country='IN')
df_locations <- locations(country='IN')
