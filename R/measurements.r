source('R/setup.r')

locations <- function(country = NULL){

  con = connection()

  filters <- list(country=country)
  filters <- Filter(Negate(is.null), filters)

  where_str <- filters_to_pg_str(filters)
  query_str <- paste("SELECT * from locations ", where_str)
  df <- DBI::dbGetQuery(con,query_str)
  return(df)
}


measurements <- function(country = NULL) {

  con = connection()

  filters <- list(country=country)
  filters <- Filter(Negate(is.null), filters)

  where_str <- filters_to_pg_str(filters)
  query_str <- paste("SELECT * from measurements left join locations on ARRAY[measurements.location] <@ (locations.names)", where_str)
  df <- DBI::dbGetQuery(con, query_str)
  return(df)
}

