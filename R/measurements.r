source('setup.r')



measurements <- function(country = NULL) {
  con = connection()

  filters <- list(country=country)
  filters <- Filter(Negate(is.null), filters)


  where_str <- pg_filters_to_str(filters)
  query_str <- paste("SELECT * from measurements left join locations on measurements.location = ANY(locations.names)", where_str)
  df <- dbGetQuery(con,query_str)

  return(df)
}



df <- measurements(country='IN')
