require(RPostgres)
# library(sp)
# library(rgdal)
require(DBI)
require(purrr)
require(dplyr)
require(dbplyr)
require(ggplot2)
require(gbm)
require(sf)
require(memoise)

# Constants for user: Pollutants
CO = "co"
PM25 = "pm25"
NO2 = "no2"
O3 = "o3"
PM10 = "pm10"
SO2 = "so2"
NO = "no"
NOX = "nox"

# Constants for connection
CONN_HOST = '34.77.246.210'
CONN_DBNAME = 'production'
CONN_USER = 'readonly'
CONN_PASSWORD = '2hNETPw5'
CONN_PORT = '5432'

pkg.globals <- new.env()
pkg.globals$CON <- NULL

poll_str <- function(poll){

  return(switch(poll,
         "co" = "CO",
         "pm25" = "PM2.5",
         "no2" = expression("NO"["2"]),
         "o3" = "Ozone",
         "pm10" = "PM10",
         "so2" = expression("SO"["2"])
  ))
}

connection_str <- function(){
  return(sprintf("PG:dbname='%s' host='%s' port='%s' user='%s' password='%s'",
                 CONN_DBNAME, CONN_HOST, CONN_PORT, CONN_USER, CONN_PASSWORD))
}

connection <- function(reconnect=FALSE) {


  if(!is.null(pkg.globals$CON) && (reconnect || !DBI::dbIsValid(pkg.globals$CON))){
    tryCatch({
      DBI::dbDisconnect(pkg.globals$CON)
    })
    pkg.globals$CON <- NULL
  }

  if(is.null(pkg.globals$CON)){
    pkg.globals$CON <- DBI::dbConnect(RPostgres::Postgres(), dbname = CONN_DBNAME,
                        host = CONN_HOST,
                        port = strtoi(CONN_PORT),
                        user = CONN_USER,
                        password = CONN_PASSWORD)
  }



  return(pkg.globals$CON)
}

tbl_safe <- function(con, query_initial){
  result <- tryCatch({
    dplyr::tbl(con, query_initial)
  },error = function(e) {
    print("Trying to reconnect")
    con <- connection(reconnect = TRUE)
    result <- dplyr::tbl(con, query_initial)
    return(result)
  })
  return(result)
}

# Caching
fc <- memoise::cache_filesystem("./cache")

