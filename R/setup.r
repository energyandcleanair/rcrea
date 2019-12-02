require("RPostgres")
require("purrr")
require("dplyr")
require("dbplyr")

library(DBI)
library(purrr)
library(dplyr)
library(dbplyr)

# Constants for user
CO = "co"
PM25 = "pm25"
NO2 = "no2"
O3 = "o3"
PM10 = "pm10"
SO2 = "so2"


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

connection <- function(reconnect=FALSE) {
  # Connect to a specific postgres database i.e. Heroku
  if(reconnect && !is.null(pkg.globals$CON)){
    dbDisconnect(pkg.globals$CON)
  }

  if(is.null(pkg.globals$CON)){

    pkg.globals$CON <- DBI::dbConnect(RPostgres::Postgres(), dbname = 'production',
                        host = '34.77.246.210',
                        port = 5432, # or any other port specified by your DBA
                        user = 'postgres',
                        password = 'sB7ynLPiOnnCAEIl')
  }

  return(pkg.globals$CON)
}


