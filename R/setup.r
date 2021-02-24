# require(RPostgres)
# require(DBI)
# require(purrr)
# require(dplyr)
# require(dbplyr)
# require(ggplot2)
# require(gbm)
# require(sf)
# require(memoise)

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

sel <- dplyr::select

poll_str <- function(poll){

  return(dplyr::recode(poll,
         "co" = "CO",
         "pm25" = "PM2.5",
         "no2" = "NO2",
         "o3" = "Ozone",
         "pm10" = "PM10",
         "so2" = "SO2"
  ))
}

source_str <- function(source){

  return(dplyr::recode(source,
                       "eea"="European Environment Agency",
                       "openaq"="OpenAQ",
                       "openaq_government"="OpenAQ (Government)",
                       "openaq_community"="OpenAQ (Community)",
                       "openaq_research"="OpenAQ (Research)",
                       "earthengine"="Sentinel-5P TROPOMI OFFL NO2",
                       "cpcb"="Central Pollution Control Board",
                       "mee"="Ministry of Ecology and Environment",
                       "csb"="Ministry of Environment and Urban Planning",
                       "jp"="Japan Atmospheric Environmental Regional Observation System",
                       "airkorea"="Air Korea",
                       "defra"="DEFRA",
                       "aurn"="AURN",
                       "airvisual"="AirVisual",
                       .default = toupper(source)
                       ))
}

connection_str <- function(){
  return(sprintf("PG:dbname='%s' host='%s' port='%s' user='%s' password='%s'",
                 CONN_DBNAME, CONN_HOST, CONN_PORT, CONN_USER, CONN_PASSWORD))
}


connection <- function(current_connection=NULL, force_reconnect=FALSE) {

  # We keep only one connection alive
  if(!is.null(current_connection) && (current_connection != pkg.globals$CON)){
    tryCatch({
      DBI::dbDisconnect(pkg.globals$CON)
    })
    pkg.globals$CON <- current_connection
  }

  if(!is.null(pkg.globals$CON) && (force_reconnect || !DBI::dbIsValid(pkg.globals$CON))){
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
    con <- connection(force_reconnect = FALSE)
    result <- dplyr::tbl(con, query_initial)
    return(result)
  })
  return(result)
}

# Caching
# fc <- memoise::cache_filesystem("cache")
# .onLoad <- function(libname, pkgname){
#   print("Creating cached functions aq_weather.m.collect and ghcnd.m.ghcnd_stations")
#   aq_weather.m.collect <<- memoise::memoise(aq_weather.collect, cache=fc)
#   ghcnd.m.ghcnd_stations <<- memoise::memoise(rnoaa::ghcnd_stations, cache=fc)
# }

## work around bug in gbm 2.1.1
predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}
