
get_app_dir <- function(){
  appDir <- system.file("shiny", package = "rcrea")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `rcrea`.", call. = FALSE)
  }
  return(appDir)
}

#' @export
run_shiny <- function() {
  appDir <- get_app_dir()
  shiny::runApp(appDir, display.mode = "normal")
}

#' @export
deploy_shiny <- function() {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  urls <- c(
    "tidyverse/lubridate",
    "energyandcleanair/leaflet.extras2",
    "energyandcleanair/rcrea")

  devtools::install_github(urls, force=F, upgrade="never")

  library(lubridate)
  library(leaflet.extras2)
  library(rcrea)

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # We could deploy like this:
  # rsconnect::deployApp(get_app_dir())
  # but it would miss the auth file that is not pushed to Github

  rsconnect::deployApp("inst/shiny",
                       appName="airquality",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = TRUE)

}
