
getAppDir <- function(){
  appDir <- system.file("shiny", package = "rcrea")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `rcrea`.", call. = FALSE)
  }
  return(appDir)
}

#' @export
runShinyApp <- function() {
  appDir <- getAppDir()
  shiny::runApp(appDir, display.mode = "normal")
}

deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  url <- "https://github.com/energyandcleanair/rcrea"
  devtools::install_github(url, force=T, upgrade="never")
  library(rcrea)

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))
  # appDir <- getAppDir()

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # We could deploy like this:
  # rsconnect::deployApp(appDir)
  # but it would miss the auth file that is not pushed to Github

  rsconnect::deployApp("inst/shiny",
                       appName="airquality",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = TRUE)

}
