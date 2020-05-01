
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
  devtools::install_github(url, force=T)
  library(rcrea)

  load_dot_env()
  appDir <- getAppDir()

  rsconnect::setAccountInfo(name='crea',
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))
  rsconnect::deployApp(appDir)
}
