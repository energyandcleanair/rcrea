
getAppDir <- function(){
  appDir <- system.file("shiny", package = "creadb")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `creadb`.", call. = FALSE)
  }
  return(appDir)
}

#' @export
runShinyApp <- function() {
  appDir <- getAppDir()
  shiny::runApp(appDir, display.mode = "normal")
}

deployShinyApp <- function() {
  require('rsconnect')
  require('dotenv')
  library(devtools)
  url <- "https://github.com/hubert-thieriot/crea_r_package"
  devtools::install_github(url = url)

  load_dot_env()
  appDir <- getAppDir()

  rsconnect::setAccountInfo(name='crea',
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))
  rsconnect::deployApp(appDir)
}
