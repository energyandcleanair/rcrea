
transport.tomtom_congestion <- function(cities, frequency="daily"){

  c <-  cities %>% dplyr::distinct(city, country)
  c$iso3 <- countrycode::countrycode(c$country, origin="iso2c", destination = "iso3c")
  c$url <- paste0("https://api.midway.tomtom.com/ranking/",frequency,"Stats/",c$iso3,"_",tolower(gsub(" ","-",c$city)))

  get_data <- function(city, country, url){
    print(url)
    tryCatch({
      result <- jsonlite::fromJSON(url) %>%
        dplyr::rename(value=congestion)
      result$city <- city
      result$country <- country
      return(result)
    }, error=function(c) {
      print(c)
      return(NA)
    })
  }

  datas <- mapply(get_data, c$city, country=c$country, url=c$url, SIMPLIFY=F)
  data <- do.call(dplyr::bind_rows, datas[!is.na(datas)])

  if(frequency=="weekly"){
    data <- data %>% dplyr::rename(date=weekStart)
  }
  data$date <- as.POSIXct(data$date)
  return(data)
}
