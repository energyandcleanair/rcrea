require(creadb)
require(DT)
require(shinyWidgets)
library(shinycssloaders)
library(countrycode)

locations <- creadb::locations(keep_only_for_dashboard=T, with_geometry=F)
countries <- unique(locations$country)
countries <- countries[!is.na(countries)]
names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name', custom_match = list(XK='Kosovo')))
countries <- countries[!is.na(names(countries))]

wholecountry_name <- '--- Whole Country ---'
standards <- creadb::standards(collect=T)
sources <- c("cpcb", "openaq", "eea")
polls <- c(creadb::PM25, creadb::PM10, creadb::NO2, creadb::O3, creadb::SO2, creadb::CO)
averagings <- c("hour", "day", "week", "month", "year")

plot_types <- list("Time Series" = "ts",
                   "Time Series (overlaid years)" = "ts_year",
                   "Heatmap" = "heatmap",
                   "Heatmap (with text)" = "heatmap_w_text")

exc_status_breaks <- c(-Inf, 0, 0.5, 0.999, Inf)
exc_status_labels <- c("Not breached","Less than halfway through","More than halfway through", "Breached")
exc_status_colours <- c("#1a964128","#a6d96a28","#fdae6128", "#d7191c28")
