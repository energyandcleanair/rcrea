require(rcrea)
require(DT)
require(shinyWidgets)
library(shinycssloaders)
library(countrycode)

Sys.setenv("GCS_AUTH_FILE"=Sys.getenv("GCS_AUTH_FILE", "keys/gcs.shiny.auth.json"))
Sys.setenv("GCS_DEFAULT_BUCKET"=Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public"))
library(googleCloudStorageR)

locations <- rcrea::locations(level=c("city","station"), with_metadata=T, with_geometry=F, collect=T)

countries <- unique(locations$country)
countries <- countries[!is.na(countries)]
names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name', custom_match = list(XK='Kosovo')))
countries <- countries[!is.na(names(countries))]

wholecountry_name <- '--- Whole Country ---'
standards <- rcrea::standards(collect=T)
sources <- c("openaq_government", "openaq_community", "openaq_research", "cpcb", "eea",  "mee", "csb", "defra", "aurn", "airvisual","jp", "airkorea",  "earthengine")
polls <- c(rcrea::PM25, rcrea::PM10, rcrea::NO2, rcrea::O3, rcrea::SO2, rcrea::CO)

noaveraging_name <- "none"
averagings <- c("hour", "day")

plot_types <- list("Time Series" = "ts",
                   "Time Series (overlaid years)" = "ts_year",
                   "Year-on-year" = "yoy",
                   "Year-on-year (overlaid years)" = "yoy_year",
                   "Heatmap" = "heatmap",
                   "Heatmap (with text)" = "heatmap_w_text")

exc_status_breaks <- c(-Inf, 0, 0.5, 0.999, Inf)
exc_status_labels <- c("Not breached","Less than halfway through","More than halfway through", "Breached")
exc_status_colours <- c("#1a964128","#a6d96a28","#fdae6128", "#d7191c28")

processes <- rcrea::processes()


# trajectories ------------------------------------------------------------
trajs.bucket <- "crea-public"
trajs.bucket_base_url <- "https://storage.googleapis.com/crea-public/"
trajs.folder <- "data/trajectories"
