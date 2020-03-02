require(creadb)
require(maptools)
require(DT)
require(shinyWidgets)



locations <- creadb::locations(country=c("IN"), with_geometry=F)
standards <- creadb::standards(collect=T)
polls <- c(creadb::PM25, creadb::PM10, creadb::NO2, creadb::O3, creadb::SO2, creadb::CO)
averagings <- c("hour", "day", "week", "month", "year")

plot_types <- list("Time Series" = "ts",
                   "Time Series (overlaid years)" = "ts_year",
                   "Heatmap" = "heatmap",
                   "Heatmap (with text)" = "heatmap_w_text")

exc_status_breaks <- c(-Inf, 0, 0.5, 0.999, Inf)
exc_status_labels <- c("Not breached","Less than halfway through","More than halfway through", "Breached")
exc_status_colours <- c("#1a964128","#a6d96a28","#fdae6128", "#d7191c28")
