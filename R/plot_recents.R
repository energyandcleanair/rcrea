
plot_recents <- function(folder, source, countries){

  width <- 10
  height <- 6


  sources <- list("eea"="European Environment Agency", "openaq"="OpenAQ")

  meas <- rcrea::measurements(country=countries, aggregate_level='country', source=source)
  countries <- unique(meas$country)

  meas[meas$unit=='mg/m3',]$value <- meas[meas$unit=='mg/m3',]$value*1000
  meas[meas$unit=='mg/m3',]$unit <- "µg/m3"

  for(country_ in countries){
    country_name <- countrycode::countrycode(country_, origin="iso2c", destination = "country.name")

    # Getting standard plot
    plt <- plot_measurements(meas%>% dplyr::filter(country==country_), running_width=30, color_by = 'year', subplot_by = c("poll"))

    # Prettying it
    (plt_dl <- directlabels::direct.label(plt + theme_classic(),method = list(dl.trans(y = y + .1), "top.bumptwice")) + theme_crea() + scale_size_manual(values=c(1), guide=F) +
      scale_color_brewer(palette="Spectral", type='qual') + theme(legend.position="right") +
      labs(
        title=paste("Average pollutant concentrations in",country_name,"per year"),
        subtitle="30-day running average",
        caption=paste("Source: CREA based on", sources[[source]]))
      +scale_x_datetime(date_labels = "%b", limits=c(as.POSIXct('0000-01-01'),as.POSIXct('0000-06-01')))
      )

    ggsave(file.path(folder, paste0(country_,"_",source,".png")), width=width, height=height, plot=plt_dl)

  }
}
