
plot_recents <- function(folder, source, countries){

  width <- list("s"=10,"m"=15,"l"=20)
  height <- list("s"=6,"m"=9,"l"=12)


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
    (plt_dl <- directlabels::direct.label(plt + theme_classic(),method = list(directlabels::dl.trans(y = y + .1), "top.bumptwice")) + theme_crea() + scale_size_manual(values=c(1), guide=F) +
      scale_color_brewer(palette="Spectral", type='qual') + theme(legend.position="right") +
      labs(
        title=paste("Average pollutant concentrations in",country_name,"per year"),
        subtitle="30-day running average",
        caption=paste("Source: CREA based on", sources[[source]]))
      +scale_x_datetime(date_labels = "%b", limits=c(as.POSIXct('0000-01-01'),as.POSIXct('0000-06-01')))
      )

  for(size in names(width)){
    ggsave(file.path(folder, paste0(country_,"_",source,"_",size,".png")), width=width[[size]], height=height[[size]], plot=plt_dl)

  }

  }
}
