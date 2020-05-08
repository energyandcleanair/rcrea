
plot_recents <- function(folder, source, countries){

  width <- list("s"=10,"m"=15,"l"=20)
  height <- list("s"=6,"m"=9,"l"=12)
  expand <- list("s"=0.15, "m"=0.1, "l"=0.05)
  sources <- list("eea"="European Environment Agency", "openaq"="OpenAQ")

  meas <- rcrea::measurements(country=countries, aggregate_level='country', source=source)
  countries <- unique(meas$country)

  meas[meas$unit=='mg/m3',]$value <- meas[meas$unit=='mg/m3',]$value*1000
  meas[meas$unit=='mg/m3',]$unit <- "µg/m3"

  for(country_ in countries){

    tryCatch({
      country_name <- countrycode::countrycode(country_, origin="iso2c", destination = "country.name")

      # Getting standard plot
      plt <- rcrea::plot_measurements(meas%>% dplyr::filter(country==country_), running_width=30, color_by = 'year', subplot_by = c("poll"))

      # Prettying it
      (plt_dl <- directlabels::direct.label(plt + theme_classic(),method = list(directlabels::dl.trans(y = y + .1), "top.bumptwice")) + theme_crea() + scale_size_manual(values=c(1), guide=F) +
          scale_color_brewer(palette="Spectral", type='div') + theme(legend.position="right") +
          labs(
            title=paste("Air pollutant concentrations in",country_name),
            subtitle="30-day running average",
            caption=paste("Source: CREA based on", sources[[source]]))
      )

      for(size in names(width)){

        # Full version
        ggsave(file.path(folder, paste0(tolower(country_),"_",source,"_full30_",size,".png")),
               width=width[[size]], height=height[[size]],
               plot=plt_dl +
                 scale_y_continuous(expand = expansion(mult = c(0, expand[[size]]))))

        # Version cut at current month end
        cutdate <- lubridate::date(paste(0,lubridate::month(lubridate::today()+lubridate::duration(1,"months")),1,sep="-"))
        ggsave(file.path(folder, paste0(tolower(country_),"_",source,"_cut30_",size,".png")),
               width=width[[size]], height=height[[size]],
               plot=plt_dl + scale_x_datetime(date_labels = "%b", limits=c(as.POSIXct('0000-01-01'),as.POSIXct(cutdate))) +
                 scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, expand[[size]]))))

      }
    }, error=function(err){
      warning(paste("Failed for country",country_,"-",err))
    })

  }
}
