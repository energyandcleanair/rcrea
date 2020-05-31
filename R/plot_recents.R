
plot_recents <- function(folder, source, countries=NULL, city=NULL, aggregate_level="country", polls=NULL, subplot_by="poll", subfile_by="country",  runnings=c(0, 7, 14, 30)){


  width <- list("s"=8,"m"=12,"l"=16)
  height <- list("s"=6,"m"=9,"l"=12)
  expand <- list("s"=0.15, "m"=0.1, "l"=0.05)
  sources <- list("eea"="European Environment Agency",
                  "openaq"="OpenAQ",
                  "earthengine"="Sentinel-5P TROPOMI OFFL NO2",
                  "cpcb"="Central Pollution Control Board",
                  "mee"="Ministry of Ecology and Environment")

  meas <- rcrea::measurements(country=countries, city=city, poll=polls, aggregate_level=aggregate_level, source=source, with_metadata = T)

  subfiles <- switch(subfile_by,
                     "country"=unique(meas$country),
                     "city"=unique(meas$region_name),
                     "gadm1"=unique(meas$region_id),
                     "poll"=unique(meas$poll))

  meas[meas$unit=='mg/m3',]$value <- meas[meas$unit=='mg/m3',]$value*1000
  meas[meas$unit=='mg/m3',]$unit <- "µg/m3"

  for(subfile in subfiles){
    for(running in runnings){
      tryCatch({

        region_name <- switch(subfile_by,
                           "country"= countrycode::countrycode(subfile, origin="iso2c", destination = "country.name"),
                           "city"=subfile,
                           "gadm1"=subfile
                           )

        filtered_meas <- switch(subfile_by,
                                "country"= meas%>% dplyr::filter(country==subfile),
                                "city"= meas%>% dplyr::filter(region_name==subfile),
                                "gadm1"= meas%>% dplyr::filter(region_id==subfile),
                                "poll"= meas%>% dplyr::filter(poll==subfile)
        )

        country <- unique(filtered_meas$country)


        # Getting standard plot
        plt <- plot_measurements(filtered_meas,
                                        poll=polls,
                                        running_width=running,
                                        color_by = 'year',
                                        subplot_by = subplot_by)

        # Prettying it
        (plt_dl <- directlabels::direct.label(plt + theme_classic(),method = list(directlabels::dl.trans(y = y + .1), "top.bumptwice")) + theme_crea() + scale_size_manual(values=c(1), guide=F) +
            scale_color_brewer(limits=factor(seq(2020,min(2017,min(lubridate::year(meas$date))))), palette="Spectral") +
            theme(legend.position="right") +
            labs(
              title=paste("Air pollutant concentrations in",region_name),
              subtitle=if(running==0){NULL}else{paste0(running,"-day running average")},
              caption=paste0("Source: CREA based on ", sources[[source]],". Updated on ",format(Sys.Date(), format="%d %B %Y")))
        )

        for(size in names(width)){

          # Full version
          ggsave(file.path(folder, paste0(tolower(country),
                                          "_",source,
                                          ifelse(aggregate_level!="country",paste0("_",tolower(subfile)),""),
                                          "_full",ifelse(running==0,"",running),
                                          "_",size,".png")),
                 width=width[[size]], height=height[[size]],
                 plot=plt_dl +
                   scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, expand[[size]]))))

          # Version cut at current month end
          cutdate <- lubridate::date(paste(0,lubridate::month(lubridate::today()+lubridate::duration(1,"months")),1,sep="-"))
          ggsave(file.path(folder, paste0(tolower(country),
                                          "_",source,
                                          ifelse(aggregate_level!="country",paste0("_",tolower(subfile)),""),
                                          "_cut",ifelse(running==0,"",running),
                                          "_",size,".png")),
                 width=width[[size]], height=height[[size]],
                 plot=plt_dl + scale_x_datetime(date_labels = "%b", limits=c(as.POSIXct('0000-01-01'),as.POSIXct(cutdate))) +
                   scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, expand[[size]]))))

        }
      }, error=function(err){
        warning(paste("Failed for file",subfile,"-",err))
      })
    }
  }
}
