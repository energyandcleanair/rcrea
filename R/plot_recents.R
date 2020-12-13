

#' Plotting
#'
#' @param folder
#' @param source
#' @param countries
#' @param city
#' @param process_id
#' @param aggregate_level
#' @param polls
#' @param subplot_by
#' @param subfile_by
#' @param running_days
#' @param add_lockdown
#'
#' @return
#' @export
#'
#' @examples
#'
plot_recents <- function(
  source=NULL,
  meas_raw=NULL,
  folder=NULL,
  poll=NULL,
  unit=NULL,
  aggregate_level="city",
  country=NULL,
  city=NULL,
  source_city=NULL, #If not null, replaces source and city list(source1=c(city1,city2), source2=c(city3,city4))
  process_id=NULL,
  running_days=c(0, 7, 14, 30),
  color_by='location_name',
  subplot_by="poll",
  subfile_by="country",
  type="ts",
  percent=FALSE, # Use scales::percent
  title=NULL,
  subtitle=NULL,
  caption=NULL,
  date_from="2015-01-01",
  add_to_ggplot=NULL,
  add_lockdown=F,
  range=c("full"), # cut: only up to current date
  size=c("s","m","l"),
  years=NULL,
  file_suffix=NULL){


  if(is.null(source) && !is.null(meas_raw) && length(unique(meas_raw$source))==1){
    source <- unique(meas_raw$source)
  }

  if(is.null(poll) && !is.null(meas_raw) && length(unique(meas_raw$poll))==1){
    poll <- unique(meas_raw$poll)
  }



  build_filename <- function(source, subfile, full_cut, aggregate_level, running, size, add_lockdown, type, suffix){

    paste0(source,
           "_",
           aggregate_level,
           "_",
           subfile,
           "_",
           full_cut,
           ifelse(running==0,"",running),
           ifelse(add_lockdown,"_lockdown",""),
           ifelse(type=="ts","",paste0("_",gsub("-","",type))),
           "_",size,
           ifelse(is.null(suffix),"",paste0("_",suffix)),
           ".png")
  }

  build_title <- function(title, subfile_by, subfile, running){

    if(subfile_by=="poll"){
      title_ <- ifelse(is.null(title),
                       paste(rcrea::poll_str(subfile),"pollutant levels"),
                       sub("\\{poll\\}", rcrea::poll_str(subfile), title)
                       )
      return(title_)
    }

    return(paste("Air pollutant concentrations in",subfile))
  }

  build_subtitle <- function(subtitle, subfile_by, subfile, running, type){
    s <- trimws(paste(subtitle, if(running==0){NULL}else{paste0(running,"-day running average")}))
    if(type %in% c("yoy","yoy-relative")){
      s <- paste0(s, ", compared with last year")
    }
    return(s)
  }

  build_ylabel <- function(type, unit){
    if(type %in% c("yoy","yoy-relative")){
      return("Change year-on-year")
    }else{
      return(unit)
    }
  }

  build_caption <- function(caption, source){

    if(is.null(caption)){
      if(is.null(source)){
        caption_ <- "Source: CREA. {updated}"
      }else{
        caption_ <- paste0("Source: CREA based on ", sources[[source]], ". {updated}")
      }
    }else{
      caption_ <- caption
    }

    sub("\\{updated\\}", paste("Updated on",format(Sys.Date(), format="%d %B %Y")), caption_)
  }



  width <- list("s"=8,"m"=12,"l"=16)
  height <- list("s"=6,"m"=9,"l"=12)
  expand <- list("s"=0.15, "m"=0.1, "l"=0.05)
  sources <- list("eea"="European Environment Agency",
                  "openaq"="OpenAQ",
                  "earthengine"="Sentinel-5P TROPOMI OFFL NO2",
                  "cpcb"="Central Pollution Control Board",
                  "mee"="Ministry of Ecology and Environment",
                  "csb"="Ministry of Environment and Urban Planning",
                  "jp"="Japan Atmospheric Environmental Regional Observation System",
                  "airkorea"="Air Korea",
                  "defra"="DEFRA")


  if(is.null(meas_raw)){
    meas_raw <- rcrea::measurements(country=country,
                                city=city,
                                poll=poll,
                                aggregate_level=aggregate_level,
                                process_id=process_id,
                                date_from=date_from,
                                source=source,
                                source_city=source_city,
                                with_metadata = T)
  }

  if(!is.null(unit)){
    meas_raw <- meas_raw %>% dplyr::filter(unit %in% !!unit)
  }else{
    if(length(unique(meas_raw$unit))==1){
      unit <- unique(meas_raw$unit)
    }
  }

  if(is.null(poll)){
    poll <- unique(meas_raw$poll)
  }

  if(is.null(years)){
    years <- unique(lubridate::year(meas_raw$date))
  }

  meas <- meas_raw
  meas[meas$unit=='mg/m3',]$value <- meas[meas$unit=='mg/m3',]$value*1000
  meas[meas$unit=='mg/m3',]$unit <- "µg/m3"

  if(add_lockdown){
    meas <- utils.add_lockdown(meas)
  }

  # Ensure common language with earlier versions
  if(!is.null(subfile_by)){
    subfile_by <- dplyr::recode(subfile_by,
                         "region_id"="location_id",
                         "region_name"="location_name",
                         "region"="location_name",
                         "pollutant"="poll",
                         .missing=NULL
    )
  }

  subfiles <- switch(subfile_by,
                     "location_id"=unique(meas$location_id),
                     "location_name"=unique(meas$location_name),
                     "country"=unique(meas$country),
                     "city"=unique(meas$location_name),
                     "gadm1"=unique(meas$location_id),
                     "poll"=unique(meas$poll))

  for(subfile in subfiles){
    for(running in running_days){
      tryCatch({

        location_name <- switch(subfile_by,
                              "country"= countrycode::countrycode(subfile, origin="iso2c", destination = "country.name"),
                              "city"=subfile,
                              "gadm1"=subfile
        )




        filtered_meas <- switch(subfile_by,
                                "country"= meas%>% dplyr::filter(country==subfile),
                                "city"= meas%>% dplyr::filter(location_name==subfile),
                                "gadm1"= meas%>% dplyr::filter(location_id==subfile),
                                "poll"= meas%>% dplyr::filter(poll==subfile)
        ) %>%
          dplyr::mutate(
            location_id=tools::toTitleCase(location_id),
            location_name=tools::toTitleCase(location_name),
            year=lubridate::year(date)) #To match plot_measurements names

        country <- unique(filtered_meas$country)

        # Getting standard plot
        plt <- rcrea::plot_measurements(filtered_meas,
                                 poll=poll,
                                 running_width=running,
                                 color_by = color_by,
                                 subplot_by = subplot_by,
                                 running_maxNAs = running/3,
                                 years=years,
                                 type=type,
                                 percent=percent
                                 )

        if(subplot_by=="poll"){
          filtered_meas$poll <- toupper(filtered_meas$poll)
        }

        if(add_lockdown){
          plt <- plt +
            geom_vline(data=filtered_meas, aes(xintercept=movement, linetype="National lockdown"),
                       color=rcrea::CREAtheme.pal_crea['Turquoise']) +
            geom_vline(data=filtered_meas, aes(xintercept=movement0, linetype="National lockdown"),
                       color=rcrea::CREAtheme.pal_crea['Turquoise']) +
            geom_vline(data=filtered_meas, aes(xintercept=partial_restriction, linetype="Partial restrictions"),
                       color=rcrea::CREAtheme.pal_crea['Turquoise']) +
            geom_vline(data=filtered_meas, aes(xintercept=partial_restriction0, linetype="Partial restrictions"),
                       color=rcrea::CREAtheme.pal_crea['Turquoise']) +
            scale_linetype_manual(values=c("dashed","dotted"), name=NULL)
        }

        # Prettying it
        if(!is.null(color_by) && (color_by != "value")){
          plt <- plt + directlabels::geom_dl(data=plt$data,
                                             aes_string(label=color_by),
                                             method=list(directlabels::dl.trans(y = y + .1),
                                                             "top.bumptwice")) +
            guides(color =F)
        }

        plt <- plt + theme_classic() +
            theme_crea() +
            scale_size_manual(values=c(1), guide=F)

        if(!is.null(color_by) && color_by=="year"){
          plt <- plt + scale_color_brewer(limits=factor(seq(2020,min(2017,min(years)))), palette="Spectral")
        }

        plt <- plt +
            theme(legend.position="right") +
            labs(
              title=build_title(title,subfile_by,subfile,running),
              subtitle=build_subtitle(subtitle,subfile_by,subfile,running,type),
              caption=build_caption(caption, source),
              y=build_ylabel(type, unit))

        if(min(meas$value, na.rm=T)<0){
          plt <- plt + geom_hline(yintercept=0)
        }

        if(!is.null(add_to_ggplot)){
          plt <- plt + add_to_ggplot
        }

        if(!is.null(folder)){

          dir.create(folder, showWarnings = F, recursive = T)

          for(s in size){

            if(min(meas$value, na.rm=T)>=0 & !stringr::str_starts(type, "yoy")){
              plt <- plt + scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, expand[[s]])))
            }

            if("full" %in% range){
              # Full version
              filename_full <- build_filename(source=source,
                                              subfile=subfile,
                                              full_cut="full",
                                              running=running,
                                              size=s,
                                              aggregate_level=aggregate_level,
                                              add_lockdown=add_lockdown,
                                              type=type,
                                              suffix=file_suffix
              )

              ggsave(file.path(folder, filename_full),
                     width=width[[s]], height=height[[s]],
                     plot=plt)
            }
            if("cut" %in% range){
              # Version cut at current month end
              cutdate <- lubridate::date(paste(lubridate::year(max(plt$data$date)),lubridate::month(lubridate::today()+lubridate::duration(1,"months")), 1, sep="-"))
              filename_cut <- build_filename(source=source,
                                             subfile=subfile,
                                             full_cut="cut",
                                             running=running,
                                             size=s,
                                             aggregate_level=aggregate_level,
                                             add_lockdown=add_lockdown,
                                             type=type
              )
              ggsave(file.path(folder, filename_cut),
                     width=width[[s]], height=height[[s]],
                     plot=plt + scale_x_datetime(date_labels = "%b", limits=c(min(min(plt$data$date)), as.POSIXct(cutdate)))
              )
            }
          }
        }else{
          return(plt)
        }

      }, error=function(err){
        warning(paste("Failed for file",subfile,"-",err))
      })
    }
  }
}


plot_meas_observation <- function(meas_raw, running_width){



}

plot_meas_trend <- function(meas_raw, running_width){



}

plot_meas_anomaly <- function(meas_raw, running_width){



}




