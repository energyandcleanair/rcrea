library(ggplot2)
library(zoo)
library(ggnewscale)
source('R/99_crea_theme.R')

# Utils -------------

cut_poll <- function(poll, value){
  # Transforms continuous to category value
  scale <- if(poll==creadb::PM25){
    cut(value,c(0, 30, 60, 90, 120, 250, Inf), labels=c("Good", "Satisfactory","Moderate","Poor","Very Poor","Severe"))
  }else if(poll==creadb::PM10){
    cut(value,c(0, 50, 100, 250, 350, 430, Inf), labels=c("Good", "Satisfactory","Moderate","Poor","Very Poor","Severe"))
  }else if(poll==creadb::CO){
    cut(value,c(0, 1000, 2000, 10000, 17000, 34000, Inf), labels=c("Good", "Satisfactory","Moderate","Poor","Very Poor","Severe"))
  }else if(poll==creadb::NO2){
    cut(value,c(0, 40, 80, 180, 280, 400, Inf), labels=c("Good", "Satisfactory","Moderate","Poor","Very Poor","Severe"))
  }else if(poll==creadb::SO2){
    cut(value,c(0, 40, 80, 380, 800, 1600, Inf), labels=c("Good", "Satisfactory","Moderate","Poor","Very Poor","Severe"))
  }else if(poll==creadb::O3){
    cut(value,c(0, 50, 100, 168, 208, 748, Inf), labels=c("Good", "Satisfactory","Moderate","Poor","Very Poor","Severe"))
  }else{
    cut(value,c(-2,-1), labels=c("Unknown"))
  }
  return(scale)
}

scale_fill_poll <- function(organization, poll){

  return(scale_fill_manual(name=NULL, values = c("Good"="green4","Satisfactory"="green3","Moderate"="yellow","Poor"="orange","Very Poor"="red", "Severe"="red4"),
                           drop = FALSE,
                           na.translate = FALSE))
}





# Plot functions ---------------
partial_plot_target <- function(poll, target, country, city, location_id, date_from, date_to, average_by, type, color_by){

  result <- NULL
  if(type=='ts'){

    # Get target values per year
    value_baseline <- creadb::measurements(country=country, city=city, poll=poll, average_by='year', collect=F,
                                       date_from = lubridate::ymd(target$year_baseline*10000 + 101),
                                       date_to = lubridate::ymd(target$year_baseline*10000 + 1231)) %>%
                      summarize(value=mean(value)) %>% collect()
    value_baseline <- value_baseline[[1]]

    years <- seq(lubridate::year(date_from), lubridate::year(date_to))
    values <- c()
    for(year in years){
      values <- c(values, value_baseline[1] * (1 + target$target_magnitude * (year-target$year_start)/(target$year_end-target$year_start)))
    }

    # Prepare plot
    plot_data <- tibble(year=years, value=values, target=target$short_name)
    plot_data <- plot_data %>% mutate(date = lubridate::as_datetime(lubridate::ymd(year*10000 + 101)))

    # Flat segments if not averaged by years
    if(average_by!='year' | 'year' %in% color_by){
      plot_data_yearend <- tibble(year=years, value=values, target=target$short_name)
      plot_data_yearend <- plot_data_yearend %>% mutate(date = pmin(date_to, lubridate::as_datetime(lubridate::ymd(year*10000 + 1231))))
      plot_data <- bind_rows(plot_data, plot_data_yearend)
    }

    if('year' %in% color_by){
      plot_data <- plot_data %>% dplyr::mutate(year=factor(lubridate::year(date)))
      lubridate::year(plot_data$date) <- 0
      result <- geom_line(data=plot_data, aes(x=date, y=value, color=year, linetype=target), size=0.8)
    }else{
      result <- geom_line(data=plot_data, aes(x=date, y=value, color=target), linetype='dashed', size=0.8)
    }

  }
  return(result)
}

add_plot_scale <- function(plot, scale, date_from, date_to){

  lower <- scale$thresholds[[1]]
  upper <- c(tail(lower, -1), Inf)
  colour <- scale$colours[[1]]
  label <- scale$labels[[1]]

  scale_df = tibble(lower, upper, colour, label)

  # Preparing colours
  colour_scale_values <- as.list(colour)
  names(colour_scale_values)<- label

  return(plot +  ggnewscale::new_scale_color() + ggnewscale::new_scale_fill() +
           geom_rect(data=scale_df, aes(ymin=lower, x=NULL, y=NULL, ymax=upper, xmin=date_from, xmax=date_to, colour_new=NULL, fill=label),  size=0, colour='white', alpha=0.2) +
           scale_fill_manual(values=colour_scale_values, breaks=rev(label))
         )
}


plot_measurements_count <- function(meas, poll=NULL, running_days=NULL, color_by='city', average_by='day', subplot_by=NULL, type='heatmap'){

  if(!is.null(running_days) && (average_by != 'day')){
    stop(paste("You cannot have rolling mean when averaging by", average_by))
  }

  # Select pollutants
  if(!is.null(poll)){
    meas = meas[meas$poll == poll, ]
  }

  # Take mean over relevant grouping (at least city, date and pollutant)
  group_by_cols <- union(c('city'), union(color_by, subplot_by))
  meas <- dplyr::mutate(meas, date=lubridate::floor_date(date, average_by))
  meas <- meas %>% group_by_at(union(group_by_cols, 'date'))  %>% dplyr::summarise(value = n())

  # Make date axis homogeneous i.e. a row for every day / month / year
  # https://stackoverflow.com/questions/14821064/line-break-when-no-data-in-ggplot2
  dates <- seq(min(lubridate::date(meas$date)), max(lubridate::date(meas$date)), by=paste(average_by))
  meas$date <- lubridate::date(meas$date)
  group_by_uniques <- unique(meas[,group_by_cols])
  df_placeholder <- merge(group_by_uniques, data.frame(date=dates), by=NULL)
  meas <- merge(meas, df_placeholder, all=TRUE)

  meas$value[is.na(meas$value)] <- 0

  # Apply running average if need be
  if(is.null(running_days)){
    meas <- dplyr::arrange(meas, date)  %>% dplyr::mutate(value_plot=value)
  }else{
    meas <- meas %>% dplyr::arrange(date) %>% group_by_at(group_by_cols)  %>%
      dplyr::mutate(value_plot=zoo::rollapply(value, width=running_days, FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA))
  }

  # Build plot
  plt <- ggplot2::ggplot(meas, aes_string(x = 'date', y = 'value_plot', color = color_by)) +
    labs(x='', y=paste('Number of measurements [/',average_by,']',sep=''),
         title=paste(''),
         subtitle = '',
         caption = '') +
    theme_minimal() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0))

  plt <- switch(type,
                "ts" = plt + geom_line(aes_string(color = color_by), size = 0.8) +
                  ylim(0, NA),
                "heatmap" = plt +
                  geom_raster(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'city'), fill='ifelse(value_plot==0, NA, value_plot)')) +
                  scale_x_date(date_breaks = "3 month", expand=c(0,0)) +
                  scale_fill_distiller(palette = "Spectral", na.value = 'black')
  )


  if(!is.null(subplot_by) && (type=='ts')){
    plt <- plt + facet_wrap(subplot_by, scales = "free")

    if(is.null(color_by) || (color_by==subplot_by)){
      plt <- plt + theme(legend.position = "none")
    }
  }

  return(plt)
}

plot_measurements <-function(meas, poll=NULL, running_width=NULL, running_days=NULL, color_by='city', average_by='day', subplot_by=NULL, type='ts'){

  # Testing the charts make sense (i.e. not averaging different pollutants)
  if(is.null(poll) && (!'poll' %in% color_by) && (!'poll' %in% subplot_by)){
    stop("You need to specify pollutant to display")
  }

  if(('location' %in% c(subplot_by, color_by) && is.na(unique(meas$location)))){
    warning("location information missing. Run measurements query with keep_location_id=T")
  }

  if(('location_id' %in% c(subplot_by, color_by) && is.na(unique(meas$location_id)))){
    warning("location information missing. Run measurements query with keep_location_id=T")
  }

  # Select pollutants
  if(!is.null(poll)){
    meas = meas[meas$poll == poll, ]
  }

  # Deprecated argument(s)
  if(exists('running_days') && !is.null(running_days)){
    warning("running_days argument is deprecated. Use running_width instead.")
    running_width = ifelse(is.null(running_width), running_days, running_width)
  }

  # Check not empty
  if(nrow(meas)==0){
    stop("No measurement")
  }

  # Take mean over relevant grouping (at least city, date and pollutant)
  group_by_cols <- union(c('city', 'poll', 'unit'), union(setdiff(color_by,c("year")), setdiff(subplot_by,c("year"))))
  meas <- dplyr::mutate(meas, date=lubridate::floor_date(date, average_by))
  meas <- meas %>% dplyr::group_by_at(union(group_by_cols, 'date'))  %>% dplyr::summarise(value = mean(value))

  # Make date axis homogeneous i.e. a row for every day / month / year
  # https://stackoverflow.com/questions/14821064/line-break-when-no-data-in-ggplot2
  dates <- seq(min(meas$date), max(meas$date), by=paste(average_by))
  group_by_uniques <- unique(meas[,group_by_cols])
  df_placeholder <- merge(group_by_uniques, data.frame(date=dates), by=NULL)
  # df_placeholder <- transform(df_placeholder, date_str=format(date, "%Y-%m-%d"))

  # meas <- transform(meas, date_str=format(date, "%Y-%m-%d"))
  # meas <- subset(meas, select = -c(date))
  meas <- merge(meas, df_placeholder, all=TRUE)

  # Apply running average if need be
  if(is.null(running_width) || (running_width==1)){
    meas <- dplyr::arrange(meas, date)  %>% dplyr::mutate(value_plot=value)
  }else{
    meas <- meas %>% dplyr::arrange(date) %>% dplyr::group_by_at(group_by_cols)  %>%
      dplyr::mutate(value_plot=zoo::rollapply(value, width=running_width, FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA))
  }

  # Remove year for time series to overlap
  if('year' %in% color_by){
    meas <- meas %>% dplyr::mutate(year=factor(lubridate::year(date)))
    lubridate::year(meas$date) <- 0
  }

  # Add categorical variable
  meas <- meas %>% group_by(poll, unit) %>% mutate(value_plot_cat=cut_poll(poll, value))

  # Build plot
  if(!is.null(color_by)){
    plt_aes <- aes_string(x='date', y='value_plot', color=color_by)
  }else{
    plt_aes <- aes_string(x='date', y='value_plot', color=shQuote("red"))
  }

  plt <- ggplot2::ggplot(meas, plt_aes) +
        labs(x='', y='concentration',
         title=paste(''),
         subtitle = '',
         caption = '') +
        .CREAtheme$theme_crea()

  plt <- switch(type,
         "ts" = plt + geom_line(size = 0.8) +
                ylim(0, NA),
         "heatmap" = plt +
                    geom_raster(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'city'), fill='value_plot_cat'), color='white') +
                    scale_y_discrete() +
                    scale_fill_poll(NULL, poll) +
                    theme(legend.position = "right"),
         "heatmap_w_text" = plt +
                    geom_tile(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'city'), fill='value_plot_cat'), color='white') +
                    scale_y_discrete(expand=c(0,0)) +
                    scale_fill_poll(NULL, poll) +
                    geom_text( aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'city'),
                                          label="paste(sprintf('%.0f', value_plot))"), size=3, color='black') + theme(axis.text.x = element_text()) +
                      labs(x='', y='', subtitle=expression('[' * mu * 'g/m'^3*']'), title=paste(poll_str(poll), 'concentration'))
         )


  if('year' %in% color_by){
    plt <- plt + scale_x_datetime(date_labels = "%b")
  }


  if(!is.null(subplot_by) && (type=='ts')){
    plt <- plt + facet_wrap(c(subplot_by,'unit'), scales = ifelse(subplot_by=='city','fixed','free_y'))

    if(is.null(color_by) || (color_by==subplot_by)){
      plt <- plt + theme(legend.position = "none")
    }
  }else{
    plt <- plt + facet_wrap(~unit, scales = 'free_y')
  }
  return(plt)
}


plot_exceedances <-function(excs, poll=NULL, average_by='day', subplot_by='city', separate_polls=TRUE){


  # Select pollutants
  if(!is.null(poll)){
    excs = excs[excs$poll == poll, ]
  }

  # Add status: 0: no violation, 100: violation reached threshold
  excs <- dplyr::mutate(excs, status= ifelse(exceedance_allowed_per_year==0,
                                      pmin(exceedance_this_year*100, 100),
                                      pmin(exceedance_this_year/exceedance_allowed_per_year*100, 100)
                                      ))


  # Take mean over relevant grouping (at least city, date and pollutant)
  group_by_cols <- union(c('city', 'poll'), subplot_by)
  excs <- dplyr::mutate(excs, date=lubridate::floor_date(date, average_by))
  excs <- excs %>% group_by_at(union(group_by_cols, 'date'))  %>% dplyr::summarise(value = n(), status=max(status))

  # Make date axis homogeneous i.e. a row for every day / month / year
  # https://stackoverflow.com/questions/14821064/line-break-when-no-data-in-ggplot2
  dates <- seq(min(excs$date), max(excs$date), by=paste(average_by))
  group_by_uniques <- unique(excs[,group_by_cols])
  df_placeholder <- merge(group_by_uniques, data.frame(date=dates), by=NULL)
  df_placeholder <- transform(df_placeholder, date_str=format(date, "%Y-%m-%d"))

  excs <- transform(excs, date_str=format(date, "%Y-%m-%d"))
  excs <- subset(excs, select = -c(date))
  excs <- merge(excs, df_placeholder, all=TRUE)

  # Build plot
  plt <- ggplot2::ggplot(excs, aes_string(x = 'date')) +
    labs(x='', y='',
         title=paste(''),
         subtitle = '',
         caption = '') +

    theme_minimal() +
    geom_tile(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'city'), fill='status'), color='white') +
    scale_y_discrete(limits = rev(unique(sort(excs$city)))) +
    scale_fill_distiller(palette = "Spectral", na.value = 'white', limits = c(0,100))


  if(separate_polls){
      plt <- plt + facet_wrap('poll', scales = "free")
  }

  return(plt)
}

map_exceedance_status <- function(exc_status){
  plt <- ggplot(data = sf::st_as_sf(exc_status)) + geom_sf(aes(color=status))
  return(plt)
}
