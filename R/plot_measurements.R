
plot_measurements <-function(meas,
                             poll=NULL,
                             running_width=NULL,
                             running_days=NULL,
                             running_maxNAs=NULL,
                             color_by='region_id',
                             average_by='day',
                             subplot_by=NULL,
                             linetype_by=NULL,
                             type='ts'){

  poll_ <- tolower(poll)

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

  if(!is.null(subplot_by) && !(subplot_by %in% c("region_id","region_name","poll"))){
    stop("subplot_by can only be 'NULL', 'region_name', 'region_id' or 'poll'")
  }

  if(!is.null(color_by) && !(color_by %in% c("region_id","year"))){
    stop("color_by can only be 'NULL', 'region_id' or 'year'")
  }


  # Select pollutants
  meas <- switch(toString(length(poll_)),
                 "0" = meas, # NULL
                 "1" = meas %>% dplyr::filter(poll==poll_),
                 meas %>% dplyr::filter(poll %in% poll_)
  )


  # Capitalize pollutants and regions for display
  meas$poll <- toupper(meas$poll)
  meas$region_id <- tools::toTitleCase(meas$region_id)


  # Deprecated argument(s)
  if(exists('running_days') && !is.null(running_days)){
    warning("running_days argument is deprecated. Use running_width instead.")
    running_width = ifelse(is.null(running_width), running_days, running_width)
  }

  # Check not empty
  if(nrow(meas)==0){
    stop("No measurement to plot")
  }

  # Take mean over relevant grouping (at least city, date and pollutant)
  group_by_cols <- union(c('region_id', 'poll', 'unit', 'process_id'), union(setdiff(color_by,c("year")), setdiff(subplot_by,c("year"))))

  if(!is.null(average_by)){
    meas <- dplyr::mutate(meas, date=lubridate::floor_date(date, average_by))
  }
  meas <- meas %>% dplyr::group_by_at(union(group_by_cols, 'date'))  %>% dplyr::summarise(value = mean(value))

  # Make date axis homogeneous i.e. a row for every day / month / year
  df_placeholder <- meas %>%
    dplyr::group_by_at(group_by_cols) %>%
    dplyr::select(c(group_by_cols, "date")) %>%
    dplyr::summarize(date=list(seq(min(date), max(date), by=paste(average_by)) %>%
                                 trunc(units=average_by))) %>%
    tidyr::unnest(cols=c(date))
  # df_placeholder <- transform(df_placeholder, date_str=format(date, "%Y-%m-%d"))

  # meas <- transform(meas, date_str=format(date, "%Y-%m-%d"))
  # meas <- subset(meas, select = -c(date))
  meas <- merge(meas, df_placeholder, all=TRUE)

  # Apply running average if need be
  if(is.null(running_width) || (running_width<=1)){
    meas <- dplyr::arrange(meas, date) %>% dplyr::filter(!is.na(value))
  }else{
    if(!is.null(running_maxNAs)){
      min_values <- running_width-running_maxNAs
    }else{
      min_values <- NULL
    }
    meas <- meas %>% utils.rolling_average(average_by=average_by,
                                           average_width=running_width,
                                           min_values=min_values,
                                           vars_to_avg=c("value"),
                                           group_by_cols = setdiff(colnames(meas), c("value","date"))) %>%
      dplyr::filter(!is.na(value))

    # meas <- meas %>% dplyr::arrange(date) %>% dplyr::group_by_at(group_by_cols)  %>%
    #   dplyr::mutate(value_plot=zoo::rollapply(value, width=running_width, FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA))
  }

  if(nrow(meas %>% dplyr::filter(!is.na(value)))==0){
    stop("No measurement to plot after applying running average. Try reducing running average width.")
  }

  if(type=='yoy'){
    meas <- utils.yoy(meas, "absolute")
  }

  # Remove year for time series to overlap
  if('year' %in% color_by){
    meas <- meas %>% dplyr::mutate(year=factor(lubridate::year(date)))
    # meas <- meas %>% dplyr::mutate(year=reorder(year, dplyr::desc(year)))
    lubridate::year(meas$date) <- 0
  }

  # Add categorical variable
  meas <- meas %>% dplyr::group_by(poll, unit) %>% dplyr::mutate(value_cat=cut_poll(poll, value))

  # Build plot
  if(!is.null(color_by) && !is.na(color_by)){
    plt_aes <- aes_string(x='date', y='value', color=color_by)
    n_colors <- nrow(meas %>% dplyr::ungroup() %>% dplyr::distinct_at(color_by))
    show_color_legend <- (color_by != subplot_by)
  }else{
    plt_aes <- aes_string(x='date', y='value', color=shQuote("red"))
    n_colors <- 1
    show_color_legend <- F
  }

  if(!is.null(linetype_by) && !is.na(linetype_by)){
    plt_aes$linetype <- aes_string(linetype=linetype_by)$linetype
  }

  units <- unique(meas$unit)
  ylabel <- ifelse(length(units)==1, units, "Concentration")

  plt <- ggplot2::ggplot(meas, plt_aes, color="red") +
    labs(x='', y=ylabel,
         title=paste(''),
         subtitle = '',
         caption = '') +
    theme_crea()
  ymin <- min(min(meas$value, na.rm=T),0)
  plt <- switch(type,
                "ts" = plt + geom_line(aes(size="1"), show.legend = show_color_legend) +
                  ylim(ymin, NA) +
                  scale_size_manual(values=c(0.8), guide = FALSE) +
                  scale_color_manual(values=RColorBrewer::brewer.pal(max(n_colors, 4), "Spectral")[n_colors:1]),
                "yoy" = plt + geom_line(aes(size="1")) +
                  ylim(ymin, NA) +
                  scale_size_manual(values=c(0.8), guide = FALSE) +
                  scale_color_manual(values=RColorBrewer::brewer.pal(max(n_colors, 4), "Spectral")[n_colors:1]),
                  # scale_color_brewer(palette="Spectral"),
                # CREAtheme.scale_color_crea_d("dramatic"),
                "heatmap" = plt +
                  geom_raster(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'region_id'), fill='value_plot_cat'), color='white') +
                  scale_y_discrete() +
                  scale_fill_poll(NULL, poll) +
                  theme(legend.position = "right"),
                "heatmap_w_text" = plt +
                  geom_tile(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'region_id'), fill='value_plot_cat'), color='white') +
                  scale_y_discrete(expand=c(0,0)) +
                  scale_fill_poll(NULL, poll) +
                  geom_text( aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'region_id'),
                                        label="paste(sprintf('%.0f', value_plot))"), size=3, color='black') + theme(axis.text.x = element_text()) +
                  labs(x='', y='', subtitle=expression('[' * mu * 'g/m'^3*']'), title=paste(poll_str(poll), 'concentration'))
  )


  if('year' %in% color_by){
    plt <- plt + scale_x_datetime(date_labels = "%b")
  }

  if(!is.null(subplot_by) && (type %in% c('ts','yoy'))){
    facets <- if(length(units)==1){subplot_by}else{c(subplot_by,'unit')}
    scales = ifelse(all(subplot_by %in% c("region_id","region_name")),'fixed','free')
    plt <- switch(as.character(length(facets)),
                  "2"= plt + facet_grid(formula(paste0(facets[1]," ~ ",facets[2])), scales=scales),
                  "3"= plt + facet_grid(formula(paste0(facets[1]," ~ ",paste(facets[2:3],collapse="+"))), scales=scales),
                  plt + facet_wrap(facets, scales=scales)
    )
    if(!show_color_legend){
      plt <- plt + guides(color = show_color_legend)
    }
  }else{
    plt <- plt + facet_wrap(~unit, scales = 'free')
  }
  return(plt)
}
