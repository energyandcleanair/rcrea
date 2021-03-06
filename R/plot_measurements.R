
plot_measurements <-function(meas,
                             poll=NULL,
                             running_width=NULL,
                             running_days=NULL,
                             running_maxNAs=NULL,
                             color_by='location_id', #location_id, location_name, poll
                             average_by='day',
                             subplot_by=NULL,
                             linetype_by=NULL,
                             line_width=0.8,
                             years=NULL,
                             type='ts',
                             percent=F,
                             date_from=NULL){

  chg_colors <- c("#35416C", "#8CC9D0", "darkgray", "#CC0000", "#990000")


  # Ensure common language with earlier versions
  if(!is.null(color_by)){
    color_by <- dplyr::recode(color_by,
                       "region_id"="location_id",
                       "region_name"="location_name",
                       "region"="location_name",
                       "pollutant"="poll",
                       .missing=NULL
    )
  }

  if(!is.null(subplot_by)){
    subplot_by <- dplyr::recode(subplot_by,
                       "region_id"="location_id",
                       "region_name"="location_name",
                       "region"="location_name",
                       "pollutant"="poll",
                       .missing=NULL
    )
  }


  # Testing the charts make sense (i.e. not averaging different pollutants)
  if(is.null(poll) && (!'poll' %in% color_by) && (!'poll' %in% subplot_by)){
    stop("You need to specify pollutant to display")
  }

  if(('location_name' %in% c(subplot_by, color_by) && is.na(unique(meas$location_name)))){
    warning("location_name information missing. Run measurements query with with_meta=T")
  }

  if(!is.null(subplot_by) && !(subplot_by %in% c("location_id","location_name","poll"))){
    stop("subplot_by can only be 'NULL', 'location_id', 'location_name' or 'poll'")
  }

  if(!is.null(color_by) && !(color_by %in% c("location_id", "location_name", "year", "value", "poll"))){
    stop("color_by can only be NULL, 'location_id', 'location_name', 'year', 'value' or 'poll'")
  }


  # Select pollutants
  if(!is.null(poll)){
    meas <- meas %>% dplyr::filter(poll %in% !!poll)
  }

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
  group_by_cols <- union(c('location_id', 'poll', 'unit', 'process_id'),
                         union(setdiff(color_by,c("year","value")),
                               setdiff(subplot_by,c("year","value"))))

  if(!is.null(average_by)){
    meas <- dplyr::mutate(meas, date=lubridate::floor_date(date, average_by))
  }

  meas <- meas %>% dplyr::group_by_at(union(group_by_cols, 'date')) %>% dplyr::summarise(value = mean(value))

  # Make date axis homogeneous i.e. a row for every day / month / year
  df_placeholder <- meas %>%
    dplyr::group_by_at(group_by_cols) %>%
    dplyr::select(c(group_by_cols, "date")) %>%
    dplyr::summarize(date=list(seq(min(date), max(date), by=paste(average_by)) %>%
                                 trunc(units=average_by))) %>%
    tidyr::unnest(cols=c(date))

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

    if(!is.null(years)){
      meas <- meas %>% dplyr::filter(lubridate::year(date) %in% years)
    }
  }

  if(!is.null(date_from)){
    meas <- meas %>% dplyr::filter(date >= date_from)
  }

  if(nrow(meas %>% dplyr::filter(!is.na(value)))==0){
    stop("No measurement to plot after applying running average. Try reducing running average width.")
  }

  if(type=='yoy'){
    meas <- utils.yoy(meas, "absolute")
  }
  if(type=='yoy-relative'){
    meas <- utils.yoy(meas, "relative")
  }

  # Remove year for time series to overlap
  meas <- meas %>% dplyr::mutate(year=factor(lubridate::year(date)))
  meas$group <- 1

  if('year' %in% color_by){
    meas <- meas %>% dplyr::mutate(year=factor(lubridate::year(date)))
    lubridate::year(meas$date) <- 0
    meas$group <- meas$year
  }

  # Build plot
  if(!is.null(color_by) && !is.na(color_by)){
    plt_aes <- aes_string(x='date', y='value', color=color_by)
    n_colors <- nrow(meas %>% dplyr::ungroup() %>% dplyr::distinct_at(color_by))
    show_color_legend <- (!color_by %in% c(subplot_by,"value"))
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

  # Add categorical variable
  if(stringr::str_starts(type,"heatmap")){
    meas <- meas %>% dplyr::group_by(poll, unit) %>% dplyr::mutate(value_cat=cut_poll(unique(poll), value))
  }

  # Capitalize pollutants and regions for display
  meas$poll <- toupper(meas$poll)
  meas <- meas %>%
    dplyr::mutate_at(intersect(c("location_id","location_name"), names(meas)),
                     tools::toTitleCase)

  # Ensure this is always datetime
  meas$date <- as.POSIXct(meas$date)

  date_format <- ifelse(average_by=="day", "%y-%m-%d", "%y-%m-%d %H:%M")
  if(!is.null(color_by) && color_by=="year"){
    date_format <- gsub("%y-", "", date_format)
  }
  meas$label <- sprintf("%s-%s\n%s: %s %s", meas$year, strftime(meas$date, date_format), meas$poll, round(meas$value), meas$unit)

  plt <- ggplot2::ggplot(meas %>% dplyr::filter(!is.na(value)), plt_aes, color="red") +
    labs(x='', y=ylabel,
         title=paste(''),
         subtitle = '',
         caption = '') +
    theme_crea()
  ymin <- min(min(meas$value, na.rm=T),0)
  maxabs <- max(abs(meas$value), na.rm=T)


  plt <- switch(type,
                "ts" = plt + geom_line(size=line_width, lineend="round", show.legend = show_color_legend,
                                       aes(text=label, group=group)) +
                  ylim(ymin, NA) +
                  {if(!is.null(color_by) && (color_by=="value")) scale_color_gradientn(colors = chg_colors, guide = F, limits=c(-maxabs,maxabs))}+
                  {if(is.null(color_by) || color_by!="value") scale_color_manual(values=RColorBrewer::brewer.pal(max(n_colors, 4), "Spectral")[n_colors:1])},
                "yoy" = plt + geom_line(size=line_width) +
                  {if(!is.null(color_by) && (color_by=="value"))scale_color_gradientn(colors = chg_colors, guide = F, limits=c(-maxabs,maxabs))}+
                  {if(is.null(color_by) || color_by!="value") scale_color_manual(values=RColorBrewer::brewer.pal(max(n_colors, 4), "Spectral")[n_colors:1])},
                "yoy-relative" = plt + geom_line(size=line_width) +
                  scale_y_continuous(labels=scales::percent) +
                  geom_hline(yintercept = 0) +
                  {if(!is.null(color_by) && (color_by=="value"))scale_color_gradientn(colors = chg_colors, guide = F, limits=c(-maxabs,maxabs))}+
                  {if(is.null(color_by) || color_by!="value") scale_color_manual(values=RColorBrewer::brewer.pal(max(n_colors, 4), "Spectral")[n_colors:1])},
                "heatmap" = plt +
                  geom_raster(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'location_id'), fill='value_cat'), color='white') +
                  scale_y_discrete() +
                  scale_fill_poll(NULL, poll) +
                  labs(y=NULL) +
                  theme(legend.position = "right"),
                "heatmap_w_text" = plt +
                  geom_tile(aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'location_id'), fill='value_cat'), color='white') +
                  scale_y_discrete(expand=c(0,0)) +
                  scale_fill_poll(NULL, poll) +
                  geom_text( aes_string(x='date', y=ifelse(!is.null(subplot_by), subplot_by, 'location_id'),
                                        label="paste(sprintf('%.0f', value_plot))"), size=3, color='black') + theme(axis.text.x = element_text()) +
                  labs(x=NULL, y=NULL, subtitle=expression('[' * mu * 'g/m'^3*']'), title=paste(poll_str(poll), 'concentration'))
  )


  if('year' %in% color_by){
    plt <- plt + scale_x_datetime(date_labels = "%d %b")
  }

  if(percent){
    plt <- plt + scale_y_continuous(labels=scales::percent)
  }

  if(!is.null(subplot_by) && (type %in% c('ts','yoy', 'yoy-relative'))){
    facets <- if(length(units)==1){subplot_by}else{c(subplot_by,'unit')}
    scales = ifelse(all(subplot_by %in% c("location_id","location_name")),'fixed','free')
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
