require("ggplot2")

library(ggplot2)
library(zoo)


plot_measurements <-function(meas, poll=NULL, running_days=NULL, color_by='city', average_by='day', subplot_by=NULL, type='ts'){

  # Testing the charts make sense (i.e. not averaging different pollutants)
  if(is.null(poll) && (!'poll' %in% color_by) && (!'poll' %in% subplot_by)){
    stop("You need to specify pollutant to display")
  }

  if(!is.null(running_days) && (average_by != 'day')){
    stop(paste("You cannot have rolling mean when averaging by", average_by))
  }

  # Select pollutants
  if(!is.null(poll)){
    meas = meas[meas$poll == poll, ]
  }

  # Take mean over relevant grouping (at least city, date and pollutant)
  group_by_cols <- union(c('city', 'poll'), union(color_by, subplot_by))
  meas <- mutate(meas, date=lubridate::floor_date(date, average_by))
  meas <- meas %>% group_by_at(union(group_by_cols, 'date'))  %>% summarise(value = mean(value))

  # Apply running average if need be
  if(is.null(running_days)){
    meas <- arrange(meas, date)  %>% mutate(value_plot=value)
  }else{
    meas <- meas %>% arrange(date) %>% group_by_at(group_by_cols)  %>%
            mutate(value_plot=rollapply(value, width=running_days, FUN=mean, align='right',fill=NA))
  }

  # Build plot
  plt <- ggplot(meas, aes_string(x = 'date', y = 'value_plot', color = color_by)) +
    labs(x='', y=expression('concentration [' * mu * 'g/m'^3*']'),
         title=paste(''),
         subtitle = '',
         caption = '') +
        theme_minimal()

  plt <- switch(type,
         "ts" = plt + geom_line(aes_string(color = color_by), size = 1) +
                ylim(0, NA),
         "heatmap" = plt +
                    geom_tile(aes(x=date, y=ifelse(!is.null(subplot_by), subplot_by , location), fill=value_plot, color=NULL)) +
                    scale_y_discrete()
         )


  if(!is.null(subplot_by) && (type=='ts')){
    plt <- plt + facet_wrap(subplot_by, scales = "free")

    if(is.null(color_by) || (color_by==subplot_by)){
      plt <- plt + theme(legend.position = "none")
    }
  }

  return(plt)
}
