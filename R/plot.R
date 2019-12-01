require("ggplot2")

library(ggplot2)
library(zoo)


plot_measurements <-function(meas, poll=NULL, running_days=NULL, color_by='city', subplot_by=NULL){


  # Testing the charts make sense (i.e. not averaging different pollutants)
  if(is.null(poll) && (!'poll' %in% color_by) && (!'poll' %in% subplot_by)){
    stop("You need to specify pollutant to display")
  }

  if(!is.null(poll)){
    meas = meas[meas$parameter == poll, ]
  }

  subtitle <- ''

  group_by_cols <- switch(color_by,
                          'city' = c('city'),
                          'location' = c('city', 'location'))

  # Apply running average
  if(is.null(running_days)){
    meas <- arrange(meas,date) %>% mutate(value_plot=value)
    # subtitle <- paste('Daily average of ', poll_str(poll),  sep="", collapse="")
  }else{

    meas <- arrange(meas,date) %>% group_by_at(group_by_cols) %>%
      mutate(value_plot=rollapply(value, width=running_days, FUN=mean, align='right',fill=NA))

    # subtitle <- paste('Running ',running_days,'-days average of ', poll_str(poll), sep="", collapse="")
  }


  plt <- ggplot(meas, aes_string(x = 'date', y = 'value_plot', color = color_by)) +
    labs(x='', y=expression('concentration [' * mu * 'g/m'^3*']'),
         title=paste(''),
         subtitle = subtitle,
         caption = '') +
    ylim(0, NA) +
    geom_line(aes_string(color = color_by), size = 1) +
    default_theme()
    # theme(legend.position = 'bottom') +
    # scale_linetype_discrete(name='', guide = guide_legend(ncol=2))
    # scale_color_manual(name='', values=c('black', 'darkred'))

  if(!is.null(subplot_by)){
    plt <- plt + facet_wrap(subplot_by, scales = "free")

    if(is.null(color_by) || (color_by==subplot_by)){
      plt <- plt + theme(legend.position = "none")
    }
  }

  return(plt)

}
