library(creadb)
library(ggplot2)
library(dplyr)
library(gbm)
library(tidyr)
library(lubridate)
library(zoo)
library(memoise)
library(worldmet)

exp_name <- "delhi_lucknow_hyder_new_thresholds_sunshine"
all_results <- NULL
# Top 10 cities with the most measurements
city <- c('Delhi', 'Lucknow', 'Hyderabad') #, 'Bengaluru', 'Hyderabad', 'Lucknow', 'Chennai')# 'Mumbai', 'Jaipur', 'Chandrapur', 'Chandrapur', 'Kolkata')
poll <- c(PM25, PM10, SO2, NO2, CO, O3, NO, NOX)

# On what rolling average do we want to train the model
training_average_by='hour'
training_average_by_widths=c(1)

# On what rolling average do we want to plot results
plotting_average_by='day'
plotting_average_by_width=30

# meas_weather <- aq_weather.m.collect(city=city,
#                                      poll=poll,
#                                      date_from='2015-01-01',
#                                      use_worldmet = T,
#                                      average_by=training_average_by,
#                                      weather_radius_km = 20)


meas_weather <- readRDS('cache/new_thresholds/meas_weather_top5_india_worldmet_sunshine.rds')
# meas_weather <- weather.sirad.join(meas_weather)
# saveRDS(meas_weather, 'cache/new_thresholds/meas_weather_top5_india_worldmet_sunshine.rds')


meas_weather$wd <- coalesce(meas_weather$wd,-1)
meas_weather$wd_factor <- factor(meas_weather$wd %/% 45)
meas_weather$precip_coalesced <- coalesce(meas_weather$precip,0)
meas_weather$precip_ghcnd_coalesced <- coalesce(meas_weather$precip_ghcnd,0)
meas_weather$ceil_hgt_coalesced <- coalesce(meas_weather$ceil_hgt,0)
meas_weather <- meas_weather %>% group_by(city, poll) %>% mutate(atmos_pres=na.approx(atmos_pres, date, na.rm=FALSE)) %>% ungroup()

# Introduce lags
hour_lags <- c(1:12)
weather_vars <- all.vars(formula)[-1]
non_weather_vars <- setdiff(colnames(meas), weather_vars)
meas_weather <- utils.add_lag(meas_weather, cols=weather_vars, hour_lags = hour_lags)

models <- aq_weather.default_models()[c('gbm')]
formula <- reformulate(termlabels=c('air_temp', paste('air_temp',hour_lags,sep="_"),
                         'atmos_pres', paste('atmos_pres',hour_lags,sep="_"),
                         'RH', paste('RH',hour_lags,sep="_"),
                         'ceil_hgt_coalesced', paste('ceil_hgt_coalesced',hour_lags,sep="_"),
                         'visibility', paste('visibility',hour_lags,sep="_"),
                         'precip_coalesced', paste('precip_coalesced',hour_lags,sep="_"),
                         'sunshine', paste('sunshine',hour_lags,sep="_"),
                         # interaction term
                         paste(c('wd_factor', paste('wd_factor',hour_lags,sep="_")),
                               c('ws', paste('ws',hour_lags,sep="_")),sep='*')),
            response='value')

formulas <- c(formula)
for(formula in formulas){
  print(paste("Formula:", paste(formula, collapse='')))
  tryCatch({
    result <- aq_weather.predict(meas_weather = meas_weather,
                                 formula=formula,
                                 training_average_by = training_average_by,
                                 training_average_by_width = 1,
                                 models = models)

    aq_weather.plot(result,
                    plotting_average_by = plotting_average_by,
                    plotting_average_by_width = plotting_average_by_width,
                    subtitle=paste(paste(unique(result$city), collapse=','),
                                   paste("Training:",training_average_by_width, training_average_by),
                                   paste(formula,collapse=''),
                                   paste("Plotting:",plotting_average_by_width, plotting_average_by),sep="\n"),
                    filename = paste0('export_', exp_name,
                                      format(Sys.time(), "%Y-%m-%d_%H%M%S"),'.pdf'))

    # Store result
    result_rich <- result
    result_rich$training_average_by=training_average_by
    result_rich$training_average_by_width=training_average_by_width
    result_rich$formula=paste(formula, collapse=' ')
    if(is.null(all_results)){
      all_results <- result_rich
    }else{
      all_results <- bind_rows(all_results,
                               result_rich)
    }
  }, error=function(cond) print(paste("error:", cond)))
}

saveRDS(all_results, 'cache/new_thresholds/results_delhi_lucknow_hyder_worldmet_sunshine_lag.rds')


#########################
# Interpreting results
#########################
# Number of measurements
ggplot(meas_weather %>% group_by(city, poll) %>% summarize(npoll=n()), aes(x=city, y=npoll)) + geom_col() + facet_wrap(~poll, scales='free_x')

# Best models
all_results %>% group_by(city, poll) %>%

all_results$training_average_by_width <- factor(all_results$training_average_by_width, ordered = T)
ggplot(all_results %>% mutate(label=paste("R2:",sprintf("%.2f",rsq))), aes(x=1,y = training_average_by_width, fill = rsq, label=label)) +
  facet_grid(city + formula ~ poll+ model_name, scales = "free") +  geom_tile() + geom_text() +
  scale_fill_continuous(na.value = 'white') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

rsq_plot_data <- all_results %>% group_by(city, poll) %>% summarize(rsq=max(rsq, na.rm=T)) %>% arrange(-rsq) %>%  ungroup() %>% mutate(label=paste("R2:",sprintf("%.2f",rsq)))
rsq_plot_data$poll <- factor(rsq_plot_data$poll)
ggplot(rsq_plot_data, aes(x=city,y = rsq, fill = rsq, label=label)) + geom_col()+ geom_text(nudge_y=0.03, size=3) + facet_wrap(~poll, scales='free_x')

  facet_grid( city , scales = "free") +  geom_tile() + geom_text() +
  scale_fill_continuous(na.value = 'white') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


  # Plot best Delhi results




  # Trying Delhi without visibility
  formula <- value ~ air_temp + wd_factor*ws + atmos_pres + RH + ceil_hgt_coalesced + precip_coalesced
  result_no_visibility <- aq_weather.predict(meas_weather = meas_weather %>% filter(city=='Delhi', poll %in% c(PM10, PM25)),
                               formula=formula,
                               training_average_by = 'hour',
                               training_average_by_width = 8,
                               models = models)

  # Understanding why other cities so low
  formula <- value ~ air_temp + wd_factor*ws + atmos_pres + RH + ceil_hgt_coalesced + precip_coalesced + visibility
  model_gbm <- aq_weather.default_models()[c('gbm')]

  result_lucknow_delhi <- aq_weather.predict(meas_weather = meas_weather %>% filter(city %in% c('Lucknow','Hyderabad', 'Delhi'), poll==PM25),
                                             formula=formula,
                                             training_average_by = 'hour',
                                             training_average_by_width = 8,
                                             models = model_gbm)

  # Plot residuals

  # Plot Delhi early measurements
  ggplot(meas_weather, aes(x=date, y=value)) + geom_line() + facet_grid(poll~city, scales='free_y')

  aq_weather.plot(all_results %>% filter(city='Delhi'),
                  plotting_average_by = 'hour',
                  plotting_average_by_width = 1,
                  subtitle=paste(paste(unique(result$city), collapse=','),
                                 paste("Training:",training_average_by_width, training_average_by),
                                 paste(formula,collapse=''),
                                 paste("Plotting:",plotting_average_by_width, plotting_average_by),sep="\n"),
                  filename = paste0('export_', 'delhi_raw',
                                    format(Sys.time(), "%Y-%m-%d_%H:%M:%S"),'.pdf'))


  # Hyderabad PM2.5 peaks
  meas_hyderabad_peak <- measurements(city='Hyderabad', poll='pm25',
                                      average_by = NULL,
                                      aggregate_at_city_level=F,
                                      date_from='2015-11-01',
                                      date_to='2015-11-02',
                                      collect=T)
