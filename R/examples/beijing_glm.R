library(creadb)
library(ggplot2)
library(dplyr)



average_by='hour'
meas_bj <- measurements(city=c('Beijing'),
                                date_from='2015-01-01',
                                collect=F, #to save time (only do collection at last step)
                                average_by=average_by,
                                with_metadata=T)

# Check which location is the most relevant
meas_bj %>% group_by(location_id) %>% tally()

# Attach weather measurements
meas_bj_noaa <- join_noaa_observations(meas_bj,
                                               measurements_averaged_by=average_by,
                                               collect=T,
                                               radius_km=20)

# See what we have
ggplot(meas_bj_noaa, aes(x=date.x, y=temp_c)) + geom_point()

# Split train test
library(gbm)
library(tidyr)
library(lubridate)
pollutant <- creadb::PM25
training_years <- c(2015, 2016, 2017, 2018)
predict_years <- c(2019)
training_data <- meas_bj_noaa %>%
                  filter(poll==pollutant,
                         lubridate::year(date.x) %in% training_years)
predict_data <- meas_bj_noaa %>%
  filter(poll==pollutant,
         lubridate::year(date.x) %in% predict_years)

gbm.fit <- gbm(
  formula = value ~ temp_c + factor(wind_deg) + wind_ms + rh_percent + prec_6h_mm,
  distribution = "gaussian",
  data = training_data,
  n.trees = 5000,
  cv.folds = 5,
  verbose = FALSE
)
gbm.perf(gbm.fit, method = "cv")
summary(gbm.fit)

predict_data$predicted <- predict.gbm(gbm.fit,predict_data)
predict_plot_data <- predict_data %>%
                    filter_at(vars(temp_c, wind_deg, wind_ms, rh_percent), any_vars(!is.na(.))) %>%
                    select(date=date.x, measured=value, predicted) %>% tidyr::gather("type","value",measured,predicted)

ggplot(predict_plot_data, aes(x=date)) +
  geom_line(aes(y=value, colour=type)) +
  xlim(as.POSIXct("2019-01-01"), as.POSIXct("2019-01-31"))

