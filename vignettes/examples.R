library(rcrea)
library(ggplot2)
library(dplyr)

locs_india <- rcrea::locations(country='IN')


str(locs_india)
unique(locs_india$city)


# Get data
# meas_delhi <- rcrea::measurements(city='Delhi')
# meas_jaipur_pm25<- rcrea::measurements(city=c('Delhi','Mumbai'), poll=rcrea::PM25, date_from='2017-01-01')
meas_cities <- rcrea::measurements(city=c('Delhi','Mumbai'), date_from='2018-01-01')

# Time series per location
rcrea::plot_measurements(meas_cities, poll=rcrea::PM25, subplot_by='location_id')
rcrea::plot_measurements(meas_cities, poll=rcrea::PM25, subplot_by='location')

# Time series per pollutant (average per city is taken by default)
rcrea::plot_measurements(meas_cities, subplot_by='poll')
rcrea::plot_measurements(meas_cities, running_width=365, subplot_by='poll')


# Time series of PM2.5 per location (yearly running average)
rcrea::plot_measurements(meas_cities, poll=rcrea::PM25, running_width=365, subplot_by='location')

# Time series of PM2.5 per city (yearly running average)
rcrea::plot_measurements(meas_cities, poll=rcrea::PM25, running_width=365, color_by='city')

# Monthly average
rcrea::plot_measurements(meas_cities, poll=rcrea::PM25, average_by='month', subplot_by='city')

# Yearly average
rcrea::plot_measurements(meas_cities, poll=rcrea::PM25, average_by='year', subplot_by='city')

# Heatmap per location with monthly data
rcrea::plot_measurements(meas_cities, poll=PM25, subplot_by='location', type='heatmap', average_by='month')

# Heatmap per city weekly values
rcrea::plot_measurements(meas_cities, poll=PM25, subplot_by='city', type='heatmap', average_by='month')



##############################
## Example 2: yearly averages
##############################
meas_country <- rcrea::measurements(country='IN', date_from='2015-01-01', average_by='year')
rcrea::plot_measurements(meas_country, poll=PM25, subplot_by='city', type='heatmap', average_by='year')
rcrea::plot_measurements(meas_country, poll=PM25, subplot_by='city', type='ts', average_by='year')

