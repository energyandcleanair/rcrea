library(creadb)
library(ggplot2)
library(dplyr)

locs_india <- creadb::locations(country='IN')


str(locs_india)
unique(locs_india$city)


# Get data
# meas_delhi <- creadb::measurements(city='Delhi')
# meas_jaipur_pm25<- creadb::measurements(city=c('Delhi','Mumbai'), poll=creadb::PM25, date_from='2017-01-01')
meas_cities <- creadb::measurements(city=c('Delhi','Mumbai'), date_from='2018-01-01')

# Time series per location
creadb::plot_measurements(meas_cities, poll=creadb::PM25, subplot_by='location_id')
creadb::plot_measurements(meas_cities, poll=creadb::PM25, subplot_by='location')

# Time series per pollutant (average per city is taken by default)
creadb::plot_measurements(meas_cities, subplot_by='poll')
creadb::plot_measurements(meas_cities, running_width=365, subplot_by='poll')


# Time series of PM2.5 per location (yearly running average)
creadb::plot_measurements(meas_cities, poll=creadb::PM25, running_width=365, subplot_by='location')

# Time series of PM2.5 per city (yearly running average)
creadb::plot_measurements(meas_cities, poll=creadb::PM25, running_width=365, color_by='city')

# Monthly average
creadb::plot_measurements(meas_cities, poll=creadb::PM25, average_by='month', subplot_by='city')

# Yearly average
creadb::plot_measurements(meas_cities, poll=creadb::PM25, average_by='year', subplot_by='city')

# Heatmap per location with monthly data
creadb::plot_measurements(meas_cities, poll=PM25, subplot_by='location', type='heatmap', average_by='month')

# Heatmap per city weekly values
creadb::plot_measurements(meas_cities, poll=PM25, subplot_by='city', type='heatmap', average_by='month')



##############################
## Example 2: yearly averages
##############################
meas_country <- creadb::measurements(country='IN', date_from='2015-01-01', average_by='year')
creadb::plot_measurements(meas_country, poll=PM25, subplot_by='city', type='heatmap', average_by='year')
creadb::plot_measurements(meas_country, poll=PM25, subplot_by='city', type='ts', average_by='year')

