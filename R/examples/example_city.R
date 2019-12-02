library(creadb)
locs_india <- creadb::locations(country='IN')


str(locs_india)
unique(locs_india$city)


# Get data
# meas_delhi <- creadb::measurements(city='Delhi')
# meas_jaipur_pm25<- creadb::measurements(city=c('Delhi','Mumbai'), poll=creadb::PM25, date_from='2017-01-01')
meas_delhi_mumbai<- creadb::measurements(city=c('Delhi','Mumbai'), date_from='2019-01-01')

# Time series per location
plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, subplot_by='location')

# Time series per pollutant (average per city is taken by default)
plot_measurements(meas_delhi_mumbai, subplot_by='poll')

# Time series of PM2.5 per location (yearly running average)
plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, running_days=365, subplot_by='location')

# Time series of PM2.5 per city (yearly running average)
plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, running_days=365, subplot_by='city')

# Monthly average
plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, average_by='month', subplot_by='city')

# Yearly average
plot_measurements(meas_delhi_mumbai, poll=creadb::PM25, average_by='year', subplot_by='city')

# Heatmap per location with monthly data
plot_measurements(meas_delhi_mumbai, poll=PM25, subplot_by='location', type='heatmap', average_by='month')

# Heatmap per city weekly values
plot_measurements(meas_delhi_mumbai, poll=PM25, subplot_by='city', type='heatmap')



