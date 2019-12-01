library(creadb)
locs_india <- creadb::locations(country='IN')


str(locs_india)
unique(locs_india$city)


meas_delhi <- creadb::measurements(city='Delhi', average_by='day')
meas_delhi_mumbai<- creadb::measurements(city=c('Delhi','Mumbai'), average_by='day',
                                          pollutant='pm25', date_from='2017-01-01')

plot_measurements(meas_delhi_mumbai, creadb::PM25, running_days=365, color_by='city') # Can be CO, PM25, NO2, O3, PM10, SO2

plot_measurements(meas_delhi_mumbai, running_days=365, subplot_by=c('city','parameter')) # Can be CO, PM25, NO2, O3, PM10, SO2

