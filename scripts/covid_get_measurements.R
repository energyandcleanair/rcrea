#----------------------------------------------------------
# This script merges covid data from coronadatascraper.com
# with CREA Air Quality data available in the vicinity.
#----------------------------------------------------------
require(creadb)
require(sf)
require(ggplot2)

#-----------------
# Key parameters
#-----------------
radius_km <- 20
date_from <- '2020-04-01'
poll <- c(creadb::PM25)

#-----------------
# Execution
#-----------------
# Get COVID timeseries and locations of interest
cor_csv <- read.csv(url('https://coronadatascraper.com/timeseries.csv'))
cor_sf <- st_as_sf(cor_csv %>%
                     distinct(name, country, lat, long) %>%
                     filter(!is.na(lat)) %>%
                     filter(country != 'China'), coords = c("long", "lat"), crs=4326)

cor_buf_sf <- cor_sf %>%
  st_transform(crs=3857) %>%
  st_buffer(dist=radius_km*1000)

# Find Air Quality locations close to it
locs <- creadb::locations()
locs_sf <- st_as_sf(locs, crs=4326)
cor_buf_joined <- st_join(cor_buf_sf, locs_sf %>% st_transform(crs = 3857), join=st_contains, suffix=c(".cov",".aq"))
loc_ids <- setdiff(unique(cor_buf_joined$id),NA)

# Get air measurements
meas <- creadb::measurements(location_id=loc_ids,
                             date_from=date_from,
                             aggregate_level='city',
                             poll=poll)

# Convert measurements date to local date string to match covid data
meas <- meas %>% rowwise() %>% mutate(
  date =strftime(date, format="%Y-%m-%d", tz=ifelse(!is.na(timezone), timezone, 'UTC'))
)

# Join back
# 1. add covid location name to measurements
cor_buf_joined <- cor_buf_joined %>% select(name.cov, city) %>% mutate(city=tolower(city)) %>%
  right_join(meas %>% select(date, city, poll, unit, timezone, value, source), by=c('city'))

# 2. merge with original dataset
cor_csv_w_aq <- cor_csv %>% left_join(cor_buf_joined %>% select(name.cov, date, poll, unit, value, source), on=c("name"="name.cov", "date"="date"))


