#----------------------------------------------------------
# This script merges covid data (exc. China) from coronadatascraper.com
# with CREADB Air Quality data available in the vicinity.
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
cor_df <- read.csv(url('https://coronadatascraper.com/timeseries.csv'))
cor_sf <- st_as_sf(cor_df %>%
                     dplyr::distinct(name, country, lat, long) %>%
                     dplyr::filter(!is.na(lat)) %>%
                     dplyr::filter(country != 'China'), coords = c("long", "lat"), crs=4326)

cor_buf_sf <- cor_sf %>%
  st_transform(crs=3857) %>%
  st_buffer(dist=radius_km*1000)

# Find Air Quality locations close to it
locs <- creadb::locations()
locs_sf <- st_as_sf(locs, crs=4326)
cor_buf_joined <- st_join(cor_buf_sf, locs_sf %>% st_transform(crs = 3857),
                          join=st_contains,
                          suffix=c(".cov",".aq"))
loc_ids <- setdiff(unique(cor_buf_joined$id),NA)

# Get air measurements
meas <- creadb::measurements(location_id=loc_ids,
                             date_from=date_from,
                             aggregate_level='city',
                             poll=poll)

# Convert measurements date to local date string to match covid data
meas <- meas %>%
  rowwise() %>%
  dplyr::mutate(date=strftime(date, format="%Y-%m-%d", tz=ifelse(!is.na(timezone), timezone, 'UTC')))

# Join back
# 1. add covid location name to measurements
cor_buf_joined <- cor_buf_joined %>%
  dplyr::distinct(name.cov, city) %>%
  dplyr::mutate(city=tolower(city)) %>%
  dplyr::right_join(meas %>% dplyr::select(date, city, poll, unit, timezone, value, source), by=c('city'))

# 2. merge with original dataset
# We do inner join to only keep measurements with both cov and aq data.
# Change to left_join if you want to keep the whole dataset
cor_aq_df <- cor_df %>%
  dplyr::inner_join(cor_buf_joined %>% dplyr::select(name.cov, date, poll, unit, value, source),
                   on=c("name"="name.cov", "date"="date"))
