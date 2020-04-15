#----------------------------------------------------------
# This script merges covid data (exc. China) from coronadatascraper.com
# with CREADB Air Quality data available in the vicinity.
#----------------------------------------------------------
require(creadb)
require(sf)
require(ggplot2)
require(countrycode)
require(raster)
require(jsonlite)


# cache folder for GADM to save time
cache_folder <- file.path('cache')

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
cor_meta_json <- rjson::fromJSON(file='https://coronadatascraper.com/locations.json')
cor_meta_df <- do.call("bind_rows",lapply(cor_meta_json, function(x){
                      tryCatch({
                        df=as_tibble(as.data.frame(x[c('country','level','name')]))
                        df$longitude=x['coordinates'][[1]][1]
                        df$latitude=x['coordinates'][[1]][2]
                        df
                      }, error=function(err){
                        as_tibble(as.data.frame(x[c('country','level','name')]))
                      })
                }))
cor_meta_sf <- st_as_sf(cor_meta_df %>% filter(!is.na(latitude)),
                        coords=c('longitude','latitude'),
                        crs=4326)

# Identify which levels are most relevant for each country
# We consider the most granular level
# whose sum of cases is close to the country total
# 'close to': up to 30% discount if summing at city level
levels <- list("country"=0,"state"=1,"county"=2,"city"=3)
levels_bonus <- list("country"=1,"state"=1.5,"county"=1.8,"city"=2)

cor_levels_df <- cor_df %>% group_by(country, level) %>%
  summarise(cases=sum(cases, na.rm=T)) %>%
  group_by(country, level) %>%
  mutate(cases_bonus=cases*levels_bonus[[as.character(level)]]) %>%
  group_by(country) %>%
  arrange(desc(cases_bonus)) %>%
  top_n(1)

# Adding names
cor_at_levels_df <- cor_levels_df %>%
  select(country, level) %>%
  left_join(cor_df %>% distinct(country, level, name))

# Find corresponding GADM areas
find_centroid <- function(country_, level_, name_){
  tryCatch({
      (cor_meta_sf %>%
        filter(country==country_, level==level_, name==name_))
    }, error=function(err){NA})
}

find_geometry <-function(level, country, name){
  iso3 <- countrycode(country, origin='country.name', destination='iso3c')
  level_n <- levels[[level]]
  centroid <- find_centroid(country, level, name)
  gadms <- raster::getData('GADM', path=cache_folder, country=iso3, level=level_n)
  st_geometry(st_intersection(centroid, st_as_sf(gadms)))
}

cor_at_levels_geom_df <- cor_at_levels_df %>% filter(country!='United States') %>% head() %>% rowwise() %>%
  mutate(geometry=list(find_geometry(level, country, name)))


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
