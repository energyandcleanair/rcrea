gee.init <- function(){
  ee_check()
  ee_Initialize(user = 'ndef', drive = TRUE)
}

gee.get_srtm <- function(){
  srtm <- ee$Image('CGIAR/SRTM90_V4') %>%
          ee$Image$toFloat() %>%
          ee$Image$rename('Elevation')
  return(srtm)
}

gee.get_pop <- function(){
  pop <- ee$ImageCollection("CIESIN/GPWv411/GPW_Population_Density") %>%
          ee$ImageCollection$sort('system:time_start', F) %>%
          ee$ImageCollection$first() %>%
          ee$Image$toFloat() %>%
          ee$Image$rename('Population Density')
  return(pop)
}

gee.get_lat <- function(){
  lat <- ee$Image$pixelLonLat()$select('latitude') %>%
          ee$Image$toFloat() %>%
          ee$Image$rename('Latitude')
  return(lat)
}

gee.get_lon <- function(){
  lon <- ee$Image$pixelLonLat()$select('longitude') %>%
          ee$Image$toFloat() %>%
          ee$Image$rename('Longitude')
  return(lon)
}

gee.get_lc <- function(date_from, date_to){
  lc <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1") %>%
        ee$ImageCollection$filterDate(date_from, date_to) %>%
        ee$ImageCollection$select('label') %>%
        ee$ImageCollection$mosaic() %>%
        ee$Image$toFloat() %>%
        ee$Image$rename('Land Cover')
  return(lc)
}

gee.get_distcoast <- function(max_dist=4000000){
  water_mask <- ee$Image('MODIS/MOD44W/MOD44W_005_2000_02_24') %>%
                ee$Image$select('water_mask')

  euclidean_kernel <- ee$Kernel$euclidean(max_dist, 'meters')
  euclidean_dist_um <- water_mask$distance(euclidean_kernel)
  dist_coast <- euclidean_dist_um$updateMask(water_mask$Not())%>%
                ee$Image$reproject(ee$Projection('EPSG:4326')) %>%
                ee$Image$toFloat() %>%
                ee$Image$rename('Distance to Coast')
  return(dist_coast)
}

gee.get_disturban <- function(max_dist=400000){
  urban_mask <- ee$Image('DLR/WSF/WSF2015/v1') %>%
                ee$Image$unmask()

  euclidean_kernel <- ee$Kernel$euclidean(max_dist, 'meters')
  dist_urban <- urban_mask$distance(euclidean_kernel) %>%
                  ee$Image$toFloat() %>%
                  ee$Image$rename('Distance to Urban')
  return(dist_urban)
}

gee.get_pm25 <- function(){
  pm25 <- ee$Image('projects/quickstart-1595845282060/assets/pm25') %>%
          ee$Image$toFloat() %>%
          ee$Image$rename('PM25')
  return(pm25)
}

gee.get_o3 <- function(date_from, date_to){
  date_from_dt <- ymd(date_from,tz='UTC')
  date_to_dt <- ymd(date_to,tz='UTC')

  if (date_from_dt >= date_to_dt){
    cat('\n[WARNING] date_from cannot be greater or equal to date_to.
        \nSetting date_to = date_from + 1')
    date_to_dt = date_from_dt + days(1)
  }

  date_from <- format(date_from_dt, "%Y-%m-%d")
  date_to <- format(date_to_dt, "%Y-%m-%d")

  o3 <- ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_O3") %>%
    ee$ImageCollection$filterDate(date_from, date_to) %>%
    ee$ImageCollection$select('O3_column_number_density') %>%
    ee$ImageCollection$mean() %>%
    ee$Image$toFloat() %>%
    ee$Image$rename('O3 Column Density')
  return(o3)
}

gee.get_no2 <- function(date_from, date_to){
  date_from_dt <- ymd(date_from,tz='UTC')
  date_to_dt <- ymd(date_to,tz='UTC')

  if (date_from_dt >= date_to_dt){
    cat('\n[WARNING] date_from cannot be greater or equal to date_to.
        \nSetting date_to = date_from + 1')
    date_to_dt = date_from_dt + days(1)
  }

  date_from <- format(date_from_dt, "%Y-%m-%d")
  date_to <- format(date_to_dt, "%Y-%m-%d")

  no2 <- ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_NO2") %>%
    ee$ImageCollection$filterDate(date_from, date_to) %>%
    ee$ImageCollection$select('tropospheric_NO2_column_number_density') %>%
    ee$ImageCollection$mean() %>%
    ee$Image$toFloat() %>%
    ee$Image$rename('Tropospheric NO2 Column Density')
  return(no2)
}

gee.get_data <- function(date_from,
                         date_to,
                         gee_poly=NULL,
                         sf_obj=NULL,
                         bbox_obj=NULL,
                         coords_vector_list=NULL,
                         res=50000){
  # The res argument should not be over 78000

  if (!dir.exists('results')){
    dir.create('results')
  }

  if (!is.null(gee_poly)){
    roi <- gee_poly
  } else if (!is.null(sf_obj)){
    roi <- sf_as_ee(sf_obj)$geometry()
  } else if (!is.null(bbox_obj)){
    x_min <- bbox_obj[1]
    y_min <- bbox_obj[2]
    x_max <- bbox_obj[3]
    y_max <- bbox_obj[4]
    roi <- ee$Geometry$Rectangle(list(x_min, y_min, x_max, y_max))
  } else if (!is.null(coords_vector_list)){
    roi <- ee$Geometry$Polygon(coords_vector_list)
  } else {
    roi <- NULL
  }

  img_stack <- gee.get_srtm() %>%
    ee$Image$addBands(gee.get_pop()) %>%
    ee$Image$addBands(gee.get_lat()) %>%
    ee$Image$addBands(gee.get_lon()) %>%
    ee$Image$addBands(gee.get_lc(date_from, date_to)) %>%
    ee$Image$addBands(gee.get_distcoast()) %>%
    ee$Image$addBands(gee.get_disturban()) %>%
    ee$Image$addBands(gee.get_pm25()) %>%
    ee$Image$addBands(gee.get_o3(date_from, date_to)) %>%
    ee$Image$addBands(gee.get_no2(date_from, date_to))

  img_stack_raster <- ee_as_raster(img_stack,
                                   region=roi,
                                   via='drive',
                                   scale=res)

  terra_ras <- terra::rast(img_stack_raster)
  terra::writeRaster(terra_ras,
                      glue('results/stack_{date_from}_{date_to}.tif'),
                      overwrite=TRUE)
  return(img_stack_raster)
}





