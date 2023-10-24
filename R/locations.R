#' Locations
#'
#' @param level
#' @param id
#' @param country
#' @param city
#' @param type
#' @param source
#' @param source_city
#' @param collect
#' @param with_geometry
#' @param with_metadata
#' @param keep_with_measurements_only
#' @param con
#'
#' @return
#' @export
#'
#' @examples
locations <- function(
  level="city", #"city" or "station",
  id=NULL, #station_id or city_id depending on level

  # Other indirect search parameters
  country=NULL,
  city=NULL, #name
  type=NULL,
  source=NULL,
  source_city=NULL, #Nested source:[city]

  # Query parameters
  collect=TRUE,
  with_geometry=TRUE,
  with_metadata=FALSE,
  with_source=TRUE,
  keep_with_measurements_only=FALSE, #Only keep locations that actually have measurements
  con=NULL){

  if(length(setdiff(level, c("station","city")))){
    stop("level should be either 'station' or 'city' or both")
  }

  if("city" %in% level & !is.null(type)){
    warning("type is ignored when level=='city'")
  }

  if(!is.null(source) & !is.null(source_city)){
    warning("Cannot define both source and source_city. Overwriting with source_city")
    source <- NULL
  }

  if(is.null(source_city) & !is.null(source)){
    source_city <- utils.to_source_city(source, city)
    city <- NULL
  }

  result <- NULL
  con = if(!is.null(con)) con else connection()


  if("station" %in% level){
    if(!is.null(source_city)){
      # Some sources were indicated, with or without cities
      for(source_ in names(source_city)){
        r <- stations(id=id, source=source_, city=source_city[[source_]], country=country, type=type,
                      with_metadata=with_metadata, with_geometry=with_geometry, collect=F,
                      con=con)
        result <- if(is.null(result)){r}else{dplyr::union(result, r)}
      }
    }else{
      result <- stations(id=id, source=source, city=city, country=country, type=type,
                         with_metadata=with_metadata, with_geometry=with_geometry, collect=F,
                         con=con)
    }
  }

  if("city" %in% level){
    if(!is.null(source_city)){
      # Some sources were indicated, with or without cities
      for(source_ in names(source_city)){
        r <- cities(id=id, name=source_city[[source_]], country=country,
                    source=source_, with_metadata=with_metadata, with_geometry=with_geometry,
                    with_source=with_source,
                    collect=F, con=con) %>%
          dplyr::mutate(city_name=name)

        result <- if(is.null(result)){r}else{dplyr::union(result, r)}
      }
    }else{
      r <- cities(id=id, name=city, country=country, source=source,
                  with_metadata=with_metadata, with_source=with_source,
                  with_geometry=with_geometry, collect=F,
                  con=con) %>%
        dplyr::mutate(city_name=name)

      result <- if(is.null(result)){r}else{dplyr::union(result, r)}
    }
  }

  if(keep_with_measurements_only){
    # Only fast enough if location_id is indexed
    result <- result %>% dplyr::inner_join(tbl_safe(con, "measurements") %>% dplyr::distinct(location_id) %>% dplyr::select(id=location_id))
  }

  if(collect){
    result <- result %>% dplyr::collect()
    if(with_geometry){
      result <- result %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }
  }

  return(result)
}
