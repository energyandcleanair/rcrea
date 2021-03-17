

facilities <- function(
  id=NULL,
  name=NULL,
  country=NULL, #ISO2
  source=NULL,
  with_units=T,
  with_geometry=F,
  collect=T,
  con=NULL
){

  # Connecting
  con = if(!is.null(con)) con else connection()
  c <- tbl_safe(con, "facilities")

  # Filter: id
  if(!is.null(id)){
    c <- c %>% dplyr::filter(id %in% !!tolower(id))
  }

  # Filter: name
  if(!is.null(name)){
    c <- c %>% dplyr::filter(tolower(name) %in% !!tolower(name))
  }

  # Filter: country
  if(!is.null(country)){
    c <- c %>% dplyr::filter(country_id %in% !!toupper(country))
  }

  # Filter: source
  if(!is.null(source)){
    c <- c %>% dplyr::filter(source %in% !!tolower(source))
  }

  c <- c %>% dplyr::rename(country=country_id)

  # Attach units if required
  if(with_units){
    c <- c %>% dplyr::left_join(tbl_safe(con, "units") %>%
                           dplyr::rename(unit_id=id, id=facility_id))
  }

  if(!with_geometry){
    c <- c %>% dplyr::select(-c(geometry))
  }

  if(collect){
    c <- c %>% dplyr::collect()
    if(with_geometry){
      c <- c %>% dplyr::mutate(geometry=sf::st_as_sfc(geometry))
    }
  }

  return(c)
}


emissions <- function(
  facility_id,
  date_from,
  date_to,
  poll
){







}
