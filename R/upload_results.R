
db_writing <- function(){
  # Create one
  try(dotenv::load_dot_env(file = ".env"))
  try(readRenviron(".Renviron"))
  db_url <- Sys.getenv("CREA_DB_URL")
  if(db_url==""){
    stop("Missing database url. Please define CREA_DB_URL in your environment")
  }
  db <- dbx::dbxConnect(url=Sys.getenv("CREA_DB_URL"))
  return(db)
}

create_new_process_id <- function(preferred_id=NULL){
  id_0 <- ifelse(!is.null(preferred_id),
                 preferred_id,
              paste0('process_', format(Sys.time(), "%Y%m%d_%H%M%S")))

  existing_ids <- processes() %>%
    dplyr::distinct(id) %>%
    dplyr::collect() %>%
    dplyr::pull(id)

  id <- id_0
  v <- 2
  while(id %in% existing_ids){
    id <- paste0(id_0,"_v",v)
    v <- v+1
  }
  return(id)
}


#' Retrieve or create process in database
#'
#' After processing data, you may want to upload it in crea database. Yet you need
#' a process record to define what processing the measurements went through.
#' This function helps you either retrieve or create a new process with given parameters.
#'
#' @param filter json definition of filter used
#' @param agg_spatial json definition of spatial aggregation
#' @param agg_temp json definition of temporal aggregation
#' @param deweather json definition of deweathering applied
#'
#' @return id (string) of the process found or created
#' @export
#'
retrieve_or_create_process <- function(filter, agg_spatial, agg_temp, deweather, preferred_name=NULL){

  safe_fromJSON <- function(x){
    if(is.na(x)){return(NA)}
    if(x=='null'){return(NULL)}
    jsonlite::fromJSON(x)
  }
  equal_lists <- function(l1, l2) {

    if(is.null(l1) && length(l2)==1 && is.null(l2[[1]])){return(T)}
    if(is.null(l2) && length(l1)==1 && is.null(l1[[1]])){return(T)}

    # Test it two named lists are equal, ignoring order
    length(setdiff(l1,l2)) + length(setdiff(l2,l1)) ==0
  }

  # Check existing process
  p=processes() %>% dplyr::collect() %>%
    dplyr::mutate(
      filter=purrr::map(filter, safe_fromJSON),
      agg_spatial=purrr::map(agg_spatial, safe_fromJSON),
      agg_temp=purrr::map(agg_temp, safe_fromJSON),
      deweather=purrr::map(deweather, safe_fromJSON)) %>%
    dplyr::mutate(
      filter_equal=purrr::map_lgl(filter, equal_lists, l2=!!safe_fromJSON(filter)),
      agg_spatial_equal=purrr::map_lgl(agg_spatial, equal_lists, l2=!!safe_fromJSON(agg_spatial)),
      agg_temp_equal=purrr::map_lgl(agg_temp, equal_lists, l2=!!safe_fromJSON(agg_temp)),
      deweather_equal=purrr::map_lgl(deweather, equal_lists, l2=!!safe_fromJSON(deweather))) %>%
    dplyr::filter(
      filter_equal & agg_spatial_equal &agg_temp_equal & deweather_equal
      ) %>%
    dplyr::select(id, filter, agg_spatial, agg_temp, deweather)

  if(nrow(p)==0){
    db <- db_writing()
    id <- create_new_process_id(preferred_name)
    p <- tibble::tibble(id=id,filter=filter,agg_spatial=agg_spatial,agg_temp=agg_temp,deweather=deweather)
    dbx::dbxInsert(db, "processes", p)
    dbx::dbxDisconnect(db)
    return(id)
  }else if(nrow(p)==1){
    return(p$id[1])
  }else{
    warning("Found several processes matching filters. Should never happen. Returning first one nonetheless")
    return(p$id[1])
  }
}


#' Upsert processed measurements in crea database
#'
#' @param meas
#'
#' @return
#' @export
#'
#' @examples
upsert_meas <- function(meas){

  meas <- meas %>% dplyr::rename(pollutant=poll)


  required_cols <- c("date","pollutant","unit","region_id","process_id","source","value")
  if(!all(required_cols %in% colnames(meas))){
    stop(paste("Missing columns ", paste(setdiff(required_cols, colnames(meas)))))
  }

  db <- db_writing()
  tryCatch({
    # We upload by chunks to avoid SSL EOF error
    ms <- split(meas, (seq(nrow(meas))-1) %/% 100000)

    upload_chunk <- function(m){
      dbx::dbxUpsert(db, "measurements_new", m %>% dplyr::select(all_of(required_cols)),
                     where_cols=c('date', 'pollutant', 'unit', 'process_id', 'region_id', 'source'))
    }

    lapply(ms, upload_chunk)

  }, error=function(err){
    dbx::dbxDisconnect(db)
    stop(paste("Upserting failed:",err))
  })
  dbx::dbxDisconnect(db)
}

#' Upsert locations in crea database
#'
#' @param meas
#'
#' @return
#' @export
#'
#' @examples
upsert_locations <- function(locs){

  required_cols <- c("id","name","city","country","timezone","source","geometry","type")
  if(!all(required_cols %in% colnames(locs))){
    stop(paste("Missing columns ", paste(setdiff(required_cols, colnames(locs)))))
  }

  db <- db_writing()
  tryCatch({
    dbx::dbxUpsert(db, "locations", locs %>% dplyr::select(all_of(required_cols)),
                   where_cols=c('id'))
  }, error=function(err){
    dbx::dbxDisconnect(db)
    stop(paste("Upserting failed:",err))
  })
  dbx::dbxDisconnect(db)
}


