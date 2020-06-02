
db_writing <- function(){
  # Create one
  load_dot_env(file = ".env")
  db_url <- Sys.getenv("CREA_DB_URL")
  if(db_url==""){
    stop("Missing database url. Please define CREA_DB_URL in your environment")
  }
  db <- dbx::dbxConnect(url=Sys.getenv("CREA_DB_URL"))
  return(db)
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

  # Check existing process
  p=processes() %>% collect() %>%
    dplyr::filter(filter==filter,
                  agg_spatial==!!agg_spatial,
                  agg_temp==!!agg_temp,
                  deweather==!!deweather) %>%
    dplyr::select(id, filter, agg_spatial, agg_temp, deweather)

  if(nrow(p)==0){
    db <- db_writing()
    id = ifelse(!is.null(preferred_name),
            preferred_name,
            paste0('Process_', format(Sys.time(), "%Y%m%d_%H%M%S")))
    p <- tibble(id=id,filter=filter,agg_spatial=agg_spatial,agg_temp=agg_temp,deweather=deweather)
    dbx::dbxUpsert(db, "processes", p ,where_cols=c('id'))
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

  meas <- meas %>% rename(pollutant=poll)

  required_cols <- c("date","pollutant","unit","region_id","process_id","source","value")
  if(!all(required_cols %in% colnames(meas))){
    stop(paste("Missing columns ", paste(setdiff(required_cols, colnames(meas)))))
  }

  db <- db_writing()
  tryCatch({
    dbx::dbxUpsert(db, "measurements_new", meas %>% dplyr::select(all_of(required_cols)),
              where_cols=c('date', 'pollutant', 'unit', 'process_id', 'region_id', 'source'))
  }, error=function(err){
    dbx::dbxDisconnect(db)
    stop(paste("Upserting failed:",err))
  })
  dbx::dbxDisconnect(db)
}


