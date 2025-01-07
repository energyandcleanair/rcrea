#' Query Measurements from CREA database
#'
#' @inheritParams measurements.dbplyr
#' @param use_api Whether to use the API endpoint (TRUE) or direct database connection (FALSE)
#' @return a tibble of measurements matching search criteria
#' @export
#'
#' @examples
#' # Using API (recommended)
#' meas_bj <- measurements(city='Beijing', date_from='2018-01-01', average_by='month')
#'
#' # Using database connection (legacy)
#' meas_bj <- measurements(city='Beijing', date_from='2018-01-01', use_api=FALSE)
measurements <- function(country=NULL,
                        city=NULL,
                        location_id=NULL,
                        location_type=NULL,
                        poll=NULL,
                        date_from='2015-01-01',
                        date_to=NULL,
                        source=NULL,
                        source_city=NULL,
                        process_id=NULL,
                        best_source_only=F,
                        average_by='day',
                        collect=TRUE,
                        user_filter=NULL,
                        with_metadata=FALSE,
                        with_geometry=FALSE,
                        aggregate_level='city',
                        deweathered=F,
                        population_weighted=F,
                        split_by_frequency='year',
                        con=NULL,
                        use_api=TRUE,
                        use_api_local=F,
                        verbose_api=F) {

  # Features that force using dbplyr
  must_use_dbplyr <- FALSE
  warnings <- c()

  if (!collect) {
    warnings <- c(warnings, "collect=FALSE is only available with database connection and will be deprecated")
    must_use_dbplyr <- TRUE
  }

  if (!is.null(user_filter)) {
    warnings <- c(warnings, "user_filter is only available with database connection and will be deprecated")
    must_use_dbplyr <- TRUE
  }

  if (best_source_only) {
    warnings <- c(warnings, "best_source_only is only available with database connection and will be deprecated")
    must_use_dbplyr <- TRUE
  }

  if (!is.null(source_city)) {
    warnings <- c(warnings, "source_city filtering is only available with database connection and will be deprecated")
    must_use_dbplyr <- TRUE
  }

  if (with_geometry) {
    warnings <- c(warnings, "with_geometry is only available with database connection and will be deprecated")
    must_use_dbplyr <- TRUE
  }

  # Output warnings if any
  if (length(warnings) > 0) {
    for (warning_msg in warnings) {
      warning(warning_msg)
    }
  }

  # Determine which method to use
  use_dbplyr <- must_use_dbplyr || !use_api

  if (use_dbplyr) {
    return(measurements.dbplyr(
      country = country,
      city = city,
      location_id = location_id,
      location_type = location_type,
      poll = poll,
      date_from = date_from,
      date_to = date_to,
      source = source,
      source_city = source_city,
      process_id = process_id,
      best_source_only = best_source_only,
      average_by = average_by,
      collect = collect,
      user_filter = user_filter,
      with_metadata = with_metadata,
      with_geometry = with_geometry,
      aggregate_level = aggregate_level,
      deweathered = deweathered,
      population_weighted = population_weighted,
      con = con
    ))
  } else {
    return(measurements.api(
      location_id = location_id,
      city_name = city,
      poll = poll,
      date_from = date_from,
      date_to = date_to,
      process_id = process_id,
      source = source,
      split_by_frequency = split_by_frequency,
      verbose = verbose_api,
      use_local = use_api_local
    ))
  }
}
