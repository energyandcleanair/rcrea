measurements.api <- function(location_id = NULL,
                             city_name = NULL,
                             poll = NULL,
                             date_from = NULL,
                             date_to = NULL,
                             process_id = "city_day_mad",
                             source = NULL,
                             split_by_frequency = "year",
                             min_date = "2010-01-01",
                             use_local = F) {
  params <- list(
    location_id = paste0(location_id, collapse = ","),
    city_name = city_name,
    pollutant = paste0(poll, collapse = ","),
    date_from = date_from,
    date_to = date_to,
    process_id = process_id,
    source = source,
    format = "csv"
  )
  # Remove NULL values and empty values
  params <- params[sapply(params, function(x) !is.null(x) && x != "")]

  base_url <- ifelse(use_local, "http://localhost:8080", "https://api.energyandcleanair.org")
  url <- httr::parse_url(glue("{base_url}/v1/measurements"))
  url$query <- params
  url <- httr::build_url(url)

  if (nchar(url) > 2048) {
    message("URL too long. Splitting location_id or city_name into multiple requests.")
    if (!is.null(location_id)) {
      pbapply::pblapply(location_id, function(x) {
        measurements.api(
          location_id = x,
          poll = poll,
          date_from = date_from,
          date_to = date_to,
          process_id = process_id,
          source = source,
          split_by_frequency = split_by_frequency,
          use_local = use_local
        )
      }) %>%
        bind_rows() %>%
        return()
    } else if (!is.null(city_name)) {
      pbapply::pblapply(city_name, function(x) {
        measurements.api(
          city_name = x,
          poll = poll,
          date_from = date_from,
          date_to = date_to,
          process_id = process_id,
          source = source,
          split_by_frequency = split_by_frequency,
          use_local = use_local
        )
      }) %>%
        bind_rows() %>%
        return()
    } else {
      stop("Either location_id or city_name must be specified.")
    }
  }

  if (!is.null(split_by_frequency)) {
    date_from <- lubridate::date(coalesce(date_from, min_date))
    date_to <- coalesce(lubridate::date(date_to), lubridate::today())

    intervals <- utils.create_date_intervals(date_from, date_to, split_by_frequency)

    pbapply::pblapply(1:length(intervals$date_froms), function(i) {
      measurements.api(
        location_id = location_id,
        city_name = city_name,
        poll = poll,
        date_from = intervals$date_froms[i],
        date_to = intervals$date_tos[i],
        process_id = process_id,
        source = source,
        split_by_frequency = NULL,
        use_local = use_local
      )
    }) %>%
      bind_rows() %>%
      return()
  }

  tryCatch(
    {
      read_csv(url, show_col_types = FALSE)
    },
    error = function(error) {
      # Try once more
      return(read_csv(url, show_col_types = FALSE))
    }
  ) %>%
    return()
}
