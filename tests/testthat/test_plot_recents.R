library(testthat)

test_that("plot recents with various configs", {

  browser() # For debug

  poll <- c(rcrea::PM25, rcrea::NO2)
  city <- c('Delhi','Mumbai','Jaipur')
  folder <- "tmp_plots"
  source <- "cpcb"
  subfile_by <- "country"
  aggregate_level <- "city"
  running_days <- c(0, 7)

  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  file.remove(f)

  expect_error(
    plot_recents(
      folder=folder,
      source=source,
      city=city,
      poll=poll,
      range="full",
      subfile_by=subfile_by,
      aggregate_level=aggregate_level,
      running_days=running_days
    ),
    NA)

  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 3 * length(running_days))
  file.remove(f)

  expect_error(
    plot_recents(
      folder=folder,
      source=source,
      city=city,
      poll=poll,
      subfile_by=subfile_by,
      aggregate_level=aggregate_level,
      running_days=running_days,
      process_id="trend_gbm_lag1"
    ),
    NA)
  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 6 * length(running_days))
  file.remove(f)

  # Deweathered EEA
  expect_error(
    plot_recents(
      folder=folder,
      source="eea",
      city=c("Paris","London"),
      poll=rcrea::NO2,
      process_id="anomaly_offsetted_gbm_lag1_city_mad",
      subfile_by="poll",
      subplot_by = "location_id",
      aggregate_level="city",
      running_days=14,
    ),
    NA)
  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 6 * 1 * length(running_days))
  file.remove(f)

  expect_error(
    plot_recents(
      folder=folder,
      source=source,
      city=city,
      poll=poll,
      subfile_by="poll",
      subplot_by = "location_id",
      aggregate_level=aggregate_level,
      running_days=running_days,
      process_id="trend_gbm_lag1"
    ),
    NA)
  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 6 * length(poll) * length(running_days))
  file.remove(f)

})
