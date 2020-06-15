library(testthat)

test_that("plot recents with various configs", {

  browser() # For debug

  polls <- c(rcrea::PM25, rcrea::NO2)
  city <- c('Delhi','Mumbai', 'Jaipur')
  folder <- "tmp_plots"
  source <- "cpcb"
  subfile_by <- "country"
  aggregate_level <- "city"
  runnings <- c(0, 7)

  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  file.remove(f)

  expect_error(
    plot_recents(
      folder=folder,
      source=source,
      city=city,
      polls=polls,
      subfile_by=subfile_by,
      aggregate_level=aggregate_level,
      runnings=runnings
    ),
    NA)

  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 6 * length(runnings))
  file.remove(f)

  expect_error(
    plot_recents(
      folder=folder,
      source=source,
      city=city,
      polls=polls,
      subfile_by=subfile_by,
      aggregate_level=aggregate_level,
      runnings=runnings,
      process_id="trend_gbm_lag1"
    ),
    NA)
  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 6 * length(runnings))
  file.remove(f)

  expect_error(
    plot_recents(
      folder=folder,
      source=source,
      city=city,
      polls=polls,
      subfile_by="poll",
      subplot_by = "region_id",
      aggregate_level=aggregate_level,
      runnings=runnings,
      process_id="trend_gbm_lag1"
    ),
    NA)
  f <- list.files(folder, pattern = "*.png", include.dirs = T, full.names = T, recursive = F)
  expect_equal(length(f), 6 * length(polls) * length(runnings))
  file.remove(f)

}
