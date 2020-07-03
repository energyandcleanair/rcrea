library(testthat)

test_that("y-o-y", {

  values <- runif(365)
  dates <- seq(lubridate::date("2018-01-01"),
               lubridate::date("2019-12-31"),
               by="days")

  df <- tibble::tibble(value=c(values, values*2), date=dates)

  # Shuffle it
  df <- df[sample(nrow(df)),]

  # Test absolute y-o-y
  df_yoy <- utils.yoy(df, mode="absolute") %>% dplyr::arrange(date)
  df_yoy$value[is.na(df_yoy$value)] <- -999
  expect_true(all(df_yoy$value == c(rep(-999,365), values)))

  # Test y-o-y NA when no value
  expect_true(!is.na(df_yoy$value[df_yoy$date=="2019-06-01"]))
  df$value[df$date=="2018-06-01"] <- NA
  df_yoy <- utils.yoy(df, mode="absolute") %>% dplyr::arrange(date)
  expect_true(is.na(df_yoy$value[df_yoy$date=="2019-06-01"]))

  # Test relative y-o-y
  df <- tibble::tibble(value=c(values, values*2), date=dates)
  df_yoy <- utils.yoy(df, mode="relative") %>% dplyr::arrange(date)
  df_yoy$value[is.na(df_yoy$value)] <- -999
  expect_true(all(df_yoy$value == c(rep(-999, 365), rep(1, 365))))

  # Test y-o-y NA when no value
  expect_true(!is.na(df_yoy$value[df_yoy$date=="2019-06-01"]))
  df$value[df$date=="2018-06-01"] <- NA
  df_yoy <- utils.yoy(df, mode="absolute") %>% dplyr::arrange(date)
  expect_true(is.na(df_yoy$value[df_yoy$date=="2019-06-01"]))
})

test_that("running average", {

  values <- seq(1,365)
  dates <- seq(lubridate::date("2018-01-01"),
               lubridate::date("2018-12-31"),
               by="days")

  df <- tibble::tibble(value=values, date=dates)

  # Shuffle it
  df <- df[sample(nrow(df)),]

  # 0 and 1 running width should have no effect (except day averaging at some point?)
  df_0 <- utils.rolling_average(df, average_by = "day", average_width = 0,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  expect_true(all(df_0 %>% dplyr::arrange(date) %>% dplyr::pull(value)
                  == df %>% dplyr::arrange(date) %>% dplyr::pull(value)))

  df_1 <- utils.rolling_average(df, average_by = "day", average_width = 1,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  expect_true(all(df_1 %>% dplyr::arrange(date) %>% dplyr::pull(value) == df_1 %>% dplyr::arrange(date) %>% dplyr::pull(value)))

  # 2
  df_2 <- utils.rolling_average(df, average_by = "day", average_width = 2,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  expect_true(is.na(df_2$value[1]))
  expect_true(all(df_2$value[2:365]==seq(1.5,364.5)))

  # 5
  df_5 <- utils.rolling_average(df, average_by = "day", average_width = 5,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  all(is.na(df_5$value[1:4]))
  expect_true(all(df_5$value[5:365]==seq(3,363)))

  #  with NAs
  values <- c(1,2,3,NA,NA,NA,7,8,9)
  dates <- seq(lubridate::date("2018-01-01"),
               lubridate::date("2018-01-09"),
               by="days")
  df <- tibble::tibble(value=values, date=dates, group="1")
  utils.rolling_average(df, average_by = "day", average_width = 2,
                       vars_to_avg = "value", group_by_cols = "group",
                       min_values=0) %>%
    dplyr::arrange(date)

  utils.rolling_average(df, average_by = "day", average_width = 3,
                        vars_to_avg = "value", group_by_cols = "group",
                        min_values=2) %>%
    dplyr::arrange(date)




})
