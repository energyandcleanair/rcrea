library(testthat)

test_that("health impact works", {

  country <- NULL
  city <- c("Paris") #,"Madrid","London")
  source <- "eea"

  m <- rcrea::measurements(city=city,
                           country=country,
                           deweathered = NULL,
                           date_from="2020-04-01",
                           source=source,
                           with_metadata = T,
                           with_geometry = T)

  m.scenarios <- m %>%
    rcrea::utils.add_city_pop() %>%
    rcrea::health.build.scenarios()

  i.detailed.1m <- health.impact(m.scenarios, date_from="2020-04-01", date_to="2020-04-30")
  i.simplified.1m <- health.simplify(i.detailed.1m)

  i.detailed.3m <- health.impact(m.scenarios, date_from="2020-04-01", date_to="2020-06-30")
  i.simplified.3m <- health.simplify(i.detailed.3m)

  i.detailed.vs <- i.simplified.1m %>% left_join(i.simplified.3m, by=c("location_id"))
  expect_true(all(i.detailed.vs$deaths.y > i.detailed.vs$deaths.x))
  expect_true(all(i.detailed.vs$cost.mlnUSD.y > i.detailed.vs$cost.mlnUSD.x))

  expect_equal(i.detailed.1m$location_id %>% unique() %>% sort(),
               m$location_id %>% unique() %>% sort())

  expect_equal(i.simplified.1m$location_id %>% unique() %>% sort(),
               m$location_id %>% unique() %>% sort())

  expect_gt(min(i.simplified.1m$deaths), 100)
  expect_lt(max(i.simplified.1m$deaths), 200)

  # No change
  m.nochange <- m.scenarios
  m.nochange$value.counterfactual=m.nochange$value.observation
  i.detailed <- health.impact(m.nochange, date_from="2020-04-01", date_to="2020-04-05") # For city of one inhabitant
  i.simplified <- health.simplify(i.detailed)
  expect_equal(sum(i.simplified$deaths), 0)
  expect_equal(sum(i.simplified$cost.mlnUSD), 0)

})
