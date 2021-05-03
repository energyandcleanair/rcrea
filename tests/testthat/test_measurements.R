
test_that("source_city query works on measurements", {

  source_city=list("eea"=c("paris","rome"), #several
                   "openaq"="paris",#one
                   "jp"=c(), #all
                   "cpcb"="") #none

  m <- measurements(source_city = source_city, with_metadata=T, poll="no2", date_from="2020-03-01", date_to="2020-03-02")

  expect_true(identical(
    m %>% filter(source=="eea") %>% distinct(location_name) %>% arrange(location_name) %>% pull() %>% tolower(),
    source_city[["eea"]] %>% sort() %>% tolower()
  ))

  expect_true(identical(
    m %>% filter(source=="openaq") %>% distinct(location_name) %>% arrange(location_name) %>% pull() %>% tolower(),
    source_city[["openaq"]] %>% sort() %>% tolower()
  ))

  expect_gt(m %>% filter(source=="jp") %>% distinct(location_id) %>% count() %>% pull(), 46)
  expect_equal(m %>% filter(source=="cpcb") %>% distinct(location_id) %>% count() %>% pull(), 0)

  m <- measurements(source="eea", with_metadata=T, date_from="2020-01-01", date_to="2020-01-02", poll="no2")
  expect_gt( m %>% filter(source=="eea") %>% distinct(location_id) %>% count() %>% pull(), 100)
  expect_equal( m %>% filter(source!="eea") %>% distinct(location_id) %>% count() %>% pull(), 0)

  m <- measurements(source="eea", city="paris", date_from="2020-01-01", date_to="2020-01-02", with_metadata=T)
  expect_equal( m %>% filter(source=="eea") %>% distinct(location_id) %>% count() %>% pull(), 1)

  m <- measurements(source=NULL, city="paris", country="FR", date_from="2020-01-01", date_to="2020-01-02", with_metadata=T)
  expect_gt(length(unique(m$source)),1)
  expect_equal( m %>% filter(tolower(location_name)!="paris") %>% distinct(location_name) %>% count() %>% pull(), 0)
  expect_gte( m %>% filter(tolower(location_name)=="paris") %>% distinct(source) %>% count() %>% pull(), 2) #At least EEA and OpenAQ for Paris

})

test_that("source filtering works", {

  m.eea <- measurements(source="eea", city="paris", date_from="2020-12-01", date_to="2020-12-03", poll="no2")
  expect_equal(unique(m.eea$source), "eea")

  m.eea <- measurements(source="eea", city="paris", date_from="2020-12-01", date_to="2020-12-03", poll="no2",
                        location_type = "background")
  expect_equal(unique(m.eea$source), "eea")


  # Select a city which we know has several sources
  m <- measurements(source=NULL, city="paris", country="FR", date_from="2020-01-01", date_to="2020-01-03", poll="no2", with_metadata=T)
  expect_gt(length(unique(m$source)),1)

  for(source in unique(m$source)){
    m.source <- measurements(source=source, city="paris", date_from="2020-12-01", date_to="2020-12-03", poll="no2")
    expect_equal(nrow(m %>% filter(source==!!source)),
                 nrow(m.source))
  }

})


test_that("gadm or country aggregation works on measurements", {

  m.gadm2 <- measurements(source="mee",
                    aggregate_level = "gadm2",
                    poll="no2",
                    city=c("Shanghai","Beijing"), # To make it faster
                    date_from="2020-03-01",
                    date_to="2020-03-02")


  expect_equal(c("chn.2.1_1","chn.24.1_1"),
               sort(unique(m.gadm2$location_id)))

  m.gadm2 <- measurements(source="mee",
                          aggregate_level = "gadm2",
                          poll="no2",
                          city=c("Shanghai","Beijing"), # To make it faster
                          date_from="2020-03-01",
                          date_to="2020-03-02",
                          with_metadata=T)

  expect_equal(c("chn.2.1_1","chn.24.1_1"),
               sort(unique(m.gadm2$location_id)))


  m.gadm2 <- measurements(location_id="chn.2.1_1",
                          source="mee",
                          aggregate_level = "gadm2",
                          poll="no2",
                          date_from="2020-03-01",
                          date_to="2020-03-02")

  expect_equal(c("chn.2.1_1"),
               sort(unique(m.gadm2$location_id)))


  expect_equal(c("chn.2.1_1","chn.24.1_1"),
               sort(unique(m.gadm2$location_id)))


  m.gadm1 <- measurements(source="mee",
                          aggregate_level = "gadm1",
                          poll="no2",
                          city=c("Shanghai","Beijing"), # To make it faster
                          date_from="2020-03-01",
                          date_to="2020-03-02")

  expect_equal(c("chn.2_1","chn.24_1"),
               sort(unique(m.gadm1$location_id)))

  m.country <- measurements(source="mee",
                          aggregate_level = "country",
                          poll="no2",
                          city=c("Shanghai","Beijing"), # To make it faster
                          date_from="2020-03-01",
                          date_to="2020-03-02")

  expect_equal(c("chn"),
               sort(unique(m.country$location_id)))
})
