
test_that("source_query works on locations (city)", {

  source_city=list("eea"=c("paris","rome"), #several
                   "openaq"="paris",#one
                   "jp"=c(), #all
                   "cpcb"="") #none
  l <- locations(level="city", source_city = source_city, with_metadata=T)

  expect_true(identical(
    l %>% filter(source=="eea") %>% distinct(city_name) %>% arrange(city_name) %>% pull() %>% tolower(),
    source_city[["eea"]] %>% sort() %>% tolower()
  ))

  expect_true(identical(
    l%>% filter(source=="openaq") %>% distinct(city_name) %>% arrange(city_name) %>% pull() %>% tolower(),
    source_city[["openaq"]] %>% sort() %>% tolower()
  ))

  expect_gt(l%>% filter(source=="jp") %>% distinct(city_name) %>% count() %>% pull(), 47)
  expect_equal(l%>% filter(source=="cpcb") %>% distinct(city_name) %>% count() %>% pull(), 0)

  l <- locations(source="eea", with_metadata=T)
  expect_gt( l%>% filter(source=="eea") %>% distinct(city_name) %>% count() %>% pull(), 100)
  expect_equal( l%>% filter(source!="eea") %>% distinct(city_name) %>% count() %>% pull(), 0)

  l <- locations(source="eea", city="paris", with_metadata=T)
  expect_equal( l%>% filter(source=="eea") %>% distinct(city_name) %>% count() %>% pull(), 1)

  l <- locations(source=NULL, city="paris", with_metadata=T)
  expect_equal( l%>% filter(tolower(city)!="paris") %>% distinct(city_name) %>% count() %>% pull(), 0)

  l <- locations(level="city", source="mee", country="FR", with_metadata=F)
  expect_equal( l%>% distinct(country_id) %>% pull(), "CN")
  # There should be cities with similar name but different ids (e.g. Suzhou)
  expect_gt( l%>% distinct(id) %>% count(),
             l%>% distinct(city_name) %>% count()
  )
})



test_that("locations returns a source column when needed", {


  l <- locations(level="city", source = "csb", with_metadata=T)
  expect_equal(l %>% distinct(source) %>% pull,
               "csb")

  l <- locations(level="city", source = "csb", with_metadata=F)
  expect_equal(l %>% distinct(source) %>% pull,
               "csb")

})

test_that("id works on locations (stations)",{

  ids <- c("1001A","1002A")
  s <- rcrea::stations(source="mee",
                       id=ids)

  expect_equal(s$id %>% unique() %>% sort(),
               ids %>% tolower() %>% sort())
})


test_that("source_query works on locations (stations)", {

  source_city=list("eea"=c("paris","rome"), #several
                   "openaq"="paris",#one
                   "jp"=c(), #all
                   "cpcb"="") #none
  l <- locations(level="station", source_city = source_city, with_metadata=T)

  expect_true(identical(
    l %>% filter(source=="eea") %>% distinct(city_name) %>% arrange(city_name) %>% pull() %>% tolower(),
    source_city[["eea"]] %>% sort() %>% tolower()
  ))

  expect_true(identical(
    l%>% filter(source=="openaq") %>% distinct(city_name) %>% arrange(city_name) %>% pull() %>% tolower(),
    source_city[["openaq"]] %>% sort() %>% tolower()
  ))

  expect_gt(l%>% filter(source=="jp") %>% distinct(city_name) %>% count() %>% pull(), 47)
  expect_equal(l%>% filter(source=="cpcb") %>% distinct(city_name) %>% count() %>% pull(), 0)

  l <- locations(level="city", source="eea", with_metadata=T)
  expect_gt( l %>% filter(source=="eea") %>% distinct(name) %>% count() %>% pull(), 100)
  expect_equal( l%>% filter(source!="eea") %>% distinct(name) %>% count() %>% pull(), 0)

  l <- locations(level="city", source="eea", city="paris", with_metadata=T)
  expect_equal( l%>% filter(source=="eea") %>% distinct(city) %>% count() %>% pull(), 1)

  l <- locations(level="city", source=NULL, city="paris", with_metadata=T)
  expect_equal( l%>% filter(tolower(name)!="paris") %>% distinct(name) %>% count() %>% pull(), 0)

})
