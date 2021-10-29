library(testthat)


test_that("reconnection works", {

  processes <- rcrea::processes()

  lapply(processes$id, function(process_id){
    p <- processes[processes$id==process_id,]

    retrieved_process_id <- rcrea::retrieve_or_create_process(
      filter = p$filter,
      agg_spatial = p$agg_spatial,
      agg_temp = p$agg_temp,
      deweather = p$deweather,
      preferred_name = process_id,
      processes=processes
    )

    expect_equal(retrieved_process_id, process_id)
  })
})
