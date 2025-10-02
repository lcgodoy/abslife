test_that("estimate_hazard output is an alife S3 object.", {
  data(aloans)
  x <-
    with(aloans, estimate_hazard(time_to_event = Z, trunc_time = Y))
  expect_true("alife" %in% class(x))
})
