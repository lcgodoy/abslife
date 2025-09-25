test_that("estimate_hazard output is an alife S3 object.", {
  data(aloans)
  x <-
    with(aloans, estimate_hazard(time = Z, trunc_time = Y))
  expect_true("alife" %in% class(x))
})
