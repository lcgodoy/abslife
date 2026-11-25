test_that("ralife gives the same draws for alife and acdf objects.", {
  data(aloans)
  h <- with(aloans, estimate_hazard(lifetime = Z, trunc_time = Y))
  set.seed(1)
  from_alife <- ralife(50, h)
  set.seed(1)
  from_acdf <- ralife(50, calc_cdf(h))
  expect_identical(from_alife, from_acdf)
  expect_true(is.numeric(from_alife))
  expect_length(from_alife, 50)
  expect_true(all(from_alife %in% h$lifetime))
})

test_that("ralife gives the same draws for alife_multi, acif, and acdf_multi objects.", {
  data(aloans)
  aloans <- transform(aloans,
                      event_type = ifelse(D == 1, "Default", "Prepayment"))
  expect_warning(
    h_cr <- with(aloans, estimate_hazard(lifetime = Z,
                                         trunc_time = Y,
                                         censoring_indicator = C,
                                         event_type = event_type)),
    "censored observations"
  )
  set.seed(1)
  from_alife_multi <- ralife(50, h_cr)
  set.seed(1)
  from_acif <- ralife(50, calc_cif(h_cr))
  set.seed(1)
  from_acdf_multi <- suppressMessages(ralife(50, calc_cdf(h_cr)))
  expect_identical(from_alife_multi, from_acif)
  expect_identical(from_alife_multi, from_acdf_multi)
  expect_s3_class(from_alife_multi, "data.frame")
  expect_identical(nrow(from_alife_multi), 50L)
  expect_named(from_alife_multi, c("event_type", "lifetime"))
})

test_that("ralife gives an error for unsupported objects.", {
  expect_error(ralife(10, data.frame(a = 1)), "does not support")
})
