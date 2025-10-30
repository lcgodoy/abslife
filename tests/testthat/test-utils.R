test_that("fix_0haz carries forward non-zero hazards", {
  haz_est <- data.frame(
    hazard = c(0.1, 0.2, 0, 0.3, 0),
    se_log_hazard = c(0.1, 0.1, 0.1, 0.1, 0.1)
  )

  result <- fix_0haz(haz_est)

  expected_hazard <- c(0.1, 0.2, 0.2, 0.3, 0.3)

  expect_equal(result$hazard, expected_hazard)
})

test_that("fix_0haz handles all zero hazards", {
  haz_est <- data.frame(
    hazard = c(0, 0, 0),
    se_log_hazard = c(0.1, 0.1, 0.1)
  )

  expect_error(fix_0haz(haz_est), "All hazard estimates are zero.")
})

test_that("fix_0haz works when first hazard is zero", {
  haz_est <- data.frame(
    hazard = c(0, 0.2, 0.3),
    se_log_hazard = c(0.1, 0.1, 0.1)
  )

  result <- fix_0haz(haz_est)

  expected_hazard <- c(0.2, 0.3)

  expect_equal(result$hazard, expected_hazard)
})

test_that("fix_0haz works with no zero hazards", {
  haz_est <- data.frame(
    hazard = c(0.1, 0.2, 0.3),
    se_log_hazard = c(0.1, 0.1, 0.1)
  )

  result <- fix_0haz(haz_est)

  expect_equal(result, haz_est)
})
