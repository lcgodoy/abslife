test_that("calc_tp calculates time points correctly", {
  time_to_event <- 1:5
  trunc_time <- 0:4
  expect_equal(calc_tp(time_to_event, trunc_time), 1:5)
})

test_that("calc_tp handles NA values", {
  time_to_event <- c(1:5, NA)
  trunc_time <- c(0:4, NA)
  expect_equal(calc_tp(time_to_event, trunc_time), 1:5)
})

test_that("calc_tp works when trunc_time has the minimum value", {
  time_to_event <- 2:6
  trunc_time <- 1:5
  expect_equal(calc_tp(time_to_event, trunc_time), 2:6)
})

test_that("calc_tp works with single values", {
  time_to_event <- 10
  trunc_time <- 5
  expect_equal(calc_tp(time_to_event, trunc_time), 6:10)
})

test_that("f_hat calculates correctly", {
  time_to_event <- c(1, 2, 3, 4, 5)
  event <- c(1, 1, 1, 1, 1)
  censoring <- c(0, 0, 0, 0, 0)
  expect_equal(f_hat(3, time_to_event, event, censoring), 0.2)
  expect_equal(f_hat(6, time_to_event, event, censoring), 0)
})

test_that("f_hat handles censoring", {
  time_to_event <- c(1, 2, 3, 4, 5)
  event <- c(1, 1, 1, 1, 1)
  censoring <- c(0, 0, 1, 0, 0)
  expect_equal(f_hat(3, time_to_event, event, censoring), 0)
})

test_that("u_hat calculates correctly", {
  time_to_event <- c(5, 5, 5, 5, 5)
  trunc_time <- c(1, 1, 1, 1, 1)
  expect_equal(u_hat(3, time_to_event, trunc_time), 1)
  expect_equal(u_hat(6, time_to_event, trunc_time), 0)
})

test_that("u_hat handles edge cases", {
  time_to_event <- c(5, 5, 5, 5, 5)
  trunc_time <- c(1, 1, 1, 1, 1)
  expect_equal(u_hat(1, time_to_event, trunc_time), 1)
  expect_equal(u_hat(5, time_to_event, trunc_time), 1)
})

test_that("single_t_hazard calculates correctly", {
  time_to_event <- c(1, 2, 3, 4, 5)
  trunc_time <- c(0, 0, 0, 0, 0)
  event <- c(1, 1, 1, 1, 1)
  censoring <- c(0, 0, 0, 0, 0)

  result <- single_t_hazard(3, trunc_time, time_to_event, event, censoring)

  expect_equal(result["time_to_event"], c(time_to_event = 3))
  expect_equal(result["fh"], c(fh = 0.2))
  expect_equal(result["uh"], c(uh = 1.0))
  expect_equal(result["hazard"], c(hazard = 0.2))
  expect_true(!is.na(result["se_log_hazard"]))
})

test_that("single_t_hazard handles zero hazard", {
  time_to_event <- c(1, 2, 4, 5)
  trunc_time <- c(0, 0, 0, 0)
  event <- c(1, 1, 1, 1)
  censoring <- c(0, 0, 0, 0)

  result <- single_t_hazard(3, trunc_time, time_to_event, event, censoring)

  expect_equal(result["hazard"], c(hazard = 0))
  expect_equal(result["se_log_hazard"], c(se_log_hazard = 0))
})

test_that("single_t_hazard handles uh = 0", {
  time_to_event <- c(1, 2, 3, 4, 5)
  trunc_time <- c(4, 4, 4, 4, 4)
  event <- c(1, 1, 1, 1, 1)
  censoring <- c(0, 0, 0, 0, 0)

  result <- single_t_hazard(3, trunc_time, time_to_event, event, censoring)

  expect_equal(result["hazard"], c(hazard = 0))
  expect_true(is.na(result["se_log_hazard"]))
})
