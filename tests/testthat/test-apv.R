library(testthat)
library(abslife)

test_that("amort_bal and disc_fac helper functions work", {
  # Simple tests for amortization and discount factors
  t <- 1:3
  orig_bal <- 1000
  int_rate <- 0.01
  payment <- 300
  
  bal <- amort_bal(t, orig_bal, int_rate, payment)
  expect_type(bal, "double")
  expect_equal(length(bal), 3)
  expect_true(all(bal >= 0))
  
  df <- disc_fac(t, int_rate)
  expect_equal(length(df), 3)
  expect_true(all(df < 1))
})

test_that("calculate_apv and solve_irr work with dummy competing risk data (Default / Pre-payment)", {
  # Setup test parameters
  cur_age <- 1
  orig_term <- 3
  orig_loan_amt <- 1000
  mon_pmt <- 500
  ref_rate <- 0.05
  orig_apy <- 0.12
  
  # Create a simple prbs data frame with 2 events: Default and Pre-payment
  prbs <- data.frame(
    event_type = rep(c("Default", "Pre-payment"), each = 3),
    lifetime = rep(1:3, 2),
    cdf = c(0.1, 0.2, 0.3,  0.05, 0.15, 0.25),
    density = c(0.1, 0.1, 0.1,  0.05, 0.10, 0.10)
  )
  
  # Define a simple recovery curve
  recov_curve <- data.frame(
    Month = 2:3,
    Recovery = c(0.4, 0.3)
  )
  
  # 1. Manual calculation for comparison
  N <- orig_term - cur_age # 2
  rem_months <- 2:3
  
  orig_ir <- (1 + orig_apy)^(1/12) - 1
  prepay_balances <- amort_bal(1:3, orig_bal = 1000, int_rate = orig_ir, payment = 500)
  prepay_cfs_rem <- prepay_balances[rem_months]
  
  recov_cfs <- orig_loan_amt * c(0.4, 0.3)
  
  cur_market_rate <- (1 + ref_rate)^(1/12) - 1
  disc_vec <- disc_fac(1:N, cur_market_rate)
  
  # S_cur_age = 1 - sum(cdf at month 1) = 1 - (0.1 + 0.05) = 0.85
  S_cur_age <- 0.85
  
  p_def <- c(0.1, 0.1) / S_cur_age
  p_pre <- c(0.10, 0.10) / S_cur_age
  
  S_maturity <- 1 - (0.3 + 0.25) # 0.45
  cond_p_mat <- S_maturity / S_cur_age # 0.45 / 0.85
  
  cum_disc <- cumsum(disc_vec)
  
  # Month 2 (rem_months[1])
  def_pv1 <- mon_pmt * 0 + recov_cfs[1] * disc_vec[1]
  rep_pv1 <- mon_pmt * cum_disc[1] + prepay_cfs_rem[1] * disc_vec[1]
  
  # Month 3 (rem_months[2])
  def_pv2 <- mon_pmt * cum_disc[1] + recov_cfs[2] * disc_vec[2]
  rep_pv2 <- mon_pmt * cum_disc[2] + prepay_cfs_rem[2] * disc_vec[2]
  
  mat_pv <- mon_pmt * cum_disc[2]
  
  expected_apv <- sum(c(def_pv1, def_pv2) * p_def) + sum(c(rep_pv1, rep_pv2) * p_pre) + mat_pv * cond_p_mat
  
  # 2. Run calculate_apv
  res <- calculate_apv(
    cur_age = cur_age,
    orig_term = orig_term,
    orig_loan_amt = orig_loan_amt,
    mon_pmt = mon_pmt,
    ref_rate = ref_rate,
    recov_curve = recov_curve,
    prbs = prbs,
    orig_apy = orig_apy
  )
  
  expect_equal(res$APV, expected_apv, tolerance = 1e-6)
  expect_equal(res$S_cur_age, S_cur_age)
  expect_equal(res$cond_p_mat, cond_p_mat)
  expect_true(res$SD >= 0)
  
  # 3. Test solve_irr
  # We should be able to recover a market rate using the calculated APV as the current balance
  irr_res <- solve_irr(
    cur_bal = res$APV,
    cur_age = cur_age,
    orig_term = orig_term,
    orig_loan_amt = orig_loan_amt,
    mon_pmt = mon_pmt,
    recov_curve = recov_curve,
    prbs = prbs,
    orig_apy = orig_apy
  )
  
  expect_equal(irr_res$annualized_rate, ref_rate, tolerance = 1e-4)
})

test_that("calculate_apv handles non-Default event names (assuming '1' is Default)", {
  prbs_numeric <- data.frame(
    event_type = rep(c("1", "2"), each = 3),
    lifetime = rep(1:3, 2),
    cdf = c(0.1, 0.2, 0.3,  0.05, 0.15, 0.25),
    density = c(0.1, 0.1, 0.1,  0.05, 0.10, 0.10)
  )
  
  recov_curve <- data.frame(
    Month = 2:3,
    Recovery = c(0.4, 0.3)
  )
  
  # Should run without error
  res_numeric <- calculate_apv(
    cur_age = 1,
    orig_term = 3,
    orig_loan_amt = 1000,
    mon_pmt = 500,
    ref_rate = 0.05,
    recov_curve = recov_curve,
    prbs = prbs_numeric,
    orig_apy = 0.12
  )
  
  expect_type(res_numeric, "list")
  expect_true(res_numeric$APV > 0)
})

test_that("calculate_apv throws correct errors for invalid inputs", {
  recov_curve <- data.frame(
    Month = 2:3,
    Recovery = c(0.4, 0.3)
  )
  
  # 1. cur_age >= orig_term
  prbs <- data.frame(
    event_type = rep(c("Default", "Pre-payment"), each = 3),
    lifetime = rep(1:3, 2),
    cdf = c(0.1, 0.2, 0.3,  0.05, 0.15, 0.25),
    density = c(0.1, 0.1, 0.1,  0.05, 0.10, 0.10)
  )
  expect_error(
    calculate_apv(3, 3, 1000, 500, 0.05, recov_curve, prbs),
    "Current age cannot be greater than or equal to the original term"
  )
  
  # 2. Not competing risks (no event_type column)
  prbs_no_type <- prbs
  prbs_no_type$event_type <- NULL
  expect_error(
    calculate_apv(1, 3, 1000, 500, 0.05, recov_curve, prbs_no_type),
    "prbs must contain competing risks"
  )
  
  # 3. Not exactly 2 event types
  prbs_3_types <- data.frame(
    event_type = rep(c("Default", "Pre-payment", "Other"), each = 3),
    lifetime = rep(1:3, 3),
    cdf = rep(c(0.1, 0.2, 0.3), 3),
    density = rep(c(0.1, 0.1, 0.1), 3)
  )
  expect_error(
    calculate_apv(1, 3, 1000, 500, 0.05, recov_curve, prbs_3_types),
    "number of event types must be exactly 2"
  )
  
  # 4. Neither 'Default' nor '1' is present
  prbs_bad_names <- data.frame(
    event_type = rep(c("A", "B"), each = 3),
    lifetime = rep(1:3, 2),
    cdf = c(0.1, 0.2, 0.3,  0.05, 0.15, 0.25),
    density = c(0.1, 0.1, 0.1,  0.05, 0.10, 0.10)
  )
  expect_error(
    calculate_apv(1, 3, 1000, 500, 0.05, recov_curve, prbs_bad_names),
    "must contain either 'Default' or '1'"
  )
})
