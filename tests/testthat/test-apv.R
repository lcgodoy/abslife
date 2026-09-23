data(aloans)
hazards <-
  suppressWarnings(
    with(subset(aloans, risk.cat == "deep_subprime"),
         estimate_hazard(lifetime = Z, trunc_time = Y,
                         event_type = ifelse(D == 1, "Default",
                                             "Prepayment"),
                         censoring_indicator = C,
                         carry_hazard = TRUE))
  )
recov_curve <- data.frame(month = 0:72, recovery = 0.3)
loan <- list(x = hazards, cur_age = 7, orig_term = 72,
             orig_loan_amt = 50000, mon_pmt = 1031.84,
             recov_curve = recov_curve)

test_that("solve_irr finds the rate that matches abs0_bal", {
  irr <- suppressWarnings(do.call(solve_irr, c(loan, abs0_bal = 37719.47)))
  apv <- suppressWarnings(
    do.call(calculate_apv, c(loan, ref_rate = irr$annualized_rate))
  )
  expect_equal(apv$APV, 37719.47, tolerance = 1e-8)
  expect_lt(irr$objective, 1e-6)
})

test_that("solve_irr propagates input errors", {
  bad <- loan
  bad$cur_age <- 72
  expect_error(suppressWarnings(do.call(solve_irr,
                                        c(bad, abs0_bal = 37719.47))),
               "Current age")
})

test_that("solve_irr errors when no rate matches abs0_bal", {
  expect_error(suppressWarnings(do.call(solve_irr, c(loan, abs0_bal = 100))),
               "No monthly rate")
})

test_that("APV functions error on missing hazards or recovery months", {
  ## hazards for this pool start at lifetime 3
  young <- loan
  young$cur_age <- 0
  expect_error(suppressWarnings(do.call(calculate_apv,
                                        c(young, ref_rate = 0.1))),
               "missing for lifetime\\(s\\) 1, 2;")
  short <- loan
  short$recov_curve <- recov_curve[recov_curve$month != 30, ]
  expect_error(suppressWarnings(do.call(calculate_apv,
                                        c(short, ref_rate = 0.1))),
               "missing month\\(s\\): 30")
})
