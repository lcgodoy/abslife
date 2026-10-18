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
  expect_error(suppressWarnings(do.call(solve_irr, c(loan, abs0_bal = 10))),
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

test_that("calculate_apv and solve_irr return apv and irr objects", {
  apv <- suppressWarnings(do.call(calculate_apv, c(loan, ref_rate = 0.05)))
  irr <- suppressWarnings(do.call(solve_irr, c(loan, abs0_bal = 37719.47)))
  expect_s3_class(apv, c("apv", "data.frame"), exact = TRUE)
  expect_s3_class(irr, c("irr", "data.frame"), exact = TRUE)
  expect_equal(nrow(apv), 1)
  expect_named(irr, c("monthly_rate", "annualized_rate", "objective",
                      "converged"))
  expect_true(irr$converged)
})

test_that("rbind stacks apv and irr objects", {
  apv1 <- suppressWarnings(do.call(calculate_apv, c(loan, ref_rate = 0.05)))
  apv2 <- suppressWarnings(do.call(calculate_apv, c(loan, ref_rate = 0.10)))
  out <- rbind(low = apv1, high = apv2)
  expect_s3_class(out, "data.frame", exact = TRUE)
  expect_equal(rownames(out), c("low", "high"))
  expect_equal(out$APV, c(apv1$APV, apv2$APV))
  irr <- suppressWarnings(do.call(solve_irr, c(loan, abs0_bal = 37719.47)))
  expect_s3_class(rbind(irr, irr), "data.frame", exact = TRUE)
})

test_that("print methods show the results and flag non-convergence", {
  apv <- suppressWarnings(do.call(calculate_apv, c(loan, ref_rate = 0.05)))
  expect_output(print(apv), "Actuarial present value")
  irr <- suppressWarnings(do.call(solve_irr, c(loan, abs0_bal = 37719.47)))
  expect_output(print(irr), "internal rate of return")
  expect_no_match(capture.output(print(irr)), "did not converge")
  bad <- irr
  bad$converged <- FALSE
  expect_output(print(bad), "did not converge")
})

test_that("setup errors and warnings name the exported function", {
  err <- tryCatch(
    with(loan, solve_irr(x, abs0_bal = 37719.47, cur_age = 72,
                         orig_term = orig_term, orig_loan_amt = orig_loan_amt,
                         mon_pmt = mon_pmt, recov_curve = recov_curve)),
    error = identity)
  expect_match(conditionMessage(err), "Current age")
  expect_identical(conditionCall(err)[[1]], quote(solve_irr))
  wrn <- tryCatch(
    with(loan, calculate_apv(x, cur_age = cur_age, orig_term = orig_term,
                             orig_loan_amt = orig_loan_amt, mon_pmt = mon_pmt,
                             ref_rate = 0.05, recov_curve = recov_curve)),
    warning = identity)
  expect_match(conditionMessage(wrn), "Extrapolating hazards")
  expect_identical(conditionCall(wrn)[[1]], quote(calculate_apv))
  ## conditions raised while evaluating the arguments keep their own call
  make_x <- function() {
    warning("from make_x")
    loan$x
  }
  wrn <- tryCatch(
    with(loan, calculate_apv(make_x(), cur_age = cur_age,
                             orig_term = orig_term,
                             orig_loan_amt = orig_loan_amt, mon_pmt = mon_pmt,
                             ref_rate = 0.05, recov_curve = recov_curve)),
    warning = identity)
  expect_identical(conditionCall(wrn), quote(make_x()))
})
