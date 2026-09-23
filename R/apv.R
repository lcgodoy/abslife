##' Calculate Loan Amortization Schedule
##'
##' @param time Vector of months
##' @param orig_bal Original loan balance
##' @param int_rate Monthly interest rate
##' @param payment Monthly payment
##' @return Outstanding balance at each month
##' @export
amort_bal <- function(time, orig_bal, int_rate, payment) {
  aux_rate <- (1 + int_rate)^time
  calc1 <- orig_bal * aux_rate
  calc2 <- payment * (aux_rate - 1) / int_rate
  return(pmax(0, calc1 - calc2))
}

##' Calculate Discount Factors
##'
##' @param time Vector of months
##' @param int_rate Monthly discount rate
##' @return Discount factor for each month
##' @export
disc_fac <- function(time, int_rate) {
  return(1 / (1 + int_rate)^time)
}

##' Set up and evaluate the APV of a loan (internal use)
##'
##' @description `.apv_setup()` checks the inputs of [calculate_apv()] and
##'   [solve_irr()] and computes everything that does not depend on the
##'   discount rate (event probabilities and cash flows). `.apv_moments()`
##'   computes the APV, its second moment, and its standard deviation for a
##'   given monthly discount rate.
##'
##' @inheritParams calculate_apv
##' @param setup The output of `.apv_setup()`.
##' @param rate A monthly discount rate.
##'
##' @return `.apv_setup()` returns a list with the number of remaining months,
##'   the default and prepayment probabilities, and the cash flows for each
##'   remaining month. `.apv_moments()` returns the same list as
##'   [calculate_apv()].
##' @name apv_helpers
##' @keywords internal
.apv_setup <- function(x, cur_age, orig_term, orig_loan_amt, mon_pmt,
                       recov_curve, orig_apy, type) {
  stopifnot(inherits(x, "alife_multi"))
  if (cur_age >= orig_term) {
    stop("Current age cannot be greater than or equal to the original term.")
  }
  evs <- unique(as.character(x$event_type))
  if (length(evs) != 2) {
    stop("The number of event types must be exactly 2.")
  }
  if ("Default" %in% evs) {
    default_name <- "Default"
  } else if ("1" %in% evs) {
    default_name <- "1"
  } else {
    stop("Competing risk events must contain either 'Default' or '1' to identify the default event.")
  }
  non_default_name <- setdiff(evs, default_name)
  rec_months <- cur_age:(orig_term - 1)
  rem_months <- rec_months + 1
  recov_id <- match(rec_months, recov_curve$month)
  if (anyNA(recov_id)) {
    stop(sprintf("Recovery curve is missing month(s): %s.",
                 paste(rec_months[is.na(recov_id)], collapse = ", ")))
  }
  if (max(x$lifetime) < orig_term) {
    warning("Lifetime observed support does not include the original loan term. Extrapolating hazards.")
    x <- extend_hazard(x,
                       end = orig_term,
                       end_event = non_default_name,
                       type = type)
  }
  ## every remaining month needs a hazard estimate for both events;
  ## otherwise, probabilities and cash flows would be misaligned
  miss <- lapply(evs, function(et) {
    setdiff(rem_months, x$lifetime[x$event_type == et])
  })
  miss <- sort(unique(unlist(miss)))
  if (length(miss) > 0) {
    stop(sprintf("Hazard estimates are missing for lifetime(s) %s; they are needed for every lifetime from 'cur_age + 1' to 'orig_term'.",
                 paste(miss, collapse = ", ")))
  }
  ## Sub-densities for remaining months
  prbs <- calc_cif(x[x$lifetime %in% rem_months, ])
  get_prbs <- function(et) {
    prbs_rem <- prbs[prbs$event_type == et, ]
    prbs_rem$pr_zx[match(rem_months, prbs_rem$lifetime)]
  }
  ## p_denom <- sum(c(p_pre, p_def))
  ## p_def <- p_def / p_denom
  ## p_pre <- p_pre / p_denom
  orig_ir <- (1 + orig_apy)^(1/12) - 1
  list(n_months = orig_term - cur_age,
       mon_pmt = mon_pmt,
       p_def = get_prbs(default_name),
       p_pre = get_prbs(non_default_name),
       prepay_cfs = amort_bal(rec_months,
                              orig_bal = orig_loan_amt,
                              int_rate = orig_ir,
                              payment = mon_pmt),
       recov_cfs = orig_loan_amt * recov_curve$recovery[recov_id])
}

##' @rdname apv_helpers
.apv_moments <- function(setup, rate) {
  disc_vec <- disc_fac(seq_len(setup$n_months), rate)
  cum_disc <- cumsum(disc_vec)
  def_pv <- setup$mon_pmt * (cum_disc - disc_vec) +
    setup$recov_cfs * disc_vec
  rep_pv <- setup$mon_pmt * cum_disc + setup$prepay_cfs * disc_vec
  APV <- sum(def_pv * setup$p_def) + sum(rep_pv * setup$p_pre)
  APV2 <- sum(def_pv^2 * setup$p_def) + sum(rep_pv^2 * setup$p_pre)
  variance <- APV2 - APV^2
  return(list(
    APV = APV,
    APV2 = APV2,
    SD = sqrt(variance)
  ))
}

##' Calculate Actuarial Present Value (APV) of a Loan
##'
##' Calculates the APV of a loan under competing risks (default vs. prepayment)
##' and left-truncated survival data.
##'
##' @details `x` must contain exactly two event types, and the default event
##'   must be labeled either `"Default"` or `"1"`; the other event type is
##'   treated as prepayment. If the observed lifetime support does not reach
##'   `orig_term`, the hazards are extrapolated up to `orig_term` with
##'   [extend_hazard()] (with a warning). After that, hazard estimates must be
##'   available for every lifetime from `cur_age + 1` to `orig_term`;
##'   otherwise, an error is thrown.
##'
##' @param x An object of class `alife_multi`. Typically the output of the
##'   `estimate_hazard` function with two event types.
##' @param cur_age Current age of the loan in months.
##' @param orig_term Original loan term in months.
##' @param orig_loan_amt Original loan amount.
##' @param mon_pmt Monthly payment amount.
##' @param ref_rate Annualized reference discount rate.
##' @param recov_curve Recovery curve `data.frame` with columns `month` and
##'   `recovery` (the recovery rate as a proportion of `orig_loan_amt`). It must
##'   contain every month from `cur_age` to `orig_term - 1`.
##' @param orig_apy Internal loan APY for amortization. Defaults to 0.15.
##' @param type A character string specifying the extrapolation type passed to
##'   [extend_hazard()]: either `"constant"` (default), `"geometric"` or
##'   `"linear"`.
##'
##' @return A list containing the APV (`APV`), its second moment (`APV2`), and
##'   its standard deviation (`SD`).
##'
##' @export
calculate_apv <- function(x,
                          cur_age,
                          orig_term, orig_loan_amt,
                          mon_pmt, ref_rate, recov_curve,
                          orig_apy = 0.15,
                          type = "constant") {
  setup <- .apv_setup(x, cur_age, orig_term, orig_loan_amt, mon_pmt,
                      recov_curve, orig_apy, type)
  cur_market_rate <- (1 + ref_rate)^(1/12) - 1
  return(.apv_moments(setup, cur_market_rate))
}

##' Solve for Risk-Adjusted Internal Rate of Return (IRR)
##'
##' Finds the monthly discount rate at which the APV of the loan (see
##' [calculate_apv()]) matches `abs0_bal`. The rate is found with
##' [stats::uniroot()] over monthly rates between -0.5 and 1; an error is
##' thrown if no rate in this range matches `abs0_bal`.
##'
##' @inherit calculate_apv details
##' @inheritParams calculate_apv
##' @param abs0_bal Current loan balance (to match the APV against).
##'
##' @return A list with the risk-adjusted monthly rate (`monthly_rate`), the
##'   annualized rate (`annualized_rate`), and the squared difference between
##'   `abs0_bal` and the APV at the solution (`objective`).
##' @export
solve_irr <- function(x,
                      abs0_bal,
                      cur_age,
                      orig_term,
                      orig_loan_amt, mon_pmt,
                      recov_curve,
                      orig_apy = 0.15,
                      type = "constant") {
  ## the inputs are checked (and the probabilities computed) only once
  setup <- .apv_setup(x, cur_age, orig_term, orig_loan_amt, mon_pmt,
                      recov_curve, orig_apy, type)
  ## the APV decreases with the rate, so there is at most one root
  apv_gap <- function(r) .apv_moments(setup, r)$APV - abs0_bal
  bounds <- c(-0.5, 1)
  gap_bounds <- vapply(bounds, apv_gap, numeric(1))
  if (gap_bounds[1] < 0 || gap_bounds[2] > 0) {
    stop(sprintf("No monthly rate in [%g, %g] matches 'abs0_bal' (%g): the APV ranges from %g to %g over this interval.",
                 bounds[1], bounds[2], abs0_bal,
                 gap_bounds[2] + abs0_bal, gap_bounds[1] + abs0_bal))
  }
  opt <- stats::uniroot(apv_gap, interval = bounds,
                        f.lower = gap_bounds[1],
                        f.upper = gap_bounds[2],
                        tol = 1e-10)
  monthly_rate <- opt$root
  annual_rate <- (1 + monthly_rate)^12 - 1
  return(list(monthly_rate = monthly_rate,
              annualized_rate = annual_rate,
              objective = opt$f.root^2))
}
