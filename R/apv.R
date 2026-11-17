##' Calculates a full loan amortization schedule
##' 
##' @description This function produces a complete amortization schedule
##' from a loan contract's terms.  It is used within [calculate_apv()] to
##' determine prepayment amounts, but it may be used as a standalone function.
##'
##' @param time A complete vector of payment periods (e.g., for a 60-month loan,
##'   input `c(1:60)`).
##' @param orig_bal The original loan balance.
##' @param int_rate The effective interest rate that compounds at the same
##'   frequency of the payment periods.
##' @param payment The payment due at each payment period (e.g., the monthly
##'   payment for a 60-month auto loan).
##' @return The outstanding balance at each month over the life of the loan
##'   assuming no payment uncertainty (i.e., the full amortization schedule).
##' 
##' @details
##' For a 12% annual percentage rate (APR) on a loan with monthly payments,
##' `int_rate` should be `(1.12)^(1/12) - 1`.
##' 
##' @export
amort_bal <- function(time, orig_bal, int_rate, payment) {
  aux_rate <- (1 + int_rate)^time
  calc1 <- orig_bal * aux_rate
  calc2 <- payment * (aux_rate - 1) / int_rate
  return(pmax(0, calc1 - calc2))
}

##' Calculate discount factors for an effective interest rate
##' 
##' @description This function produces a vector of discount factors from a
##' timeline and effective interest rate.  It is used within
##' [calculate_apv()] to discount future payments, but it may be used as a
##' standalone function.
##'
##' @param time A complete vector of payment periods (e.g., for a 60-month loan,
##'   input `c(1:60)`).
##' @param int_rate The effective interest rate that compounds at the same
##'   frequency of the payment periods.
##' @return A vector of discount factors corresponding to `time`.
##' 
##' @details
##' For a 12% annual percentage rate (APR) on a loan with monthly payments,
##' `int_rate` should be `(1.12)^(1/12) - 1`.
##' 
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
##'   remaining month. `.apv_moments()` returns a list with the APV (`APV`)
##'   and its standard deviation (`SD`).
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
    ## APV2 = APV2,
    SD = sqrt(variance)
  ))
}

## evaluates 'expr', a call to an internal function, and re-signals the errors
## and warnings raised by that function with 'call', so that the messages name
## the exported function; other conditions (e.g., from evaluating the
## arguments of 'expr') keep their own call
.with_call <- function(expr, call) {
  fun <- substitute(expr)[[1]]
  raised_by_fun <- function(cond) {
    cl <- conditionCall(cond)
    is.call(cl) && identical(cl[[1]], fun)
  }
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      if (raised_by_fun(e)) e <- simpleError(conditionMessage(e), call)
      stop(e)
    }),
    ## a calling handler (unlike tryCatch) lets 'expr' run to the end
    warning = function(w) {
      if (raised_by_fun(w)) {
        warning(simpleWarning(conditionMessage(w), call))
        invokeRestart("muffleWarning")
      }
    }
  )
}

##' Calculates the actuarial present value (APV) of a loan
##'
##' @description This function calculates the APV of a loan under competing
##' risks (default vs. prepayment).  It combines an estimated lifetime
##' distribution (payment uncertainty) with discounting.  It is designed to
##' be used with an [estimate_hazard()] object.  The method follows the
##' design in Lautier et al. (2026).
##'
##' @details `x` must contain exactly two event types, and the default event
##'   must be labeled either `Default` or `1`; the other event type will be
##'   treated as a prepayment. If the observed lifetime support does not reach
##'   `orig_term`, the hazard rates are extrapolated up to `orig_term` with
##'   [extend_hazard()].  If this occurs, a `warning` message will be thrown.
##'   Hazard rate estimates must be available for every lifetime from
##'   `cur_age + 1` to `orig_term`; otherwise, an `error` will be thrown.
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
##' @param orig_apy Internal loan annual percentage yield (APY) for
##' amortization. Defaults to 0.15.
##' @param type A character string specifying the extrapolation type passed to
##'   [extend_hazard()]: `constant` (default), `geometric` or `linear`.
##' 
##' @references  
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##'
##' @return An object of class `apv`: a one-row `data.frame` with the APV
##'   (`APV`), and its standard deviation (`SD`).  Note that [rbind()] can stack
##'   several `apv` objects into a `data.frame`, and the named arguments become
##'   its row names.
##'
##' @export
calculate_apv <- function(x,
                          cur_age,
                          orig_term, orig_loan_amt,
                          mon_pmt, ref_rate, recov_curve,
                          orig_apy = 0.15,
                          type = "constant") {
  setup <- .with_call(.apv_setup(x, cur_age, orig_term, orig_loan_amt,
                                 mon_pmt, recov_curve, orig_apy, type),
                      sys.call())
  cur_market_rate <- (1 + ref_rate)^(1/12) - 1
  out <- .apv_moments(setup, cur_market_rate)
  return(new_apv(as.data.frame(out)))
}

##' Solves for the risk-adjusted internal rate of return (IRR)
##'
##' @description Finds the monthly discount rate such that the actuarial
##' present value (APV) of the loan (see [calculate_apv()]) matches `abs0_bal`.
##' For theoretical details, see Lautier et al. (2026).  Because this
##' calculation must solve a polynomial of high-degree, the rate is found by
##' using [stats::uniroot()] and searching over monthly effective rates between
##' -0.9999 and 1.  If no solution can be found, an `error` will be thrown. If
##' [stats::uniroot()] does not reach a tolerance of `1e-10` within 1,000
##' iterations, [stats::uniroot()] will produce a warning and the `converged`
##' column of the resulting `irr` object will be `FALSE`.
##'
##' @inherit calculate_apv details
##' @inheritParams calculate_apv
##' @param abs0_bal Current loan balance, interpreted as the price of the
##' loan (i.e., we find the risk-adjusted rate such that the current loan
##' balance matches the calculated APV).
##' 
##' @references  
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##'
##' @return An object of class `irr`: a one-row `data.frame` with the
##'   risk-adjusted monthly rate (`monthly_rate`), the annualized rate
##'   (`annualized_rate`), the squared difference between `abs0_bal` and the
##'   APV at the solution (`objective`), and a flag indicating if the root
##'   finder converged (`converged`). Note that [rbind()] stacks several `irr`
##'   objects into a `data.frame`, and its named arguments become its row names.
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
  setup <- .with_call(.apv_setup(x, cur_age, orig_term, orig_loan_amt,
                                 mon_pmt, recov_curve, orig_apy, type),
                      sys.call())
  ## the APV decreases with the rate, so there is at most one root
  apv_gap <- function(r) .apv_moments(setup, r)$APV - abs0_bal
  bounds <- c(-0.9999, 1)
  gap_bounds <- vapply(bounds, apv_gap, numeric(1))
  if (gap_bounds[1] < 0 || gap_bounds[2] > 0) {
    stop(sprintf("No monthly rate in [%g, %g] matches 'abs0_bal' (%g): the APV ranges from %g to %g over this interval.",
                 bounds[1], bounds[2], abs0_bal,
                 gap_bounds[2] + abs0_bal,
                 gap_bounds[1] + abs0_bal))
  }
  maxiter <- 1000
  opt <- stats::uniroot(apv_gap,
                        interval = bounds,
                        f.lower = gap_bounds[1],
                        f.upper = gap_bounds[2],
                        tol = 1e-10,
                        maxiter = maxiter)
  monthly_rate <- opt$root
  annual_rate <- (1 + monthly_rate)^12 - 1
  ## uniroot() sets 'iter' to 'maxiter' when it does not converge
  return(new_irr(data.frame(monthly_rate = monthly_rate,
                            annualized_rate = annual_rate,
                            objective = opt$f.root^2,
                            converged = opt$iter < maxiter)))
}
