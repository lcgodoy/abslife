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

##' Calculate Actuarial Present Value (APV) of a Loan
##'
##' Calculates the APV of a loan under competing risks (Default vs. Pre-payment)
##' and left-truncated survival data.
##'
##' @param cur_age Current age of the loan in months.
##' @param orig_term Original loan term in months.
##' @param orig_loan_amt Original loan amount.
##' @param mon_pmt Monthly payment amount.
##' @param ref_rate Annualized reference discount rate.
##' @param recov_curve Recovery curve data frame (columns: Month, Recovery).
##' @param prbs CDF and density probabilities from abslife (competing risks).
##' @param orig_apy Internal loan APY for amortization. Defaults to 0.15.
##'
##' @return A list containing APV, second moment (APV2), standard deviation
##'   (SD), survival probability to current age (S_cur_age), and conditional
##'   probability of survival to maturity (cond_p_mat).
##'
##' @export
calculate_apv <- function(cur_age, orig_term, orig_loan_amt, mon_pmt,
                          ref_rate, recov_curve, prbs, orig_apy = 0.15) {
  if (cur_age >= orig_term) {
    stop("Current age cannot be greater than or equal to the original term.")
  }
  if (!("event_type" %in% colnames(prbs))) {
    stop("prbs must contain competing risks (multiple event types) in 'event_type' column.")
  }
  evs <- unique(as.character(prbs$event_type))
  if (length(evs) != 2) {
    stop("The number of event types must be exactly 2.")
  }
  if ("Default" %in% evs) {
    default_name <- "Default"
    non_default_name <- setdiff(evs, "Default")
  } else if ("1" %in% evs) {
    default_name <- "1"
    non_default_name <- setdiff(evs, "1")
  } else {
    stop("Competing risk events must contain either 'Default' or '1' to identify the default event.")
  }
  N <- orig_term - cur_age
  rem_months <- cur_age:(orig_term - 1)
  orig_ir <- (1 + orig_apy)^(1/12) - 1
  prepay_balances <- amort_bal(1:orig_term,
                               orig_bal = orig_loan_amt,
                               int_rate = orig_ir,
                               payment = mon_pmt)
  prepay_cfs_rem <- prepay_balances[rem_months]
  recov_sub <- recov_curve[recov_curve$Month %in% rem_months, ]
  recov_sub <- recov_sub[order(recov_sub$Month), ]
  recov_cfs <- orig_loan_amt * recov_sub$Recovery
  cur_market_rate <- (1 + ref_rate)^(1/12) - 1
  disc_vec <- disc_fac(1:N, cur_market_rate)
  
  ## Sub-densities for remaining months
  prbs_def_rem <- prbs[prbs$event_type == default_name, ]
  prbs_def_rem <- prbs_def_rem[order(prbs_def_rem$lifetime), ]
  p_def <- prbs_def_rem$density
  
  prbs_pre_rem <- prbs[prbs$event_type == non_default_name, ]
  prbs_pre_rem <- prbs_pre_rem[order(prbs_pre_rem$lifetime), ]
  p_pre <- prbs_pre_rem$density
  
  cum_disc <- cumsum(disc_vec)
  def_pv <- mon_pmt * (cum_disc - disc_vec) + recov_cfs * disc_vec
  rep_pv <- mon_pmt * cum_disc + prepay_cfs_rem * disc_vec
  APV <- sum(def_pv * p_def) + sum(rep_pv * p_pre)
  APV2 <- sum(def_pv^2 * p_def) + sum(rep_pv^2 * p_pre)
  variance <- APV2 - APV^2
  return(list(
    APV = APV,
    APV2 = APV2,
    SD = sqrt(variance)
  ))
}

##' Solve for Risk-Adjusted Internal Rate of Return (IRR)
##'
##' @param cur_bal Current loan balance (to match EPV against).
##' @param cur_age Current age of the loan in months.
##' @param orig_term Original loan term in months.
##' @param orig_loan_amt Original loan amount.
##' @param mon_pmt Monthly payment amount.
##' @param recov_curve Recovery curve data frame.
##' @param prbs CDF and density probabilities from abslife.
##' @param orig_apy Internal loan APY for amortization. Defaults to 0.15.
##'
##' @return List with risk adjusted monthly rate, annualized rate, and objective
##'   value.
##' @export
solve_irr <- function(cur_bal, cur_age, orig_term, orig_loan_amt, mon_pmt,
                      recov_curve, prbs, orig_apy = 0.15) {
  irr_loss <- function(r) {
    ann_rate <- (1 + r)^12 - 1
    apv_res <- tryCatch({
      calculate_apv(cur_age, orig_term, orig_loan_amt,
                    mon_pmt, ann_rate, recov_curve, prbs,
                    orig_apy = orig_apy)
    }, error = function(e) {
      return(list(APV = Inf))
    })
    diff_val <- cur_bal - apv_res$APV
    return(diff_val * diff_val)
  }  
  opt <- stats::optimize(irr_loss, interval = c(-1, 1), tol = 1e-07)
  monthly_rate <- opt$minimum
  annual_rate <- (1 + monthly_rate)^12 - 1
  return(list(
    monthly_rate = monthly_rate,
    annualized_rate = annual_rate,
    objective = opt$objective
  ))
}
