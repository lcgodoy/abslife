##' Computes the theoretical support of a lifetime variable
##'
##' This helper function generates a sequence of evaluation points (Lautier et
##' al. 2025, <DOI:10.1214/25-AOAS2103>).
##'
##' @param Delta a scalar denoting the marketing period (in months) during which
##'   the ABS is marketed to prospective investors after the trust closes to new
##'   loan originations.
##' @param m a scalar representing the total number of months over which the
##'   underlying consumer auto loans are originated. This defines the length of
##'   the loan origination window before the trust closes.
##' @param omega a scalar denoting the known, finite upper bound of a loan's
##'   lifetime (in months), which is fixed by its amortization schedule at
##'   contract signing (for example, \eqn{\omega = 72} for a 72-month loan).
##' @param epsilon a scalar (default is \code{NULL}) denoting denotes the
##'   present time (the current calendar month of observation) of the data
##'   generation process for an active ABS pool. If larger than \code{omega},
##'   right-censoring is present because it indicates some active loans are
##'   still making ongoing payments and have not yet terminated.
##'
##' @note The observed support may not extend all the way to \eqn{omega}
##'   (\code{omega}). In that scenario, one may want to set \eqn{omega} to the
##'   age of the oldest contract in the bond (corresponding to \eqn{\min(\omega,
##'   \varepsilon - 1)} in the notation used in Lautier et al. 2025), with
##'   \eqn{\varepsilon} denoting the present time.
##' 
##' @return A numeric vector representing the theoretical or observed support
##'   associated with the inputs.
##' @export
retrieve_support <- function(Delta, m, omega, epsilon = NULL) {
  stopifnot(length(Delta) == 1)
  stopifnot(length(m) == 1)
  stopifnot(length(omega) == 1)
  if (is.null(epsilon)) {
    epsilon <- omega + 1
  } else {
    stopifnot(length(epsilon) == 1)
  }
  xi <- min(epsilon - 1, omega)
  support_x <- seq.int(from = Delta + 1, to = xi)
  support_y <- seq.int(from = Delta + 1, to = Delta + m)
  return(list("X" = support_x, "Y" = support_y))
}

##' Calculate Observed Support
##'
##' This helper function generates the observed support of the lifetime variable
##' based on the study's overall time range (Lautier et al. 2023, <DOI:
##' 10.1016/j.ecosta.2023.05.005>). In particular, it calculates \eqn{\Delta}
##' and \eqn{m} based on left-truncation and time-to-event variables and outputs
##' a sequence ranging from \eqn{\Delta + 1} to \eqn{\xi}, where \eqn{\xi =
##' \min(\omega, \varepsilon - 1)}, with \eqn{\varepsilon} denoting the present
##' time.
##'
##' @param lifetime The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times.
##'
##' @return A numeric vector representing the observed support of the lifetime
##'   variable.
##' @export
calc_osup <- function(lifetime, trunc_time) {
  delta <- min(c(lifetime, trunc_time), na.rm = TRUE) - 1
  delta_p_m  <- max(trunc_time, na.rm = TRUE)
  m <- delta_p_m - delta
  ## if (is.null(C)) {
  ##   omega <- max(lifetime, na.rm = TRUE)
  ## } else {
  ##   omega <- max(lifetime[C < 1], na.rm = TRUE)
  ## }
  omega <- max(lifetime, na.rm = TRUE)
  if (delta + 1 > omega) {
    warning("There are less than 2 timepoints.")
    return(numeric(0)) 
  }
  eval_points <- retrieve_support(delta, m, omega)[["X"]]
  return(eval_points)
}

##' @title \eqn{\hat{f}(x)}
##' @inheritParams single_t_hazard
##' @return a scalar
##' @author lcgodoy
f_hat <- function(t, lifetime,
                  event,
                  censoring_indicator) {
  mean(censoring_indicator == 0 & event == 1 &
       lifetime == t)
}

##' @title \eqn{\hat{U}(x)}
##' @inheritParams single_t_hazard
##' @return a scalar
##' @author lcgodoy
u_hat <- function(t,
                  lifetime,
                  trunc_time) {
  mean(t >= trunc_time & t <= lifetime)
}

##' @title Variance of the log-transformed hazard estimate
##' @param lambda hazard rate
##' @param risk_set \eqn{hat{U}}
##' @param fh \eqn{hat{f}}
##' @param n sample size (or number of timepoints)
##' @return a scalar
##' @author lcgodoy
var_hat <- function(lambda, risk_set, fh, n) {
  uh <- risk_set
  lfh <- log(fh)
  luh <- log(uh)
  exp(log(uh - fh) -  log(n) - luh - lfh)
}

##' @title Hazard estimate for a single time-point.
##'
##' @description Internal use.
##' 
##' @param t A time point at which hazard estimates are sought.
##' @inheritParams estimate_hazard
##' @param event event indicator
##' @return A vector containing the time to event, \eqn{\hat{C}_n}, the number
##'   of events, and the hazard estimate along with its standard error.
##' @author lcgodoy
single_t_hazard <- function(t,
                            trunc_time,
                            lifetime,
                            event,
                            censoring_indicator) {
  fh <- f_hat(t, lifetime, event, censoring_indicator)
  uh <- u_hat(t, lifetime, trunc_time)
  n <- length(lifetime)
  hazard <- ifelse(uh == 0, 0, fh / uh)
  var_log_h <- - log(fh) - log1p(- hazard) - log(n)
  c(lifetime = t,
    risk_set = uh,
    hazard = hazard,
    se_log_hazard = ifelse(is.finite(var_log_h),
                           exp(0.5 * var_log_h),
                           0.0))
}

##' @title Auxiliary function for `estimate_hazard`
##' @param support where to calculate the hazards
##' @param event_indicator legacy.
##' @inheritParams estimate_hazard
##' @return a `data.frame`
##' @author lcgodoy
.hazard_core <- function(support, trunc_time,
                         lifetime, censoring_indicator,
                         event_indicator,
                         carry_hazard) {
  out <-
    sapply(support,
           single_t_hazard,
           trunc_time = trunc_time,
           lifetime = lifetime,
           censoring_indicator = censoring_indicator,
           event = event_indicator)
  out <- as.data.frame(t(out))
  if (carry_hazard)
    out <- fix_0haz(out)
  return(out)
}

##' @title Auxiliary function for `estimate_hazard`
##' @inheritParams estimate_hazard
##' @return Nothing
##' @author lcgodoy
check_censored <- function(lifetime, censoring_indicator, support_lifetime_rv) {
  max_support <- max(support_lifetime_rv, na.rm = TRUE)
  is_problematic <- (lifetime == max_support) & (censoring_indicator == 1)
  if (any(is_problematic, na.rm = TRUE)) {
    warning(paste(
      "Warning: Detected censored observations at the maximum observed limit of the support",
      "(lifetime == max(support_lifetime_rv)).",
      "This may lead to identifiability issues or unstable hazard estimates at the tail."
    ))
  }
}

##' @title Hazard rate
##'
##' @description Estimate the non-parametric hazard rate for truncated and
##'   censored data
##'
##' @details Point estimate and asymptotic confidence intervals are calculated
##'   based on <REFERENCES> (We can also include some brief notation/definitions
##'   here)
##' 
##' @param lifetime A numeric vector representing the observed time to
##'   event.
##' @param trunc_time A numeric vector representing the observed left-truncated
##'   time.
##' @param censoring_indicator An indicator for censoring (1=censored, 0=not). Defaults to
##'   a vector of 0s if `NULL`. An observation is only treated as an event if
##'   status=1 AND censoring=0.
##' @param event_type a vector of "events identifies" (experimental)
##' @param support_lifetime_rv A `vector` of time points at which to evaluate
##'   the hazard.  If `NULL` (the default), it is calculated for a sequence from
##'   `Delta + 1` to `omega` (that is, `max(lifetime)`).
##' @param carry_hazard A `boolean` indicator on whether 0 hazard estimates
##'   should be replaced by the last non-zero estimate. Defaults to `FALSE`
##' @param ci_level A number between 0 and 1 indicating the level of the
##'   confidence intervals.
##'
##' @export
##' 
##' @return A `data.frame` with the hazard estimate their standard errors and
##'   asymptotic confidence intervals.
##'
estimate_hazard <- function(lifetime,
                            trunc_time = NULL,
                            censoring_indicator = NULL,
                            event_type = NULL,
                            support_lifetime_rv = NULL,
                            carry_hazard = FALSE,
                            ci_level = .95) {
  n_obs <- length(lifetime)
  if (is.null(trunc_time)) {
    trunc_time <- rep(0, n_obs)
  }
  if (is.null(censoring_indicator)) {
    censoring_indicator <- rep(0, n_obs)
  }
  if (!is.null(trunc_time)) {
    stopifnot(length(lifetime) == length(trunc_time))
    stopifnot(!all(is.na(lifetime)))
  }
  ## if (!is.null(censoring)) {
  ##   stopifnot(!is.null(trunc_time))
  ## }
  ## taking censoring into account
  ## event <- ifelse(event == 1 & censoring == 0, 1, 0)
  ## evaluation points based on the paper
  if (is.null(support_lifetime_rv)) {
    Delta <- min(c(lifetime, trunc_time), na.rm = TRUE)
    m <- max(trunc_time, na.rm = TRUE)
    omega <- max(lifetime)
    support_lifetime_rv <- calc_osup(lifetime, trunc_time)
  }
  ## throws a warning if there are censored observations at
  ## max(support_lifetime_rv)
  check_censored(lifetime, censoring_indicator, support_lifetime_rv)
  run_by_type <-
    !is.null(event_type) && length(unique(event_type)) > 1
  if (!run_by_type) {
    event_i <- rep(1, n_obs)
    out <- .hazard_core(support_lifetime_rv,
                        trunc_time,
                        lifetime,
                        censoring_indicator,
                        event_i,
                        carry_hazard)
  } else {
    uncensored_id <- which(censoring_indicator == 0)
    etypes <- unique(event_type[uncensored_id])
    if (!all(unique(event_type) %in% etypes)) {
      warning("Dropping event types for which no lifetime information is available.")
    }
    out <- vector(mode = "list", length = length(etypes))
    for (i in seq_along(out)) {
      event_i <- as.integer(event_type == etypes[i])
      out[[i]] <-
        .hazard_core(support_lifetime_rv,
                     trunc_time,
                     lifetime,
                     censoring_indicator,
                     event_i,
                     carry_hazard)
      out[[i]] <-
        cbind.data.frame(event_type = etypes[i],
                         out[[i]])
    }
    out <- do.call(rbind, out)
    rownames(out) <- NULL
  }
  upper_tail <- 1 - .5 * (1 - ci_level)
  z <- stats::qnorm(upper_tail)
  out$lower_ci <-
    ifelse(out[["hazard"]] * (out[["hazard"]] - 1) == 0,
           out[["hazard"]],
           stats::plogis(stats::qlogis(out[["hazard"]]) -
                         z * out[["se_log_hazard"]]))
  out$upper_ci <-
    ifelse(out[["hazard"]] * (out[["hazard"]] - 1) == 0,
           out[["hazard"]],
           stats::plogis(stats::qlogis(out[["hazard"]]) +
                         z * out[["se_log_hazard"]]))
  out <- new_alife(out)
  return(out)
}
