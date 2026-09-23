##' Computes the observable support of a lifetime variable
##'
##' This helper function generates the observable supports of the lifetime and
##' left-truncation random variables (Lautier et al. 2025,
##' \doi{10.1214/25-AOAS2103}).
##'
##' @param Delta a nonnegative integer denoting the period of time (in months)
##'   during which the ABS is marketed to prospective investors.  It is after
##'   the trust closes to new loan originations but before its first reporting
##'   month.
##' @param m is a positive integer representing the total number of months over
##'   which the underlying consumer auto loans are originated. This defines the
##'   length of the loan origination window before the trust closes.
##' @param omega is a positive integer denoting the known, finite upper bound of
##'   a loan's lifetime (in months), which is nonrandom and observable from its
##'   amortization schedule at contract signing (for example, for a 72-month
##'   loan, we likely have \eqn{\omega = 72}).
##' @param epsilon is a positive integer (default is \code{NULL}) denoting the
##'   present time (the current calendar month age since the first loan was
##'   originated) of the data generation process for an active ABS pool. If
##'   \eqn{\varepsilon < \omega + m}, then right-censoring is present because it
##'   indicates some active loans are still making ongoing payments and have not
##'   yet terminated.
##'
##' @note The observed support may not extend all the way to \eqn{\omega}
##'   (\code{omega}). In that scenario, we set \eqn{\omega} to the age of the
##'   oldest active loan in the ABS bond (corresponding to \eqn{\xi \equiv
##'   \min(\omega, \varepsilon - 1)} in the notation used in Lautier et
##'   al. 2025), with \eqn{\varepsilon} denoting the present time.
##' 
##' @return A list with two integer vectors: \code{X}, the observable support
##'   of the lifetime variable (from \eqn{\Delta + 1} to \eqn{\xi}), and
##'   \code{Y}, the support of the left-truncation variable (from
##'   \eqn{\Delta + 1} to \eqn{\Delta + m}).
##' @export
retrieve_support <- function(Delta, m, omega,
                             epsilon = NULL) {
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
##' This helper function generates the observable support of the lifetime variable
##' based on the study's overall time range (Lautier et al. 2023,
##' \doi{10.1016/j.ecosta.2023.05.005}). In particular, it takes \eqn{\Delta}
##' as the smallest observed time, \code{min(c(lifetime, trunc_time))}, and
##' \eqn{\omega} as the largest observed lifetime, \code{max(lifetime)}, and
##' outputs a sequence ranging from \eqn{\Delta + 1} to \eqn{\omega} (see
##' [retrieve_support()]).
##'
##' @param lifetime The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times.
##'
##' @return A numeric vector representing the observed support of the lifetime
##'   variable. An empty vector (with a warning) if fewer than two time points
##'   are available.
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
  eval_points <- retrieve_support(delta + 1, m, omega)[["X"]]
  return(eval_points)
}

##' @title \eqn{\hat{f}(x)}
##' @inheritParams single_t_hazard
##' @return a scalar
##' @author lcgodoy
##' @keywords internal
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
##' @keywords internal
u_hat <- function(t,
                  lifetime,
                  trunc_time) {
  mean(t >= trunc_time & t <= lifetime)
}

##' @title Variance of the log-transformed hazard estimate
##' @param lambda hazard rate (currently unused).
##' @param risk_set \eqn{\hat{U}}
##' @param fh \eqn{\hat{f}}
##' @param n sample size (or number of timepoints)
##' @return a scalar
##' @author lcgodoy
##' @keywords internal
var_hat <- function(lambda, risk_set, fh, n) {
  uh <- risk_set
  lfh <- log(fh)
  luh <- log(uh)
  exp(log(uh - fh) -  log(n) - luh - lfh)
}

##' @title Hazard estimate for a single time-point
##'
##' @description Internal use.
##' 
##' @param t A time point at which hazard estimates are sought.
##' @inheritParams estimate_hazard
##' @param event event indicator
##' @return A named vector containing the time point (`lifetime`), the risk
##'   set estimate \eqn{\hat{U}(t)} (`risk_set`), the hazard estimate
##'   (`hazard`), and the standard error of the hazard estimate at the logit
##'   scale (`se_log_hazard`).
##' @author lcgodoy
##' @keywords internal
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

##' @title Hazard estimates over a support (auxiliary function for
##'   `estimate_hazard`)
##' @param support where to calculate the hazards
##' @param event_indicator A binary vector flagging the observations whose
##'   event is of the type being estimated (all ones when there are no
##'   competing risks).
##' @inheritParams estimate_hazard
##' @return a `data.frame`
##' @author lcgodoy
##' @keywords internal
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

##' @title Check for censoring at the end of the support (auxiliary function
##'   for `estimate_hazard`)
##' @description Throws a warning when there are censored observations at the
##'   maximum of `support_lifetime_rv`.
##' @inheritParams estimate_hazard
##' @return `NULL`, invisibly. Called for its side effect.
##' @author lcgodoy
##' @keywords internal
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
##'   censored data.
##'
##' @details Point estimate and asymptotic confidence intervals are calculated
##'   based on Lautier et al. 2023, \doi{10.1016/j.ecosta.2023.05.005}; Lautier
##'   et al. 2025, \doi{10.1214/25-AOAS2103}.
##' 
##' @param lifetime A numeric vector representing the observed time to
##'   event (or censoring).
##' @param trunc_time A numeric vector representing the observed
##'   left-truncation time. If `NULL` (the default), it is set to a vector of
##'   0s (i.e., no left-truncation).
##' @param censoring_indicator An indicator for censoring (1 = censored, 0 =
##'   not). Defaults to a vector of 0s if `NULL`. An observation is only
##'   treated as an event (of a given type) if its censoring indicator is 0.
##' @param event_type An optional vector of event identifiers (e.g.,
##'   `"Default"` and `"Prepayment"`) for competing risks (experimental). When
##'   it has more than one unique value, cause-specific hazards are estimated
##'   for each event type.
##' @param support_lifetime_rv A `vector` of time points at which to evaluate
##'   the hazard. If `NULL` (the default), it is calculated by [calc_osup()] as
##'   a sequence from `Delta + 1` to `omega` (that is, `max(lifetime)`).
##' @param carry_hazard A `logical` indicator on whether 0 hazard estimates
##'   should be replaced by the last non-zero estimate. Defaults to `FALSE`.
##' @param ci_level A number between 0 and 1 indicating the level of the
##'   confidence intervals.
##'
##' @export
##' 
##' @return An object of class `alife` (or `alife_multi`, when `event_type`
##'   has more than one unique value), that is, a `data.frame` with the hazard
##'   estimates, their standard errors, and asymptotic confidence intervals.
##'   Importantly, in the output, the column called `se_log_hazard` represents
##'   the standard error of the `hazard` rate at the `logit` scale.
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
