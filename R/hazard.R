##' Dimensions of Observable Support
##'
##' This helper function returns the observable support of the lifetime and
##' left-truncation random variables (e.g., Lautier et al., 2026).
##'
##' @param Delta A nonnegative integer denoting the period of time (in months)
##'   during which the ABS is marketed to prospective investors.  It is after
##'   the trust closes to new loan originations but before its first reporting
##'   month.
##' @param m A positive integer representing the total number of months over
##'   which the underlying consumer auto loans are originated. This defines the
##'   length of the loan origination window before the trust closes.
##' @param omega A positive integer denoting the known, finite upper bound of
##'   a loan's lifetime (in months), which is nonrandom and observable from its
##'   amortization schedule at contract signing (for example, for a 72-month
##'   loan, we likely have \eqn{\omega = 72}).
##' @param epsilon A positive integer (default is \code{NULL}) denoting the
##'   present time (the current calendar month age since the first loan was
##'   originated) of the data generation process for an active ABS pool. If
##'   \eqn{\varepsilon < \omega + m}, then right-censoring is present because it
##'   indicates some active loans are still making ongoing payments and have not
##'   yet terminated (Lautier et al., 2023)
##'
##' @note The observed support may not extend all the way to \eqn{\omega}
##'   (\code{omega}). In that scenario, we set \eqn{\omega} to the age of the
##'   oldest active loan in the ABS bond (corresponding to \eqn{\xi \equiv
##'   \min(\omega, \varepsilon - 1)} in the notation used in Lautier et
##'   al., 2023), with \eqn{\varepsilon} denoting the present time.
##'   
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
##' contingent cash-flows: A discrete-time survival analysis approach.
##' *Insurance: Mathematics and Economics*, 110, 53-71.
##' \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). Estimating a discrete
##' distribution subject to random left-truncation with an application to
##' structured finance. *Econometrics and Statistics*, 37, 174-198.
##' \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
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
##' This helper function generates the observable support of the lifetime
##' random variable based on the sample data (Lautier et al., 2023).
##' In particular, we assign \eqn{\Delta}
##' as the smallest observed time, \code{min(c(lifetime, trunc_time))}, and
##' \eqn{\omega} as the largest observed lifetime, \code{max(lifetime)}.  The
##' output is a sequence ranging from \eqn{\Delta + 1} to \eqn{\xi} (see
##' [retrieve_support()]).
##'
##' @param lifetime The vector of observed event and/or right-censoring times.
##' @param trunc_time The vector of observed left-truncation times.
##' 
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
##' contingent cash-flows: A discrete-time survival analysis approach.
##' *Insurance: Mathematics and Economics*, 110, 53-71.
##' \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
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

##' Empirical frequencies, unadjusted
##' 
##' This function calculates \eqn{\hat{f}}, which is the numerator of the
##' cause-specific hazard rate estimator from Lautier et al. (2026a).
##' 
##' @inheritParams single_t_hazard
##' 
##' @note The \eqn{\hat{f}} estimator simplifies to its equivalent in Lautier et
##' al. (2023) if there is only one event type (i.e., no competing risks).  If
##' there is both only one event type and no right-censoring, then the
##' \eqn{\hat{f}} estimator further simplifies to its equivalent in Lautier et
##' al. (2026b).
##'
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
##' contingent cash-flows: A discrete-time survival analysis approach.
##' *Insurance: Mathematics and Economics*, 110, 53-71.
##' \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
##' distribution subject to random left-truncation with an application to
##' structured finance. *Econometrics and Statistics*, 37, 174-198.
##' \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
##' 
##' @return a scalar probability between 0 and 1
##' 
##' @keywords internal
f_hat <- function(t, lifetime,
                  event,
                  censoring_indicator) {
  mean(censoring_indicator == 0 & event == 1 &
       lifetime == t)
}

##' Empirical risk set
##' 
##' This function calculates \eqn{\hat{U}}, which is the denominator of the
##' cause-specific hazard rate estimator from Lautier et al. (2026a).  This
##' quantity adjusts \eqn{\hat{f}} (see [f_hat()]) for the inherent survival
##' bias due to left-truncation.  In survival analysis, \eqn{\hat{U}} is
##' commonly referred to as the *risk set*.
##' 
##' @inheritParams single_t_hazard
##' 
##' @note The \eqn{\hat{U}} estimator simplifies to its equivalent in Lautier et
##' al. (2026b) if there is no right-censoring present in the data.
##' 
##' @return a scalar probability between 0 and 1
##' 
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
##' distribution subject to random left-truncation with an application to
##' structured finance. *Econometrics and Statistics*, 37, 174-198.
##' \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
##' 
##' @return a scalar
##' @keywords internal
u_hat <- function(t,
                  lifetime,
                  trunc_time) {
  mean(t >= trunc_time & t <= lifetime)
}

##' Asymptotic variance for logit-transformed confidence intervals
##' 
##' This function calculates the estimated asymptotic variance of the
##' logit-transformed hazard rate estimates.  It is used to construct the
##' asymptotic confidence intervals for the hazard rate estimates.
##' The logit-transformation ensures the estimated confidence intervals are
##' bounded between 0 and 1 (Lautier et al., 2026a).
##'  
##' @param risk_set The estimated risk set, \eqn{\hat{U}} (see [u_hat()]).
##' @param fh The estimated, unadjusted event probabilities, \eqn{\hat{f}}
##' (see [f_hat()]).
##' @param n The sample size.
##' 
##' @note The asymptotic, logit-transformed variance estimator simplifies to its
##' equivalent in Lautier et al. (2023) if there is only one event type (i.e.,
##' no competing risks).  If there is both only one event type and no
##' right-censoring, then the asymptotic, logit-transformed variance estimator
##' further simplifies to its equivalent in Lautier et al. (2026b).
##' 
##' @return a positive scalar
##' 
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
##' contingent cash-flows: A discrete-time survival analysis approach.
##' *Insurance: Mathematics and Economics*, 110, 53-71.
##' \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
##' distribution subject to random left-truncation with an application to
##' structured finance. *Econometrics and Statistics*, 37, 174-198.
##' \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
##' 
##' @keywords internal
##'

##  var_hat <- function(risk_set, fh, n) {
##   uh <- risk_set
##   lfh <- log(fh)
##   luh <- log(uh)
##   exp(log(uh - fh) -  log(n) - luh - lfh)
## }

##' Estimates the hazard rate for a single time point
##' 
##' This function calculates the estimated hazard rate, \eqn{\hat{\lambda}}, for
##' a single, desired time point.  It is the discrete time, hazard rate
##' estimator adjusted for random left-truncation, right-censoring, and
##' competing risks (Lautier et al., 2026a).
##' 
##' @param t The desired estimation time point.
##' @inheritParams estimate_hazard
##' @param event An event indicator for each competing risk.
##' 
##' @note The \eqn{\hat{\lambda}} estimator simplifies to its equivalent in
##' Lautier et al. (2023) if there is only one event type (i.e., no competing
##' risks).  If there is both only one event type and no right-censoring, then
##' the \eqn{\hat{\lambda}} estimator further simplifies to its equivalent in
##' Lautier et al. (2026b).
##' 
##' @return A named vector containing the desired estimation time point
##' (`lifetime`), the risk set estimate, \eqn{\hat{U}} (`risk_set`), the hazard
##' rate estimate, \eqn{\hat{\lambda}} (`hazard`), and the standard error of the
##' hazard estimate on the logit scale (`se_log_hazard`).
##' 
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
##' contingent cash-flows: A discrete-time survival analysis approach.
##' *Insurance: Mathematics and Economics*, 110, 53-71.
##' \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
##' distribution subject to random left-truncation with an application to
##' structured finance. *Econometrics and Statistics*, 37, 174-198.
##' \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
##' 
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

##' Estimates the vector of hazard rates over the lifetime support (auxiliary)
##' 
##' This function calculates the estimated hazard rate, \eqn{\hat{\lambda}}, for
##' the entire observable lifetime.  It is an auxiliary function for the main
##' user function, [estimate_hazard()].  (Please see [estimate_hazard()] for more
##' detailed user support.)
##' 
##' @param support A vector of all time points to estimate \eqn{\hat{\lambda}}.
##' @param event_indicator A binary vector that is set to 1 for all observations
##' with a terminating event type that matches the desired cause for the
##' hazard rate estimate and 0 otherwise.  When no competing events are present,
##' this vector is set to 1 for all observations.
##'   
##' @inheritParams estimate_hazard
##' 
##' @return a `data.frame` used within [estimate_hazard()].
##' 
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

##' Boundary point verification
##' 
##' The support conditions of Lautier et al. (2026) require that any
##' observations at the maximum observed lifetime time point are complete (i.e.,
##' there are no right-censored observations at the maximum of
##' `support_lifetime_rv`).  If
##' this is not true, then a user will need to decide on how to extrapolate
##' beyond the final observed time point.  As such, this function triggers a
##' warning message for users within [estimate_hazard()].  Hence, it is an
##' auxiliary function for use within [estimate_hazard()].
##' 
##' @inheritParams estimate_hazard
##' 
##' @return `NULL`.  This is an invisible function called for its connection to
##' a potential warning message within [estimate_hazard()].
##' 
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##' 
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

##' Estimating the discrete time, hazard rate subject to left-truncation
##'
##' This function calculates the estimated hazard rate, \eqn{\hat{\lambda}}, for
##' the entire vector of observable support.  It is the discrete time, hazard
##' rate estimator adjusted for random left-truncation, right-censoring, and
##' competing risks (Lautier et al., 2026a).
##'
##' @details Returns the vector of hazard rate estimates from Lautier et al.
##' (2026a, Theorem 2.1) with asymptotic confidence intervals from Lautier et
##' al. (2026a, Lemma 1).  When there are no competing risks, this function
##' returns the equivalent estimates and asymptotic confidence intervals from
##' Lautier et al. (2023).  When there are no competing risks and no
##' right-censoring, the estimates and asymptotic confidence intervals follow
##' from Lautier et al. (2026b).
##' 
##' @param lifetime A numeric vector representing the minimum of the observed
##'   termination time or right-censoring time.
##' @param trunc_time A numeric vector representing the observed
##'   left-truncation time. If `NULL` (the default), this becomes a zero vector
##'   (i.e., there is no left-truncation).
##' @param censoring_indicator An indicator to identify if an observation is
##'   right-censored (1) or complete (0). Defaults to a zero vector if `NULL`
##'   (i.e., there are no right-censored observations).
##'   An observation is only treated as an event (of a given type) if its
##'   right-censoring indicator is 0.
##' @param event_type An optional vector of event identifiers (e.g.,
##'   `"Default"` and `"Prepayment"`) for the case of competing risks. When
##'   this field has more than one unique value, cause-specific hazards are
##'   estimated for each unique event type.
##' @param support_lifetime_rv A `vector` of time points at which to evaluate
##'   the hazard. If `NULL` (the default), it is calculated by [calc_osup()] as
##'   a sequence from \eqn{\Delta + 1} to \eqn{\xi} (that is, `max(lifetime)`).
##' @param carry_hazard A `logical` indicator to determine whether or not zero
##'   hazard rate estimates should be replaced by the last non-zero hazard rate
##'   estimate (i.e., ``carried-forward''). Defaults to `FALSE`.
##' @param ci_level A number between 0 and 1 indicating the desired confidence
##'   interval width.  Defaults to 0.95.
##'
##' @export
##' 
##' @return An object of class `alife` (or `alife_multi`, when `event_type`
##'   has more than one unique value). These objects are each a `data.frame`
##'   with the time point, hazard estimate, standard error, and asymptotic
##'   confidence interval for all time points in the support.  We emphasize that
##'   the column called `se_log_hazard` represents the standard error of the
##'   `hazard` rate on the logit scale.
##'   
##' @references
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
##' contingent cash-flows: A discrete-time survival analysis approach.
##' *Insurance: Mathematics and Economics*, 110, 53-71.
##' \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
##' credit risk in current consumer automobile loans.  *Journal of the Royal
##' Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
##' \url{https://doi.org/10.1093/jrsssa/qnae137}}
##' 
##' {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
##' distribution subject to random left-truncation with an application to
##' structured finance. *Econometrics and Statistics*, 37, 174-198.
##' \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
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
