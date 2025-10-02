##' Calculate Default Time Points
##'
##' This helper function generates a default sequence of evaluation points based
##' on the study's overall time range (PUT REFERENCE HERE). In particular, it
##' calculates \eqn{\delta} and \eqn{m} based on left-truncation and
##' time-to-event variables and outputs a sequence ranging from \eqn{\delta + 1}
##' to ??.
##'
##' @param time_to_event The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times.
##'
##' @return A numeric vector of default time points to evaluate the hazard at.
##' @export
calc_tp <- function(time_to_event, trunc_time) {
  delta <- min(c(time_to_event, trunc_time), na.rm = TRUE)
  omega <- max(time_to_event)
  eval_points <- seq(from = delta + 1, to = omega)
  return(eval_points)
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
##' @param time_to_event The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times. Defaults to NULL (no
##'   truncation), which is equivalent to a vector of zeros.
##' @param event_type The event indicator vector (1=default,
##'   0=censored). Defaults to NULL (no censoring), which is equivalent to a
##'   vector of ones.
##' @param censoring An indicator for censoring (1=censored, 0=not). Defaults to
##'   a vector of 0s if `NULL`. An observation is only treated as an event if
##'   status=1 AND censoring=0.
##' @param support_event A `vector` of time points at which to evaluate the
##'   hazard.  If `NULL` (the default), it is calculated for a sequence from
##'   `delta + 1` to `??`.
##' @param return_cdf A `boolean` indicator on whether to return the estimated
##'   CDF associated to the hazard rate or not. Default is `TRUE`
##'
##' @export
##' 
##' @return A `data.frame` with the hazard estimate their standard errors and
##'   asymptotic confidence intervals.
##'
estimate_hazard <- function(time_to_event,
                            trunc_time = NULL,
                            event_type = NULL,
                            censoring = NULL,
                            support_event = NULL,
                            support_cens = NULL,
                            return_cdf = TRUE) {
  n_obs <- length(time_to_event)
  if (is.null(trunc_time)) {
    trunc_time <- rep(0, n_obs)
  }
  if (is.null(event_type)) {
    event_type <- rep(1, n_obs)
  }
  if (is.null(censoring)) {
    censoring <- rep(0, n_obs)
  }
  if (!is.null(trunc_time)) {
    stopifnot(length(time_to_event) == length(trunc_time))
    stopifnot(!all(is.na(time_to_event)))
  }
  if (!is.null(censoring)) {
    stopifnot(!is.null(trunc_time))
  }
  ## avoiding NOTE (look for best practices here)
  hazard <- se_log_hazard <- NULL
  ## taking censoring into account
  event_type <- ifelse(event_type == 1 & censoring == 0, 1, 0)
  ## evaluation points based on the paper
  if (is.null(support_event)) {
    delta <- min(c(time_to_event, trunc_time), na.rm = TRUE)
    m <- max(trunc_time, na.rm = TRUE)
    omega <- max(time_to_event)
    support_event <- calc_tp(time_to_event, trunc_time)
  }
  results <- sapply(support_event, function(t) {
    at_risk_idx <- (trunc_time <= t) & (time >= t)
    n_at_risk   <- sum(at_risk_idx)
    events_idx  <- (time_to_event == t) & (event_type == 1)
    n_events    <- sum(events_idx)
    if (n_at_risk == 0) {
      hazard <- 0
      var_log_h <- NA
    } else {
      hazard <- n_events / n_at_risk
      var_log_h <- ifelse(n_events > 0, (1 - hazard) / n_events, NA)
    }
    c(
        time_to_event = t,
        n_risk = n_at_risk,
        n_event = n_events,
        hazard = hazard,
        se_log_hazard = sqrt(var_log_h)
    )
  })
  out <- as.data.frame(t(results))
  out <- 
    transform(out,
              lower_ci = exp(log(hazard) - 1.96 * se_log_hazard),
              upper_ci = exp(log(hazard) + 1.96 * se_log_hazard))
  if (return_cdf)
    out <- transform(out, cdf = 1 - cumprod(1 - hazard))
  out <- new_alife(out)
  return(out)
}
