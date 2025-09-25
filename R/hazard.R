##' @title Hazard rate
##'
##' @description Estimate the non-parametric hazard rate for truncated and
##'   censored data
##'
##' @details Point estimate and asymptotic confidence intervals are calculated
##'   based on <REFERENCES> (We can also include some brief notation/definitions
##'   here)
##' 
##' @param time The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times. Defaults to NULL (no
##'   truncation), which is equivalent to a vector of zeros.
##' @param status The event indicator vector (1=event, 0=censored). Defaults to
##'   NULL (no censoring), which is equivalent to a vector of ones.
##' @param censoring An indicator for censoring (1=censored, 0=not). Defaults to
##'   a vector of 0s if `NULL`. An observation is only treated as an event if
##'   status=1 AND censoring=0.
##' @param eval_points A `vector` of time points at which to evaluate the
##'   hazard.  If `NULL` (the default), it is calculated for a sequence from
##'   `delta + 1` to `delta + m`.
##' @param return_cdf A `boolean` indicator on whether to return the estimated
##'   CDF associated to the hazard rate or not. Default is `TRUE`
##'
##' @return A `data.frame` with the hazard estimate their standard errors and
##'   asymptotic confidence intervals.
##'
estimate_hazard <- function(time,
                            trunc_time = NULL,
                            status = NULL,
                            censoring = NULL,
                            eval_points = NULL,
                            return_cdf = TRUE) {
  n_obs <- length(time)
  if (is.null(trunc_time)) {
    trunc_time <- rep(0, n_obs)
  }
  if (is.null(status)) {
    status <- rep(1, n_obs)
  }
  if (is.null(censoring)) {
    censoring <- rep(0, n_obs)
  }
  if (!is.null(trunc_time)) {
    stopifnot(length(time) == length(trunc_time))
  }
  if (!is.null(censoring)) {
    stopifnot(!is.null(trunc_time))
  }
  ## avoiding NOTE (look for best practices here)
  hazard <- se_log_hazard <- NULL
  ## taking censoring into account
  status <- ifelse(status == 1 & censoring == 0, 1, 0)
  ## evaluation points based on the paper
  if (is.null(eval_points)) {
    delta <- min(c(time, trunc_time), na.rm = TRUE)
    m <- max(trunc_time, na.rm = TRUE)
    eval_points <- seq(from = delta + 1, to = delta + m)
  }
  results <- sapply(eval_points, function(t) {
    at_risk_idx <- (trunc_time <= t) & (time >= t)
    n_at_risk   <- sum(at_risk_idx)
    events_idx  <- (time == t) & (status == 1)
    n_events    <- sum(events_idx)
    if (n_at_risk == 0) {
      hazard <- 0
      var_log_h <- NA
    } else {
      hazard <- n_events / n_at_risk
      var_log_h <- ifelse(n_events > 0, (1 - hazard) / n_events, NA)
    }
    c(
        time = t,
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
