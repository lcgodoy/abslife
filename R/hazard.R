##' Calculate Default Time Points
##'
##' This helper function generates a default sequence of evaluation points based
##' on the study's overall time range (Lautier et al. 2023, <DOI:
##' 10.1016/j.ecosta.2023.05.005>). In particular, it calculates \eqn{\Delta}
##' and \eqn{m} based on left-truncation and time-to-event variables and outputs
##' a sequence ranging from \eqn{\Delta + 1} to \eqn{\omega}.
##'
##' @param time_to_event The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times.
##'
##' @return A numeric vector of default time points to evaluate the hazard at.
##' @export
calc_tp <- function(time_to_event, trunc_time) {
  delta <- min(c(time_to_event, trunc_time), na.rm = TRUE)
  omega <- max(time_to_event, na.rm = TRUE)
  if (delta + 1 > omega) {
    warning("There are less than 2 timepoints.")
    return(numeric(0)) 
  }
  eval_points <- seq.int(from = delta + 1, to = omega, by = 1L)
  return(eval_points)
}

##' @title \eqn{\hat{f}(x)}
##' @inheritParams single_t_hazard
##' @return a scalar
##' @author lcgodoy
f_hat <- function(t, time_to_event,
                  event,
                  censoring) {
  mean(censoring == 0 & event == 1 &
       time_to_event == t)
}

##' @title \eqn{\hat{U}(x)}
##' @inheritParams single_t_hazard
##' @return a scalar
##' @author lcgodoy
u_hat <- function(t,
                  time_to_event,
                  trunc_time) {
  mean(t >= trunc_time & t <= time_to_event)
}

##' @title Variance of the log-transformed hazard estimate
##' @param lambda hazard rate
##' @param uh \eqn{hat{U}}
##' @param fh \eqn{hat{f}}
##' @param n sample size (or number of timepoints)
##' @return a scalar
##' @author lcgodoy
var_hat <- function(lambda, uh, fh, n) {
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
                            time_to_event,
                            event,
                            censoring) {
  fh <- f_hat(t, time_to_event, event, censoring)
  uh <- u_hat(t, time_to_event, trunc_time)
  n <- length(time_to_event)
  if (uh == 0) {
    hazard <- 0
    var_log_h <- NA
  } else {
    hazard <- fh / uh
    var_log_h <-
      ifelse(hazard == 0,
             0,
             var_hat(hazard, uh, fh, n))
  }
  c(
      time_to_event = t,
      fh = fh,
      uh = uh,
      hazard = hazard,
      se_log_hazard = sqrt(var_log_h)
  )
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
##' @param time_to_event A numeric vector representing the observed time to
##'   event.
##' @param trunc_time A numeric vector representing the observed left-truncated
##'   time.
##' @param event_indicator The event indicator vector (1=default,
##'   0=censored). Defaults to NULL (no censoring), which is equivalent to a
##'   vector of ones.
##' @param censoring An indicator for censoring (1=censored, 0=not). Defaults to
##'   a vector of 0s if `NULL`. An observation is only treated as an event if
##'   status=1 AND censoring=0.
##' @param event_type a vector of "events identifies" (experimental)
##' @param support_lifetime_rv A `vector` of time points at which to evaluate
##'   the hazard.  If `NULL` (the default), it is calculated for a sequence from
##'   `Delta + 1` to `omega` (that is, `max(time_to_event)`).
##' @param return_cdf A `boolean` indicator on whether to return the estimated
##'   CDF associated to the hazard rate or not. Default is `TRUE`
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
estimate_hazard <- function(time_to_event,
                            trunc_time = NULL,
                            event_indicator = NULL,
                            censoring = NULL,
                            event_type = NULL,
                            support_lifetime_rv = NULL,
                            return_cdf = TRUE,
                            carry_hazard = FALSE,
                            ci_level = .95) {
  n_obs <- length(time_to_event)
  if (is.null(trunc_time)) {
    trunc_time <- rep(0, n_obs)
  }
  if (is.null(event_indicator)) {
    event_indicator <- rep(1, n_obs)
  }
  if (is.null(censoring)) {
    censoring <- rep(0, n_obs)
  }
  if (!is.null(trunc_time)) {
    stopifnot(length(time_to_event) == length(trunc_time))
    stopifnot(!all(is.na(time_to_event)))
  }
  ## if (!is.null(censoring)) {
  ##   stopifnot(!is.null(trunc_time))
  ## }
  ## avoiding NOTE (look for best practices here)
  hazard <- se_log_hazard <- NULL
  ## taking censoring into account
  ## event <- ifelse(event == 1 & censoring == 0, 1, 0)
  ## evaluation points based on the paper
  if (is.null(support_lifetime_rv)) {
    Delta <- min(c(time_to_event, trunc_time), na.rm = TRUE)
    m <- max(trunc_time, na.rm = TRUE)
    omega <- max(time_to_event)
    support_lifetime_rv <- calc_tp(time_to_event, trunc_time)
  }
  etype_check <- ifelse(is.null(event_type), NA,
                 ifelse(length(unique(event_type)) == 1,
                        NA, event_type))
  if (is.na(etype_check)) {
    results <- sapply(support_lifetime_rv,
                      single_t_hazard,
                      trunc_time = trunc_time,
                      time_to_event = time_to_event,
                      censoring = censoring,
                      event = event_indicator)
    out <- as.data.frame(t(results))
    if (carry_hazard)
      out <- fix_0haz(out)
  } else {
    etypes <- unique(event_type)
    out <- vector(mode = "list", length = length(etypes))
    for (i in seq_along(out)) {
      event_i <- as.integer(event_type == etypes[i])
      out[[i]] <-
        sapply(support_lifetime_rv,
               single_t_hazard,
               trunc_time = trunc_time,
               time_to_event = time_to_event,
               event = event_i,
               censoring = censoring) |>
        t() |>
        as.data.frame()
      out[[i]] <-
        cbind.data.frame(event_type = etypes[i],
                         out[[i]])
      if (carry_hazard)
        out[[i]] <- fix_0haz(out[[i]])
    }
    out <- do.call(rbind, out)
    rownames(out) <- NULL
  }
  upper_tail <- 1 - .5 * (1 - ci_level)
  z <- stats::qnorm(upper_tail)
  out <- 
    transform(out,
              lower_ci = exp(log(hazard) - z * se_log_hazard),
              upper_ci = exp(log(hazard) + z * se_log_hazard))
  if (return_cdf)
    out <- transform(out, cdf = 1 - cumprod(1 - hazard))
  out <- new_alife(out)
  return(out)
}
