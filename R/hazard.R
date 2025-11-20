##' Calculate Default Time Points
##'
##' This helper function generates a default sequence of evaluation points based
##' on the study's overall time range (Lautier et al. 2023, <DOI:
##' 10.1016/j.ecosta.2023.05.005>). In particular, it calculates \eqn{\Delta}
##' and \eqn{m} based on left-truncation and time-to-event variables and outputs
##' a sequence ranging from \eqn{\Delta + 1} to \eqn{\omega}.
##'
##' @param lifetime The vector of event or censoring times.
##' @param trunc_time The vector of left-truncation times.
##'
##' @return A numeric vector of default time points to evaluate the hazard at.
##' @export
calc_tp <- function(lifetime, trunc_time) {
  delta <- min(c(lifetime, trunc_time), na.rm = TRUE)
  omega <- max(lifetime, na.rm = TRUE)
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
                            lifetime,
                            event,
                            censoring_indicator) {
  fh <- f_hat(t, lifetime, event, censoring_indicator)
  uh <- u_hat(t, lifetime, trunc_time)
  n <- length(lifetime)
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
      lifetime = t,
      fh = fh,
      uh = uh,
      hazard = hazard,
      se_log_hazard = sqrt(var_log_h)
  )
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
    support_lifetime_rv <- calc_tp(lifetime, trunc_time)
  }
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
    etypes <- unique(event_type)
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
  out$lower_ci <- ifelse(out$hazard == 0, NA_real_,
                         exp(log(out$hazard) - z * out$se_log_hazard))
  out$upper_ci <- ifelse(out$hazard == 0, NA_real_,
                         exp(log(out$hazard) + z * out$se_log_hazard))
  out <- new_alife(out)
  return(out)
}
