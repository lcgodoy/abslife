##' Carry Forward Last Observation for Zero-Hazard Estimates (Internal Use)
##'
##' @param haz_est A \code{data.frame} calculated using the function
##'   \code{estimate_hazard}
##'
##' @return A \code{data.frame} with hazard estimates carried forward when zeros
##'   are present.
fix_0haz <- function(haz_est) {
  hazard <- se_log_hazard <- NULL
  zero_rows <- with(haz_est, hazard == 0)
  if (requireNamespace("zoo", quietly = TRUE)) {
    haz_est$hazard[zero_rows] <- NA_real_
    haz_est$se_log_hazard[zero_rows] <- NA_real_
    haz_est <- zoo::na.locf(haz_est)
  } else {
    first_valid_row <- which(!zero_rows)[1]
    if (is.na(first_valid_row))
      stop("All hazard estimates are zero.")
    ids <- first_valid_row:NROW(haz_est)
    zero_rows <- zero_rows[ids]
    haz_est <- haz_est[ids, ]
    for (i in seq_along(zero_rows)[-1]) {
      if (zero_rows[i]) {
        haz_est[i, c("hazard", "se_log_hazard")] <-
          haz_est[i - 1, c("hazard", "se_log_hazard")]
      }
    }
  }
  return(haz_est)
}

##' @title Samples from a "abslife" object
##' @param n number of samples
##' @param x the output of a `calc_cdf` function
##'
##' @return a vector of "time to event" samples
##' @author lcgodoy
##' @export
ralife_cdf <- function(n, x) {
  if ("event_type" %in% names(x)) {
    etypes <- unique(x$event_type)
    out <- vector(mode = "list",
                  length = length(etypes))
    for (i in seq_along(etypes)) {
      x_sub <- x[x$event_type == etypes[i], ]
      u <- stats::runif(n)
      indices <- pmin(findInterval(u, x_sub$cdf) + 1, NROW(x_sub))
      out[[i]] <- data.frame(event_type = etypes[i],
                             lifetime = x_sub$lifetime[indices])
    }
    out <- do.call(rbind, out)
  } else {
    u <- stats::runif(n)
    indices <- pmin(findInterval(u, x$cdf) + 1, NROW(x))
    out <- x$lifetime[indices]
  }
  return(out)
}

##' @rdname rmat
aux_kmat <-
  function(hazard, rephaz, support_length) {
    nt <- support_length
    out <- rep(0.0, nt)
    out[(nt - rephaz + 1):nt] <- rep(- 1 / (1 - hazard), rephaz)
    return(out)
  }

##' @title Build the auxiliary matrix K
##' 
##' @param hazard A `vector` of estimated hazards at every timepoint.
##' @param se_log_hazard A `vector` of SE estimates for the log-hazards.
##' @param rephaz number of times `hazard` must be repeated (auxiliary)
##' @param support_length length of `hazard`.
##' @param pmfvar a square `matrix` corresponding to the output of a `build_pmf`
##'   call.
##'
##' @return A square `matrix` with number of rows (and columns) matching the
##'   dimension of `hazard`.
##' @name rmat
##' @author lcgodoy
build_kmat <- function(hazard) {
  nt <- length(hazard)
  reps <- rev(seq_len(nt))
  kmat <- mapply(aux_kmat, hazard = hazard,
                 rephaz = reps,
                 support_length = nt)
  return(kmat)
}

##' @rdname rmat
build_rmat <- function(hazard) {
  nt <- length(hazard)
  rmat <- matrix(0.0, ncol = nt, nrow = nt)
  diag(rmat) <- cumprod(1 - hazard)
  return(rmat)
}

##' @rdname rmat
build_amat <- function(support_length) {
  amat <- matrix(0.0,
                 ncol = support_length,
                 nrow = support_length)
  idsd <-
    seq(from = 2, by = support_length + 1,
        length.out = support_length - 1)
  diag(amat) <- - 1.0
  amat[idsd] <- 1.0
  return(amat)
}

##' @rdname rmat
build_sigmat <- function(hazard, se_log_hazard) {
  se_hazard <- se_log_hazard * hazard
  nt <- length(hazard)
  sigmat <- matrix(0.0, ncol = nt, nrow = nt)
  diag(sigmat) <- se_hazard
  return(sigmat)
}

##' @rdname rmat
build_pmfvar <- function(hazard, se_log_hazard) {
  sig_sqrt <- build_sigmat(hazard, se_log_hazard)
  kmat <- build_kmat(hazard)
  rmat <- build_rmat(hazard)
  amat <- build_amat(length(hazard))
  out <- amat %*% rmat %*% kmat %*% sig_sqrt
  out <- tcrossprod(out)
  return(out)
}

##' @rdname rmat
build_cdfvar <- function(pmfvar) {
  nt <- NROW(pmfvar)
  astmat <- matrix(1.0, nrow = nt, ncol = nt)
  astmat[lower.tri(astmat)] <- 0.0
  out <- crossprod(astmat, pmfvar)
  out <- out %*% astmat
  return(out)
}

## below is a more efficient way to compute those things

## build_q <- function(hazard, se_log_hazard) {
##   n <- length(hazard)
##   r_vec <- cumprod(1 - hazard)
##   sig_vec <- se_log_hazard * hazard
##   k_vec <- -1 / (1 - hazard)
##   Q <- outer(r_vec, k_vec * sig_vec)
##   Q[upper.tri(Q)] <- 0.0  
##   return(Q)
## }

## build_cdfpmfses <- function(hazard, se_log_hazard) {
##   qmat <- build_q(hazard, se_log_hazard)
##   cdf_se <- sqrt(diag(tcrossprod(qmat)))
##   q_pad <- rbind(0.0, qmat)
##   zmat <- -diff(q_pad)
##   pmf_se <- sqrt(diag(tcrossprod(zmat)))
##   return(cbind(cdf_se, pmf_se))
## }
