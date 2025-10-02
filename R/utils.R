##' Carry Forward Last Observation for Zero-Hazard Estimates (Internal Use)
##'
##' @param haz_est A \code{data.frame} calculated using the function
##'   \code{estimate_hazard}
##'
##' @return A \code{data.frame} with hazard estimates carried forward when zeros
##'   are present.
fix_0haz <- function(haz_est) {
  hazard <- se_log_hazard <- NULL
  zero_rows <- with(haz_est, which(hazard == 0))
  if (requireNamespace("zoo", quietly = TRUE)) {
    haz_est$hazard[zero_rows] <- NA_real_
    haz_est$se_log_hazard[zero_rows] <- NA_real_
    haz_est <- zoo::na.locf(haz_est)
  } else {
    first_valid_row <- which(haz_est$hazard != 0)[1]
    if (is.na(first_valid_row))
      stop("All hazard estimates are zero.")
    ids <- first_valid_row:NROW(haz_est)
    zero_rows <- zero_rows[ids]
    haz_est <- haz_est[ids, ]
    for (i in seq_along(zero_rows)) {
      if (zero_rows[i]) {
        haz_est[i, c("hazard", "se_log_hazard")] <-
          haz_est[i - 1, c("hazard", "se_log_hazard")]
      }
    }
  }
  return(haz_est)
}
