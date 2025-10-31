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

  # Find the first row with a non-zero hazard
  first_valid_row <- which(!zero_rows)[1]

  # If all hazards are zero or there are no rows, do nothing.
  if (is.na(first_valid_row)) {
    return(haz_est)
  }

  # Iterate from the row *after* the first valid one
  for (i in (first_valid_row + 1):NROW(haz_est)) {
    # If the current row is a zero-hazard row, replace it with the previous one
    if (zero_rows[i]) {
      haz_est[i, c("hazard", "se_log_hazard")] <-
        haz_est[i - 1, c("hazard", "se_log_hazard")]
    }
  }

  return(haz_est)
}
