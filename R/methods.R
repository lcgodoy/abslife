##' Plot Method for an 'alife' Object
##'
##' @param x An object of class `alife`. Typically the output of the
##'   `estimate_hazard` function.
##' @param col_ci The color for the confidence interval polygon.
##' @param col_line The color for the hazard rate line.
##' @param ... Additional arguments passed to the base `plot` function (e.g.,
##'   `main`, `xlab`, `ylab`, `ylim`).
##'
##' @importFrom graphics polygon lines abline
##' @seealso [estimate_hazard()]
##' @return A plot of the hazard rate.
##' @export
##'
plot.alife <- function(x, col_ci = "skyblue", col_line = "navy", ...) {
  args <- list(...)
  defaults <- list(
      xlab = "t",
      ylab = expression(hat(lambda)(t)),
      main = "Hazard Rate Estimate",
      ylim = range(c(x$lower_ci, x$upper_ci), na.rm = TRUE),
      xlim = range(x$time, na.rm = TRUE)
  )
  ## avoiding NOTE (look for best practices here)
  lower_ci <- upper_ci <- time_to_event <- hazard <- trunc_time <- NULL
  plot_args <- utils::modifyList(defaults, args)
  delta <- min(c(x$time_to_event, x$trunc_time), na.rm = TRUE)
  m <- max(x$trunc_time, na.rm = TRUE)
  do.call("plot",
          c(list(x = x$time_to_event, y = x$hazard, type = "n"),
            plot_args))
  ci_data <- subset(x, !is.na(lower_ci) & !is.na(upper_ci))
  polygon(
      x = c(ci_data$time_to_event, rev(ci_data$time_to_event)),
      y = c(ci_data$lower_ci, rev(ci_data$upper_ci)),
      col = col_ci,
      border = NA
  )
  lines(ci_data$time_to_event, ci_data$hazard, col = col_line, lwd = 2)
  abline(v = m + delta, lty = 2)
}
