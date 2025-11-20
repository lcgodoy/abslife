##' Plot Method for an 'alife' Object
##'
##' @param x An object of class `alife`. Typically the output of the
##'   `estimate_hazard` function.
##' @param col_ci The color for the confidence interval polygon.
##' @param col_line The color for the hazard rate line.
##' @param ... Additional arguments passed to the base `plot` function (e.g.,
##'   `main`, `xlab`, `ylab`, `ylim`).
##'
##' @importFrom graphics polygon lines
##' @seealso [estimate_hazard()]
##' @return A plot of the hazard rate.
##' @export
##'
plot.alife <- function(x, col_ci = "skyblue", col_line = "navy", ...) {
  args <- list(...)
  defaults <- list(
      xlab = "x",
      ylab = "Hazard rate",
      main = NA_character_,
      ylim = range(c(x$lower_ci, x$upper_ci), na.rm = TRUE),
      xlim = range(x$time, na.rm = TRUE)
  )
  plot_args <- utils::modifyList(defaults, args)
  do.call("plot",
          c(list(x = x$time_to_event, y = x$hazard, type = "n"),
            plot_args))
  ci_data <- x[!is.na(x$lower_ci) & !is.na(x$upper_ci), ]
  polygon(
      x = c(ci_data$time_to_event, rev(ci_data$time_to_event)),
      y = c(ci_data$lower_ci, rev(ci_data$upper_ci)),
      col = col_ci,
      border = NA
  )
  lines(ci_data$time_to_event, ci_data$hazard, col = col_line, lwd = 2)
}

##' Plot Method for an 'alife_multi' Object
##'
##' Creates a faceted plot, with one panel per event type.
##'
##' @param x An object of class `alife-multi`.
##' @param col_ci The color for the confidence interval polygon.
##' @param col_line The color for the hazard rate line.
##' @param ... Additional arguments passed to the base `plot` function (e.g.,
##'   `xlab`, `ylab`, `ylim`).
##'
##' @importFrom graphics polygon lines par
##' @seealso [estimate_hazard()]
##' @return A faceted plot of the hazard rates.
##' @export
##'
plot.alife_multi <- function(x, col_ci = "skyblue", col_line = "navy", ...) {
  etypes <- unique(x$event_type)
  n_types <- length(etypes)
  n_cols <- ceiling(sqrt(n_types))
  n_rows <- ceiling(n_types / n_cols)
  old_par <- par(no.readonly = TRUE) 
  on.exit(par(old_par))
  par(mfrow = c(n_rows, n_cols), 
      mar = c(4, 4, 2, 1))
  for (et in etypes) {
    # Subset the data for this event type
    x_sub <- x[x$event_type == et, ]
    args <- list(...)
    defaults <- list(
      xlab = "x",
      ylab = "Cause specific hazard rate",
      # Use the event type in the title
      main = paste("Event:", et),
      ylim = range(c(x_sub$lower_ci, x_sub$upper_ci), na.rm = TRUE),
      xlim = range(x_sub$time_to_event, na.rm = TRUE)
    )
    plot_args <- utils::modifyList(defaults, args)
    do.call("plot",
            c(list(x = x_sub$time_to_event, y = x_sub$hazard, type = "n"),
              plot_args))
    ci_data <- x_sub[!is.na(x_sub$lower_ci) & !is.na(x_sub$upper_ci), ]    
    polygon(
      x = c(ci_data$time_to_event, rev(ci_data$time_to_event)),
      y = c(ci_data$lower_ci, rev(ci_data$upper_ci)),
      col = col_ci,
      border = NA
    )
    lines(ci_data$time_to_event, ci_data$hazard, col = col_line, lwd = 2)
  }
}

##' Summary Method for an 'alife' Object
##'
##' @param object An object of class `alife`. Typically the output of the
##'   `estimate_hazard` function.
##' @param by an `integer` defining the periodicity of the summary.
##' @param ... Additional arguments passed to the base `print` function (e.g.,
##'   `digits`).
##'
##' @seealso [estimate_hazard()]
##' @return A summary of the hazard rate.
##' @export
summary.alife <- function(object, by = 5, ...) {
  lower <- min(object$time_to_event, na.rm = TRUE)
  upper <- max(object$time_to_event, na.rm = TRUE)
  times <- seq.int(from = lower, to = upper, by = by)
  cols <- c("time_to_event",
            "hazard",
            "se_log_hazard",
            "lower_ci",
            "upper_ci")
  object[object$time_to_event %in% times, cols]
}

##' Summary Method for an 'alife_multi' Object
##'
##' @param object An object of class `alife`. Typically the output of the
##'   `estimate_hazard` function.
##' @param by an `integer` defining the periodicity of the summary.
##' @param ... Additional arguments passed to the base `print` function (e.g.,
##'   `digits`).
##'
##' @seealso [estimate_hazard()]
##' @return A summary of the hazard rate.
##' @export
summary.alife_multi <- function(object, by = 5, ...) {
  lower <- min(object$time_to_event, na.rm = TRUE)
  upper <- max(object$time_to_event, na.rm = TRUE)
  times <- seq.int(from = lower, to = upper, by = by)
  cols <- c("event_type",
            "time_to_event",
            "hazard",
            "se_log_hazard",
            "lower_ci",
            "upper_ci")
  df_list <- split(object, object$event_type)
  df_list <- lapply(df_list, function(df) {
    df[df$time_to_event %in% times, cols]
  })
  out <- do.call(rbind, df_list)
  rownames(out) <- NULL
  return(out)
}

##' @title Calculate CDF from Hazard Estimates
##'
##' @description Adds a 'cdf' column to an 'alife' or 'alife-multi' object based
##'   on the hazard estimates.
##'
##' @param x An object of class `alife` or `alife-multi`.
##' @param ... Not used.
##'
##' @return The original object `x` with a new `cdf` column.
##' @export
calc_cdf <- function(x, ...) {
  UseMethod("calc_cdf")
}

##' @export
##' @rdname calc_cdf
calc_cdf.alife <- function(x, ...) {
  x$cdf <- 1 - cumprod(1 - x$hazard)
  return(x)
}

##' @export
##' @rdname calc_cdf
calc_cdf.alife_multi <- function(x, ...) {
  df_list <- split(x, x$event_type)
  df_list_with_cdf <- lapply(df_list, function(df) {
    df$cdf <- 1 - cumprod(1 - df$hazard)
    return(df)
  })
  out <- do.call(rbind, df_list_with_cdf)
  rownames(out) <- NULL
  validate_alife(out)
  return(out)
}
