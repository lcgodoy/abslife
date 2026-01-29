##' @title Computing multiple CIs at once.
##' @inheritParams plot.alife
##' @return a \code{list}
##' @name mci
##' @author lcgodoy
multiple_cis <- function(x, ci_level = .95) UseMethod("multiple_cis", x)

##' @rdname mci
multiple_cis.alife <- function(x, ci_level = .95) {
  nci_level <- length(ci_level)
  out <- vector(mode = "list", length = length(ci_level))
  for (i in seq_along(out)) {
    upper_tail <- 1 - .5 * (1 - ci_level[[i]])
    z <- stats::qnorm(upper_tail)
    out[[i]] <- data.frame(lifetime = x[["lifetime"]],
                           lower_ci = ifelse(x[["hazard"]] == 0, 0,
                                             exp(log(x[["hazard"]]) -
                                                 z * x[["se_log_hazard"]])),
                           upper_ci = ifelse(x[["hazard"]] == 0, 0,
                                             pmin(exp(log(x[["hazard"]]) +
                                                      z * x[["se_log_hazard"]]),
                                                  1)))
  }
  names(out) <- ci_level
  return(out)
}

##' @rdname mci
multiple_cis.alife_multi <- function(x, ci_level = .95) {
  out <- vector(mode = "list", length = length(ci_level))
  for (i in seq_along(out)) {
    upper_tail <- 1 - .5 * (1 - ci_level[[i]])
    z <- stats::qnorm(upper_tail)
    lower_vals <- ifelse(x[["hazard"]] == 0, 0,
                         exp(log(x[["hazard"]]) - z * x[["se_log_hazard"]]))
    upper_vals <- ifelse(x[["hazard"]] == 0, 0,
                         pmin(exp(log(x[["hazard"]]) +
                                  z * x[["se_log_hazard"]]),
                              1))
    out[[i]] <- data.frame(lifetime = x[["lifetime"]],
                           event_type = x[["event_type"]],
                           lower_ci = lower_vals,
                           upper_ci = upper_vals)
  }
  names(out) <- ci_level
  return(out)
}

##' @rdname mci
multiple_cis.acdf <- function(x, ci_level = .95) {
  out <- vector(mode = "list", length = length(ci_level))
  for (i in seq_along(out)) {
    upper_tail <- 1 - .5 * (1 - ci_level[[i]])
    z <- stats::qnorm(upper_tail)
    cdf_lower <- pmax(0, x[["cdf"]] - z * x[["se_cdf"]])
    cdf_upper <- pmin(1, x[["cdf"]] + z * x[["se_cdf"]])
    dens_lower <- pmax(0, x[["density"]] - z * x[["se_dens"]])
    dens_upper <- x[["density"]] + z * x[["se_dens"]]    
    out[[i]] <- data.frame(
      lifetime   = x[["lifetime"]],
      cdf_lower  = cdf_lower,
      cdf_upper  = cdf_upper,
      dens_lower = dens_lower,
      dens_upper = dens_upper
    )
  }
  names(out) <- ci_level
  return(out)
}

##' @rdname mci
##' @export
multiple_cis.acdf_multi <- function(x, ci_level = .95) {
  out <- vector(mode = "list", length = length(ci_level))
  
  for (i in seq_along(out)) {
    upper_tail <- 1 - .5 * (1 - ci_level[[i]])
    z <- stats::qnorm(upper_tail)

    cdf_lower <- pmax(0, x[["cdf"]] - z * x[["se_cdf"]])
    cdf_upper <- pmin(1, x[["cdf"]] + z * x[["se_cdf"]])
    
    dens_lower <- pmax(0, x[["density"]] - z * x[["se_dens"]])
    dens_upper <- x[["density"]] + z * x[["se_dens"]]    
    
    out[[i]] <- data.frame(
      lifetime   = x[["lifetime"]],
      event_type = x[["event_type"]],
      cdf_lower  = cdf_lower,
      cdf_upper  = cdf_upper,
      dens_lower = dens_lower,
      dens_upper = dens_upper
    )
  }
  names(out) <- ci_level
  return(out)
}

##' Plot Method for an 'alife' Object
##'
##' @param x An object of class `alife`. Typically the output of the
##'   `estimate_hazard` function.
##' @param ci_level A numeric vector of confidence ci_level to plot (e.g.,
##'   \code{c(0.5, 0.95)}). Defaults to \code{0.95}.
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
plot.alife <- function(x, ci_level = 0.95,
                       col_ci = 2,
                       col_line = 1,
                       ...) {
  ci_level <- sort(ci_level, decreasing = TRUE)
  cis_list <- multiple_cis(x, ci_level = ci_level)
  all_lower <- unlist(lapply(cis_list, \(x) x[["lower_ci"]]))
  all_upper <- unlist(lapply(cis_list, \(x) x[["upper_ci"]]))
  args <- list(...)
  defaults <- list(
      xlab = "x",
      ylab = "Hazard rate",
      main = NA_character_,
      ylim = range(c(all_lower, all_upper), na.rm = TRUE),
      xlim = range(x$lifetime, na.rm = TRUE)
  )
  plot_args <- utils::modifyList(defaults, args)
  do.call("plot",
          c(list(x = x$lifetime, y = x$hazard, type = "n"),
            plot_args))
  n_ci_level <- length(ci_level)
  transparency <-
    if (n_ci_level > 1) seq(0.2, 0.8, length.out = n_ci_level) else 0.8
  for (i in seq_along(ci_level)) {
    lvl_name <- as.character(ci_level[i])
    ci_data <- cis_list[[lvl_name]]
    polygon(
        x = c(ci_data[["lifetime"]], rev(ci_data[["lifetime"]])),
        y = c(ci_data[["lower_ci"]], rev(ci_data[["upper_ci"]])),
        col = grDevices::adjustcolor(col_ci, alpha.f = transparency[i]),
        border = NA
    )
  }
  lines(x$lifetime, x$hazard, col = col_line, lwd = 2)
}

##' Plot Method for an 'alife_multi' Object
##'
##' Creates a faceted plot, with one panel per event type.
##'
##' @param x An object of class `alife-multi`.
##' @param ci_level A numeric vector of confidence ci_level to plot (e.g.,
##'   \code{c(0.5, 0.95)}). Defaults to \code{0.95}.
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
plot.alife_multi <- function(x, ci_level = 0.95,
                             col_ci = 2, col_line = 1, ...) {
  ci_level <- sort(ci_level, decreasing = TRUE)
  cis_list <- multiple_cis(x, ci_level = ci_level)
  etypes <- unique(x$event_type)
  n_types <- length(etypes)
  n_cols <- ceiling(sqrt(n_types))
  n_rows <- ceiling(n_types / n_cols)
  n_ci_level <- length(ci_level)
  transparency <-
    if (n_ci_level > 1) seq(0.2, 0.8, length.out = n_ci_level) else 0.8
  old_par <- par(no.readonly = TRUE) 
  on.exit(par(old_par))
  par(mfrow = c(n_rows, n_cols), 
      mar = c(4, 4, 2, 1))
  for (et in etypes) {
    x_sub <- x[x$event_type == et, ]
    all_lower <- unlist(lapply(cis_list,
                               \(x) x[x$event_type == et, ][["lower_ci"]]))
    all_upper <- unlist(lapply(cis_list,
                               \(x) x[x$event_type == et, ][["upper_ci"]]))
    args <- list(...)
    defaults <- list(
      xlab = "x",
      ylab = "Cause specific hazard rate",
      main = paste("Event:", et),
      ylim = range(c(all_lower, all_upper), na.rm = TRUE),
      xlim = range(x_sub$lifetime, na.rm = TRUE)
    )
    plot_args <- utils::modifyList(defaults, args)
    do.call("plot",
            c(list(x = x_sub$lifetime, y = x_sub$hazard, type = "n"),
              plot_args))
    for (i in seq_along(ci_level)) {
      lvl_name <- as.character(ci_level[i])
      ci_data <- cis_list[[lvl_name]]
      ci_data <- ci_data[ci_data$event_type == et, ]
      polygon(
          x = c(ci_data[["lifetime"]], rev(ci_data[["lifetime"]])),
          y = c(ci_data[["lower_ci"]], rev(ci_data[["upper_ci"]])),
          col = grDevices::adjustcolor(col_ci, alpha.f = transparency[i]),
          border = NA
      )
    }
    lines(x_sub$lifetime, x_sub$hazard, col = col_line, lwd = 2)
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
  lower <- min(object$lifetime, na.rm = TRUE)
  upper <- max(object$lifetime, na.rm = TRUE)
  times <- seq.int(from = lower, to = upper, by = by)
  cols <- c("lifetime",
            "hazard",
            "se_log_hazard",
            "lower_ci",
            "upper_ci")
  object[object$lifetime %in% times, cols]
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
  lower <- min(object$lifetime, na.rm = TRUE)
  upper <- max(object$lifetime, na.rm = TRUE)
  times <- seq.int(from = lower, to = upper, by = by)
  cols <- c("event_type",
            "lifetime",
            "hazard",
            "se_log_hazard",
            "lower_ci",
            "upper_ci")
  df_list <- split(object, object$event_type)
  df_list <- lapply(df_list, function(df) {
    df[df$lifetime %in% times, cols]
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
  if (any(x$hazard == 1))
    warning("Not reporting CDF (and density) values for time points where the hazard rate equals 1.")
  y <- x[x$hazard < 1, ]
  y$cdf <- 1 - cumprod(1 - y$hazard)
  y$density <- y$cdf - c(0, y$cdf)[seq_along(y$cdf)]
  pmfvarcov <- build_pmfvar(y$hazard, y$se_log_hazard)
  cdfvarcov <- build_cdfvar(pmfvarcov)
  y$se_cdf <- sqrt(diag(cdfvarcov))
  y$se_dens <- sqrt(diag(pmfvarcov))
  out <- new_acdf(y[, c("lifetime", "cdf", "se_cdf", "density", "se_dens")])
}

##' @export
##' @rdname calc_cdf
calc_cdf.alife_multi <- function(x, ...) {
  df_list <- split(x, x$event_type)
  df_list_with_cdf <- lapply(df_list, function(df) {
    df <- df[df$hazard < 1, ]
    df$cdf <- 1 - cumprod(1 - df$hazard)
    df$density <- df$cdf - c(0, df$cdf)[seq_along(df$cdf)]
    pmfvarcov <- build_pmfvar(df$hazard, df$se_log_hazard)
    cdfvarcov <- build_cdfvar(pmfvarcov)
    df$se_cdf <- sqrt(diag(cdfvarcov))
    df$se_dens <- sqrt(diag(pmfvarcov))
    return(df)
  })
  out <- do.call(rbind, df_list_with_cdf)
  rownames(out) <- NULL
  out <- new_acdf(out[, c("event_type", "lifetime", "cdf",
                          "se_cdf", "density", "se_dens")])
  return(out)
}

##' Summary Method for an 'acdf' Object
##'
##' @param object An object of class `acdf`. Typically the output of the
##'   `estimate_hazard` function.
##' @param by an `integer` defining the periodicity of the summary.
##' @param ... Additional arguments passed to the base `print` function (e.g.,
##'   `digits`).
##'
##' @seealso [calc_cdf()]
##' @return A summary of the CDF function.
##' @export
summary.acdf <- function(object, by = 5, ...) {
  lower <- min(object$lifetime, na.rm = TRUE)
  upper <- max(object$lifetime, na.rm = TRUE)
  times <- seq.int(from = lower, to = upper, by = by)
  cols <- c("lifetime", "cdf", "density")
  object[object$lifetime %in% times, cols]
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
summary.acdf_multi <- function(object, by = 5, ...) {
  lower <- min(object$lifetime, na.rm = TRUE)
  upper <- max(object$lifetime, na.rm = TRUE)
  times <- seq.int(from = lower, to = upper, by = by)
  cols <- c("event_type", "lifetime", "cdf", "density")
  df_list <- split(object, object$event_type)
  df_list <- lapply(df_list, function(df) {
    df[df$lifetime %in% times, cols]
  })
  out <- do.call(rbind, df_list)
  rownames(out) <- NULL
  return(out)
}

##' Plot Method for an 'acdf' Object
##'
##' @param x An object of class `acdf`. Typically the output of the
##'   `estimate_hazard` function.
##' @param ci_level A numeric vector of confidence ci_level to plot (e.g.,
##'   \code{c(0.5, 0.95)}). Defaults to \code{0.95}.
##' @param col_ci The color for the confidence interval polygon.
##' @param col_line The color for the hazard rate line.
##' @param ... Additional arguments passed to the base `plot` function (e.g.,
##'   `main`, `xlab`, `ylab`, `ylim`).
##'
##' @importFrom graphics polygon lines points arrows
##' @seealso [calc_cdf()]
##' @return A plot of the CDF.
##' @export
plot.acdf <- function(x, ci_level = 0.95,
                      col_ci = "grey80",
                      col_line = 1,
                      ...) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(1, 2))
  
  ci_level <- sort(ci_level, decreasing = TRUE)
  cis_list <- multiple_cis.acdf(x, ci_level = ci_level)
  
  n_ci_level <- length(ci_level)
  transparency <- if (n_ci_level > 1) {
    seq(0.2, 0.8, length.out = n_ci_level)
  } else {
    1.0
  }
  
  args <- list(...)
  
  defaults_cdf <- list(
    xlab = "Lifetime",
    ylab = "CDF",
    main = "Cumulative Distribution",
    ylim = c(0, 1),
    xlim = range(x$lifetime, na.rm = TRUE)
  )
  plot_args_cdf <- utils::modifyList(defaults_cdf, args)
  
  do.call("plot", c(list(x = x$lifetime, y = x$cdf, type = "n"), plot_args_cdf))
  
  for (i in seq_along(ci_level)) {
    lvl_name <- as.character(ci_level[i])
    ci_data <- cis_list[[lvl_name]]
    xx <- ci_data[["lifetime"]]
    x_poly <- c(xx[1], rep(xx[-1], each = 2))
    yy_low <- ci_data[["cdf_lower"]]
    y_low_poly <- c(rep(yy_low[-length(yy_low)], each = 2), yy_low[length(yy_low)])
    yy_up <- ci_data[["cdf_upper"]]
    y_up_poly <- c(rep(yy_up[-length(yy_up)], each = 2), yy_up[length(yy_up)])
    polygon(
      x = c(x_poly, rev(x_poly)),
      y = c(y_low_poly, rev(y_up_poly)),
      col = grDevices::adjustcolor(col_ci, alpha.f = if(n_ci_level > 1) 0.2 + (i*0.1) else 0.4),
      border = NA
    )
  }
  lines(x$lifetime, x$cdf, col = col_line, type = "s", lwd = 2)
  all_dens_upper <- unlist(lapply(cis_list, function(d) d$dens_upper))
  max_dens_y <- max(all_dens_upper, x$density, na.rm = TRUE)
  defaults_dens <- list(
    xlab = "Lifetime",
    ylab = "Probability Mass",
    main = "Density (PMF)",
    ylim = c(0, max_dens_y),
    xlim = range(x$lifetime, na.rm = TRUE)
  )
  plot_args_dens <- utils::modifyList(defaults_dens, args)
  do.call("plot", c(list(x = x$lifetime, y = x$density, type = "n"), plot_args_dens))
  ## Draw Error Bars
  for (i in seq_along(ci_level)) {
    lvl_name <- as.character(ci_level[i])
    ci_data <- cis_list[[lvl_name]]
    arrows(
      x0 = ci_data$lifetime, y0 = ci_data$dens_lower,
      x1 = ci_data$lifetime, y1 = ci_data$dens_upper,
      length = 0.05, angle = 90, code = 3,
      col = grDevices::adjustcolor(col_ci, alpha.f = transparency[i]),
      lwd = 1.5
    )
  }
  points(x$lifetime, x$density, col = col_line, pch = 19)
}


##' Plot Method for an 'acdf_multi' Object
##'
##' Creates a faceted plot, with one panel per event type.
##'
##' @param x An object of class `acdf_multi`.
##' @param ... Additional arguments passed to the base `plot` function (e.g.,
##'   `xlab`, `ylab`, `ylim`).
##'
##' @importFrom graphics polygon lines par
##' @seealso [calc_cdf()]
##' @return A faceted plot of the CDFs.
##' @export
##'
plot.acdf_multi <- function(x, ...) {
  etypes <- unique(x$event_type)
  n_types <- length(etypes)
  n_cols <- ceiling(sqrt(n_types))
  n_rows <- ceiling(n_types / n_cols)
  old_par <- par(no.readonly = TRUE) 
  on.exit(par(old_par))
  par(mfrow = c(n_rows, n_cols), 
      mar = c(4, 4, 2, 1))
  for (et in etypes) {
    x_sub <- x[x$event_type == et, ]    
    args <- list(...)
    defaults <- list(
      xlab = "x",
      ylab = "Cause specific CDF",
      main = paste("Event:", et),
      ylim = c(0, 1),
      xlim = range(x_sub$lifetime, na.rm = TRUE)
    )
    plot_args <- utils::modifyList(defaults, args)
    do.call("plot",
            c(list(x = x_sub$lifetime, y = x_sub$cdf, type = "s"),
              plot_args))
  }
}

##' Plot Method for an 'acdf_multi' Object
##'
##' Creates a faceted plot with one row per event type. Each row contains
##' two panels: one for the Cumulative Distribution Function (CDF) and one
##' for the Probability Mass Function (Density).
##'
##' @param x An object of class `acdf_multi`.
##' @param ci_level A numeric vector of confidence levels to plot (e.g.,
##'   \code{c(0.5, 0.95)}). Defaults to \code{0.95}.
##' @param col_ci The color for the confidence interval polygon/bars.
##' @param col_line The color for the main estimate line/points.
##' @param ... Additional arguments passed to the base `plot` function (e.g.,
##'   `xlab`, `ylab` override).
##'
##' @importFrom graphics polygon lines par points arrows
##' @importFrom grDevices adjustcolor
##' @seealso [calc_cdf()]
##' @return A faceted plot of CDFs and Densities.
##' @export
plot.acdf_multi <- function(x, ci_level = 0.95,
                            col_ci = "grey80",
                            col_line = 1,
                            ...) {
  
  etypes <- unique(x$event_type)
  n_types <- length(etypes)
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfrow = c(n_types, 2), 
      mar = c(4, 4, 2, 1), 
      oma = c(0, 0, 2, 0))
  
  ci_level <- sort(ci_level, decreasing = TRUE)
  n_ci_level <- length(ci_level)
  transparency <- if (n_ci_level > 1) {
    seq(0.2, 0.8, length.out = n_ci_level)
  } else {
    1.0
  }
  
  args <- list(...)
  
  for (et in etypes) {
    x_sub <- x[x$event_type == et, ]
    cis_list <- multiple_cis.acdf(x_sub, ci_level = ci_level)
    
    defaults_cdf <- list(
      xlab = "Lifetime",
      ylab = "Cause Specific CDF",
      main = paste("CDF - Event:", et),
      ylim = c(0, 1),
      xlim = range(x_sub$lifetime, na.rm = TRUE)
    )
    
    plot_args_cdf <- utils::modifyList(defaults_cdf, args)
    
    do.call("plot", c(list(x = x_sub$lifetime, y = x_sub$cdf, type = "n"),
                      plot_args_cdf))
    
    for (i in seq_along(ci_level)) {
      lvl_name <- as.character(ci_level[i])
      ci_data <- cis_list[[lvl_name]]
      
      xx <- ci_data[["lifetime"]]
      x_poly <- c(xx[1], rep(xx[-1], each = 2))
      
      yy_low <- ci_data[["cdf_lower"]]
      y_low_poly <- c(rep(yy_low[-length(yy_low)], each = 2),
                      yy_low[length(yy_low)])
      
      yy_up <- ci_data[["cdf_upper"]]
      y_up_poly <- c(rep(yy_up[-length(yy_up)], each = 2),
                     yy_up[length(yy_up)])
      
      polygon(
        x = c(x_poly, rev(x_poly)),
        y = c(y_low_poly, rev(y_up_poly)),
        col = grDevices::adjustcolor(col_ci,
                                     alpha.f =
                                       if(n_ci_level > 1) 0.2 + (i * 0.1) else 0.4),
        border = NA
      )
    }

    lines(x_sub$lifetime, x_sub$cdf, col = col_line, type = "s", lwd = 2)

    all_dens_upper <- unlist(lapply(cis_list, function(d) d$dens_upper))
    max_dens_y <- max(all_dens_upper, x_sub$density, na.rm = TRUE)
    
    defaults_dens <- list(
      xlab = "Lifetime",
      ylab = "Probability Mass",
      main = paste("Density - Event:", et),
      ylim = c(0, max_dens_y),
      xlim = range(x_sub$lifetime, na.rm = TRUE)
    )
    
    plot_args_dens <- utils::modifyList(defaults_dens, args)
    
    do.call("plot", c(list(x = x_sub$lifetime, y = x_sub$density, type = "n"),
                      plot_args_dens))
    
    for (i in seq_along(ci_level)) {
      lvl_name <- as.character(ci_level[i])
      ci_data <- cis_list[[lvl_name]]
      arrows(
        x0 = ci_data$lifetime, y0 = ci_data$dens_lower,
        x1 = ci_data$lifetime, y1 = ci_data$dens_upper,
        length = 0.05, angle = 90, code = 3,
        col = grDevices::adjustcolor(col_ci, alpha.f = transparency[i]),
        lwd = 1.5
      )
    }
    points(x_sub$lifetime, x_sub$density, col = col_line, pch = 19)
  }
}
