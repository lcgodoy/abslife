##' @title Prepare alife object for ggplot
##'
##' @description Internal helper to create a long-form data frame with multiple
##'   confidence intervals for plotting.
##'
##' @param x An object of class 'alife' or 'alife-multi'
##' @param ci_level A numeric vector of confidence levels (e.g., c(0.80, 0.95))
##' @return A long-form data.frame with CI columns.
##' @noRd
.prep_ggplot <- function(x, ci_level = 0.95) {
  sorted_levels <- rev(sort(ci_level))
  ci_list <- lapply(sorted_levels, function(lvl) {
    upper_tail <- 1 - .5 * (1 - lvl)
    z <- stats::qnorm(upper_tail)
    df <- x
    df$level <- factor(paste0(lvl * 100, "%"))
    log_haz <- log(df$hazard)
    df$lower_ci <- exp(log_haz - z * df$se_log_hazard)
    df$upper_ci <- exp(log_haz + z * df$se_log_hazard)
    df$lower_ci[df$hazard == 0] <- NA_real_
    df$upper_ci[df$hazard == 0] <- NA_real_
    return(df)
  })
  long_df <- do.call(rbind, ci_list)
  level_order <- paste0(sorted_levels * 100, "%")
  long_df$level <- factor(long_df$level, levels = level_order)  
  return(long_df)
}

##' @title Plot an 'alife' (or `multi_alife`) Object using `ggplot2`
##'
##' @param object An object of class `alife` or `alife-multi`.
##' @param ... Not used.
##'
##' @return ggplot.
##' @export
ggauto <- function(object, ...) {
  UseMethod("ggauto")
}


##' @title Plot an 'alife' Object using ggplot2
##'
##' @param object An object of class `alife`.
##' @param ci_level A single number or numeric vector of confidence levels 
##'   to display (e.g., `0.95` or `c(0.80, 0.95)`).
##' @param ... Additional arguments (not used).
##'
##' @return A `ggplot` object.
##' @importFrom rlang .data
##' @export
ggauto.alife <- function(object, ci_level = 0.95, ...) {  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for this plot. ",
      "Please install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )
  }
  plot_data <- .prep_ggplot(object, ci_level)
  p <- ggplot2::ggplot(
    plot_data,
    # Use .data$column_name inside aes()
    ggplot2::aes(x = .data$lifetime, y = .data$hazard)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$lower_ci,
        ymax = .data$upper_ci,
        fill = .data$level,
        group = .data$level
      ),
      alpha = 0.4
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_fill_brewer(type = "seq") +
    ggplot2::labs(
      ## title = "Hazard rate",
      x = "x",
      y = "Hazard rate",
      fill = "CI Level"
    )
  return(p)
}

##' @title Plot an 'alife-multi' Object using `ggplot2`
##'
##' @description Creates a faceted plot, with one panel per event type.
##'
##' @param object An object of class `alife-multi`.
##' @param ci_level A single number or numeric vector of confidence levels 
##'   to display (e.g., `0.95` or `c(0.80, 0.95)`).
##' @param ... Additional arguments (not used).
##'
##' @importFrom rlang .data
##' @return A `ggplot` object.
##' @export
ggauto.alife_multi <- function(object, ci_level = 0.95, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for this plot. ",
      "Please install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )
  }
  plot_data <- .prep_ggplot(object, ci_level)
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = .data$lifetime,
                                    y = .data$hazard)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_ci,
                   ymax = .data$upper_ci,
                   fill = .data$level,
                   group = .data$level),
      alpha = 0.4
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_fill_brewer(type = "seq") +
    ggplot2::labs(
      ## title = "Hazard Rate Estimate by Event Type",
      x = "x",
      y = "Event_Type specific hazard",
      fill = "CI Level"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$event_type),
                        labeller = ggplot2::label_both) +
  ggplot2::theme_minimal()
  return(p)
}
