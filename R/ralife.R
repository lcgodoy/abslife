##' @title Sample lifetimes from an estimated distribution
##'
##' @description Draws random lifetimes from the distribution estimated by
##'   [estimate_hazard()], [calc_cdf()], or [calc_cif()]. For competing risks,
##'   each draw also has an event type.
##'
##' @details For `alife` and `acdf` objects, the function uses the inverse of
##'   the estimated CDF. If the estimated CDF at the last lifetime is less than
##'   1, the last lifetime receives the remaining probability.
##'
##'   For `alife_multi`, `acdf_multi`, and `acif` objects, the function samples
##'   rows of the cumulative incidence table. The sampling probabilities are
##'   `density` (`acdf_multi`) or `pr_zx` (`acif`). If these probabilities sum
##'   to less than 1, the function rescales them so that they sum to 1.
##'
##'   An `alife` object goes through [calc_cdf()] first. An `alife_multi`
##'   object goes through [calc_cif()] first.
##'
##' @param n number of samples.
##' @param x an object of class `alife`, `alife_multi`, `acdf`, `acdf_multi`,
##'   or `acif`. Typically the output of [estimate_hazard()], [calc_cdf()], or
##'   [calc_cif()].
##' @param ... not used.
##'
##' @return For `alife` and `acdf` objects, a vector of `n` sampled lifetimes.
##'   For `alife_multi`, `acdf_multi`, and `acif` objects, a `data.frame` with
##'   `n` rows and the columns `event_type` and `lifetime`.
##' @seealso [estimate_hazard()], [calc_cdf()], [calc_cif()]
##' @author lcgodoy
##' @examples
##' aart_hazard <- estimate_hazard(lifetime = aart$Xi,
##'                                trunc_time = aart$Yi,
##'                                carry_hazard = TRUE)
##' set.seed(1)
##' ralife(n = 10, x = aart_hazard)
##' ralife(n = 10, x = calc_cdf(aart_hazard))
##' @name ralife
##' @export
ralife <- function(n, x, ...) {
  UseMethod("ralife", x)
}

##' @rdname ralife
##' @export
ralife.acdf <- function(n, x, ...) {
  u <- stats::runif(n)
  indices <- pmin(findInterval(u, x$cdf) + 1, NROW(x))
  out <- x$lifetime[indices]
  return(out)
}

##' @rdname ralife
##' @export
ralife.acdf_multi <- function(n, x, ...) {
  .ralife_rows(n, x, prob = x$density)
}

##' @rdname ralife
##' @export
ralife.acif <- function(n, x, ...) {
  .ralife_rows(n, x, prob = x$pr_zx)
}

##' @rdname ralife
##' @export
ralife.alife <- function(n, x, ...) {
  ralife(n, calc_cdf(x))
}

##' @rdname ralife
##' @export
ralife.alife_multi <- function(n, x, ...) {
  ralife(n, calc_cif(x))
}

##' @rdname ralife
##' @export
ralife.default <- function(n, x, ...) {
  stop(sprintf(paste("ralife() does not support objects of class '%s'.",
                     "Use the output of estimate_hazard(), calc_cdf(),",
                     "or calc_cif()."),
               class(x)[1]))
}

## Samples rows of a competing-risks table (`acdf_multi` or `acif`) with
## probabilities `prob`. `sample()` rescales `prob` to sum to 1.
##' @noRd
.ralife_rows <- function(n, x, prob) {
  id_samples <- sample(x = seq_len(NROW(x)), size = n, prob = prob,
                       replace = TRUE)
  out <- data.frame(event_type = x$event_type[id_samples],
                    lifetime   = x$lifetime[id_samples])
  return(out)
}
