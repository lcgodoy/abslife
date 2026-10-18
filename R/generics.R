##' Validate an alife Object
##'
##' Checks if an object is a valid `alife` object by checking if it is a data
##' frame and contains all required columns.
##'
##' @param x An object to validate.
##'
##' @return `NULL`, invisibly, if validation is successful. Throws an error on
##'   failure.
##' @keywords internal
validate_alife <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c(## "event_type",
               "lifetime", "risk_set",
               "hazard", "se_log_hazard",
               "lower_ci", "upper_ci")
  stopifnot(all(my_cols %in% colnames(x)))
}

##' Create an alife Object
##'
##' Constructs an object of class `alife` from a data frame, after validating
##' its structure. Note that this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an `alife`
##'   object: `lifetime`, `risk_set`, `hazard`, `se_log_hazard`, `lower_ci`,
##'   and `upper_ci`. Defaults to an empty `data.frame`.
##'
##' @return An object of class `alife`. If `x` has an `event_type` column, the
##'   object also inherits from `alife_multi`.
##' @keywords internal
new_alife <-function(x = data.frame()) {
  validate_alife(x)
  new_class <- c("alife", class(x))
  if ("event_type" %in% colnames(x))
    new_class <- c("alife_multi", new_class)
  structure(x, class = new_class)
}

##' Validate an acdf Object
##'
##' Checks if an object is a valid `acdf` object by ensuring it is a data
##' frame and contains all required columns.
##'
##' @param x An object to validate.
##'
##' @return `NULL`, invisibly, if validation is successful. Throws an error on
##'   failure.
##' @keywords internal
validate_acdf <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c("lifetime", "density")
  stopifnot(all(my_cols %in% colnames(x)))
}

##' Create an acdf Object
##'
##' Constructs an object of class `acdf` from a data frame, after validating
##' its structure. Note that this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an `acdf`
##'   object: `lifetime` and `density`. Defaults to an empty `data.frame`.
##'
##' @return An object of class `acdf`.
##' @keywords internal
new_acdf <-function(x = data.frame()) {
  validate_acdf(x)
  new_class <- c("acdf", class(x))
  structure(x, class = new_class)
}

##' Create an acdf_multi Object
##'
##' Competing-risks counterpart of \code{new_acdf()}. Internal use.
##'
##' @inheritParams new_acdf
##'
##' @return An object of class `acdf_multi`.
##' @keywords internal
new_acdf_multi <- function(x = data.frame()) {
  validate_acdf(x)
  new_class <- c("acdf_multi", class(x))
  structure(x, class = new_class)
}

##' Validate an acif Object
##'
##' Checks if an object is a valid `acif` object by ensuring it is a data
##' frame and contains all required columns.
##'
##' @param x An object to validate.
##'
##' @return `NULL`, invisibly, if validation is successful. Throws an error on
##'   failure.
##' @keywords internal
validate_acif <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c("lifetime", "event_type", "all_surv", "cif")
  stopifnot(all(my_cols %in% colnames(x)))
}

##' Create an acif Object
##'
##' Constructs an object of class `acif` from a data frame, after validating
##' its structure. Note that this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an `acif`
##'   object: `lifetime`, `event_type`, `all_surv`, and `cif`. Defaults to an
##'   empty `data.frame`.
##'
##' @return An object of class `acif`.
##' @keywords internal
new_acif <-function(x = data.frame()) {
  validate_acif(x)
  structure(x, class = c("acif", class(x)))
}

##' Validate an apv Object
##'
##' Checks if an object is a valid `apv` object by checking if it is a data
##' frame and contains all required columns.
##'
##' @param x An object to validate.
##'
##' @return `NULL`, invisibly, if validation is successful. Throws an error on
##'   failure.
##' @keywords internal
validate_apv <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c("APV", "SD")
  stopifnot(all(my_cols %in% colnames(x)))
}

##' Create an apv Object
##'
##' Constructs an object of class `apv` from a data frame, after validating
##' its structure. Note that this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an `apv`
##'   object: `APV` and `SD`. Defaults to an empty `data.frame`.
##'
##' @return An object of class `apv`.
##' @keywords internal
new_apv <- function(x = data.frame()) {
  validate_apv(x)
  structure(x, class = c("apv", class(x)))
}

##' Validate an irr Object
##'
##' Checks if an object is a valid `irr` object by checking if it is a data
##' frame and contains all required columns.
##'
##' @param x An object to validate.
##'
##' @return `NULL`, invisibly, if validation is successful. Throws an error on
##'   failure.
##' @keywords internal
validate_irr <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c("monthly_rate", "annualized_rate", "objective", "converged")
  stopifnot(all(my_cols %in% colnames(x)))
  stopifnot(is.logical(x$converged))
}

##' Create an irr Object
##'
##' Constructs an object of class `irr` from a data frame, after validating
##' its structure. Note that this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an `irr`
##'   object: `monthly_rate`, `annualized_rate`, `objective`, and `converged`.
##'   Defaults to an empty `data.frame`.
##'
##' @return An object of class `irr`.
##' @keywords internal
new_irr <- function(x = data.frame()) {
  validate_irr(x)
  structure(x, class = c("irr", class(x)))
}
