##' Validate an alife Object
##'
##' Checks if an object is a valid `alife` object by ensuring it is a data
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
##' @export
##' @examples
##' # Create a minimal data frame with the required columns
##' df <- data.frame(lifetime = 1:2, risk_set = c(.8, .20),
##'                  hazard = c(0.1, 0.125), se_log_hazard = c(0.1, 0.1),
##'                  lower_ci = c(0.08, 0.1), upper_ci = c(0.12, 0.15))
##'
##' # Construct the alife object
##' my_alife_obj <- new_alife(df)
##' class(my_alife_obj)
##'
new_alife <- function(x = data.frame()) {
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
##' @export
##' @examples
##' # Create a minimal data frame with the required columns
##' df <- data.frame(lifetime = 1:2, cdf = c(0.1, 1),
##'                  density = c(0.1, 0.9))
##'
##' # Construct the acdf object
##' my_acdf_obj <- new_acdf(df)
##' class(my_acdf_obj)
##'
new_acdf <- function(x = data.frame()) {
  validate_acdf(x)
  new_class <- c("acdf", class(x))
  structure(x, class = new_class)
}

##' Create an acdf_multi Object
##'
##' Competing-risks counterpart of [new_acdf()]. Internal use.
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
##' @export
new_acif <- function(x = data.frame()) {
  validate_acif(x)
  structure(x, class = c("acif", class(x)))
}
