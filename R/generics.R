##' Validate an alife Object
##'
##' Checks if an object is a valid `alife` object by ensuring it is a data
##' frame and contains all required columns.
##'
##' @param x An object to validate.
##'
##' @return The input `x`, invisibly, if validation is successful. Throws an
##'   error on failure.
##' @keywords internal
validate_alife <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c(## "event_type",
               "lifetime", "fh", "uh",
               "hazard", "se_log_hazard",
               "lower_ci", "upper_ci")
  stopifnot(all(my_cols %in% colnames(x)))
}

##' Create an alife Object
##'
##' Constructs an object of class `alife` from a data frame, after validating
##' its structure. Note that, this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an 'alife' object.
##'   Defaults to an empty `data.frame`.
##'
##' @return An object of class `alife`.
##' @export
##' @examples
##' # Create a minimal data frame with the required columns
##' df <- data.frame(lifetime = 1:2, fh = c(.2, .01), uh = c(.8, .20),
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
##' @return The input `x`, invisibly, if validation is successful. Throws an
##'   error on failure.
##' @keywords internal
validate_acdf <- function(x) {
  stopifnot(is.data.frame(x))
  my_cols <- c("lifetime", "cdf")
  stopifnot(all(my_cols %in% colnames(x)))
}

##' Create an acdf Object
##'
##' Constructs an object of class `acdf` from a data frame, after validating
##' its structure. Note that, this function is mostly for internal usage.
##'
##' @param x A `data.frame` that has the required columns for an 'alife' object.
##'   Defaults to an empty `data.frame`.
##'
##' @return An object of class `acdf`.
##' @export
##' @examples
##' # Create a minimal data frame with the required columns
##' df <- data.frame(lifetime = 1:2, fh = c(.2, .01), uh = c(.8, .20),
##'                  hazard = c(0.1, 0.125), se_log_hazard = c(0.1, 0.1),
##'                  lower_ci = c(0.08, 0.1), upper_ci = c(0.12, 0.15))
##'
##' # Construct the alife object
##' my_alife_obj <- new_alife(df)
##' class(my_alife_obj)
##'
new_acdf <- function(x = data.frame()) {
  validate_acdf(x)
  new_class <- c("acdf", class(x))
  if ("event_type" %in% colnames(x))
    new_class <- c("acdf_multi", new_class)
  structure(x, class = new_class)
}
