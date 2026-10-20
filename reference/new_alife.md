# Create an alife Object

Constructs an object of class `alife` from a data frame, after
validating its structure. Note that this function is mostly for internal
usage.

## Usage

``` r
new_alife(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an `alife` object:
  `lifetime`, `risk_set`, `hazard`, `se_log_hazard`, `lower_ci`, and
  `upper_ci`. Defaults to an empty `data.frame`.

## Value

An object of class `alife`. If `x` has an `event_type` column, the
object also inherits from `alife_multi`.
