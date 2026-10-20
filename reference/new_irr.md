# Create an irr Object

Constructs an object of class `irr` from a data frame, after validating
its structure. Note that this function is mostly for internal usage.

## Usage

``` r
new_irr(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an `irr` object:
  `monthly_rate`, `annualized_rate`, `objective`, and `converged`.
  Defaults to an empty `data.frame`.

## Value

An object of class `irr`.
