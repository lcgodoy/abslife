# Create an acdf Object

Constructs an object of class `acdf` from a data frame, after validating
its structure. Note that this function is mostly for internal usage.

## Usage

``` r
new_acdf(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an `acdf` object:
  `lifetime` and `density`. Defaults to an empty `data.frame`.

## Value

An object of class `acdf`.
