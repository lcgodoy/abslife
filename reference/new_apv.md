# Create an apv Object

Constructs an object of class `apv` from a data frame, after validating
its structure. Note that this function is mostly for internal usage.

## Usage

``` r
new_apv(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an `apv` object:
  `APV` and `SD`. Defaults to an empty `data.frame`.

## Value

An object of class `apv`.
