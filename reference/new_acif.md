# Create an acif Object

Constructs an object of class `acif` from a data frame, after validating
its structure. Note that this function is mostly for internal usage.

## Usage

``` r
new_acif(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an `acif` object:
  `lifetime`, `event_type`, `all_surv`, and `cif`. Defaults to an empty
  `data.frame`.

## Value

An object of class `acif`.
