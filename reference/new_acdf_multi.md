# Create an acdf_multi Object

Competing-risks counterpart of
[`new_acdf()`](http://lcgodoy.me/abslife/reference/new_acdf.md).
Internal use.

## Usage

``` r
new_acdf_multi(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an `acdf` object:
  `lifetime` and `density`. Defaults to an empty `data.frame`.

## Value

An object of class `acdf_multi`.
