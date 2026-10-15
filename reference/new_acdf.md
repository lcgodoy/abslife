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

## Examples

``` r
# Create a minimal data frame with the required columns
df <- data.frame(lifetime = 1:2, cdf = c(0.1, 1),
                 density = c(0.1, 0.9))

# Construct the acdf object
my_acdf_obj <- new_acdf(df)
class(my_acdf_obj)
#> [1] "acdf"       "data.frame"
```
