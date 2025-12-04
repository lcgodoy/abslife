# Create an acdf Object

Constructs an object of class `acdf` from a data frame, after validating
its structure. Note that, this function is mostly for internal usage.

## Usage

``` r
new_acdf(x = data.frame())
```

## Arguments

- x:

  A `data.frame` that has the required columns for an 'alife' object.
  Defaults to an empty `data.frame`.

## Value

An object of class `acdf`.

## Examples

``` r
# Create a minimal data frame with the required columns
df <- data.frame(lifetime = 1:2, risk_set = c(.8, .20),
                 hazard = c(0.1, 0.125), se_log_hazard = c(0.1, 0.1),
                 lower_ci = c(0.08, 0.1), upper_ci = c(0.12, 0.15))

# Construct the alife object
my_alife_obj <- new_alife(df)
class(my_alife_obj)
#> [1] "alife"      "data.frame"
```
