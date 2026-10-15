# Calculate Observed Support

This helper function generates the observable support of the lifetime
random variable based on the sample data (Lautier et al., 2023). In
particular, we assign \\\Delta\\ as the smallest observed time,
`min(c(lifetime, trunc_time))`, and \\\omega\\ as the largest observed
lifetime, `max(lifetime)`. The output is a sequence ranging from
\\\Delta + 1\\ to \\\xi\\ (see
[`retrieve_support()`](http://lcgodoy.me/abslife/reference/retrieve_support.md)).

## Usage

``` r
calc_osup(lifetime, trunc_time)
```

## Arguments

- lifetime:

  The vector of observed event and/or right-censoring times.

- trunc_time:

  The vector of observed left-truncation times.

## Value

A numeric vector representing the observed support of the lifetime
variable. An empty vector (with a warning) if fewer than two time points
are available.

## References

Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
contingent cash-flows: A discrete-time survival analysis approach.
*Insurance: Mathematics and Economics*, 110, 53-71.
<https://doi.org/10.1016/j.insmatheco.2023.02.003>
