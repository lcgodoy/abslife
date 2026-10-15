# Calculate Observed Support

This helper function generates the observable support of the lifetime
variable based on the study's overall time range (Lautier et al. 2023,
[doi:10.1016/j.ecosta.2023.05.005](https://doi.org/10.1016/j.ecosta.2023.05.005)
). In particular, it takes \\\Delta\\ as the smallest observed time,
`min(c(lifetime, trunc_time))`, and \\\omega\\ as the largest observed
lifetime, `max(lifetime)`, and outputs a sequence ranging from
\\\Delta + 1\\ to \\\omega\\ (see
[`retrieve_support()`](http://lcgodoy.me/abslife/reference/retrieve_support.md)).

## Usage

``` r
calc_osup(lifetime, trunc_time)
```

## Arguments

- lifetime:

  The vector of event or censoring times.

- trunc_time:

  The vector of left-truncation times.

## Value

A numeric vector representing the observed support of the lifetime
variable. An empty vector (with a warning) if fewer than two time points
are available.
