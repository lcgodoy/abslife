# Calculate Default Time Points

This helper function generates a default sequence of evaluation points
based on the study's overall time range (Lautier et al. 2023, \<DOI:
10.1016/j.ecosta.2023.05.005\>). In particular, it calculates \\\Delta\\
and \\m\\ based on left-truncation and time-to-event variables and
outputs a sequence ranging from \\\Delta + 1\\ to \\\omega\\.

## Usage

``` r
calc_tp(time_to_event, trunc_time)
```

## Arguments

- time_to_event:

  The vector of event or censoring times.

- trunc_time:

  The vector of left-truncation times.

## Value

A numeric vector of default time points to evaluate the hazard at.
