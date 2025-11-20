# \\\hat{f}(x)\\

\\\hat{f}(x)\\

## Usage

``` r
f_hat(t, time_to_event, event, censoring)
```

## Arguments

- t:

  A time point at which hazard estimates are sought.

- time_to_event:

  A numeric vector representing the observed time to event.

- event:

  event indicator

- censoring:

  An indicator for censoring (1=censored, 0=not). Defaults to a vector
  of 0s if `NULL`. An observation is only treated as an event if
  status=1 AND censoring=0.

## Value

a scalar

## Author

lcgodoy
