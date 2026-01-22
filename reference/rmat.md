# Build the auxiliary matrix K

Build the auxiliary matrix K

## Usage

``` r
aux_kmat(hazard, rephaz, support_length)

build_kmat(hazard)

build_rmat(hazard)

build_amat(support_length)

build_sigmat(hazard, se_log_hazard)

build_pmfvar(hazard, se_log_hazard)

build_cdfvar(pmfvar)
```

## Arguments

- hazard:

  A `vector` of estimated hazards at every timepoint.

- rephaz:

  number of times `hazard` must be repeated (auxiliary)

- support_length:

  length of `hazard`.

- se_log_hazard:

  A `vector` of SE estimates for the log-hazards.

- pmfvar:

  a square `matrix` corresponding to the output of a `build_pmf` call.

## Value

A square `matrix` with number of rows (and columns) matching the
dimension of `hazard`.

## Author

lcgodoy
