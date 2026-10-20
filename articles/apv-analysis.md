# Actuarial Present Value and IRR Analysis

``` r

library(abslife)
#> Welcome to the abslife package!
#> WARNING: Under construction.
```

This vignette provides a clear example of how to calculate the Actuarial
Present Value (APV) and risk-adjusted Internal Rate of Return (IRR) for
a loan portfolio subject to competing risks (Default and Pre-payment).

We use the built-in `aloans` dataset from the `abslife` package, which
contains left-truncated and right-censored loan performance records.

## Step 1: Hazard Estimation with Competing Risks

First, we estimate the hazard rates for the competing risks (Default
vs. Pre-payment) conditioning on left-truncation. We focus on deep
subprime loans and specify `support_lifetime_rv` up to the loan term of
72 months to cover the entire potential duration.

``` r

data(aloans)

orig_term <- 72
cur_age <- 7

# Estimate hazards for the competing risk events
hazards <-
  with(subset(aloans, risk.cat == "deep_subprime"),
       estimate_hazard(lifetime = Z,
                       trunc_time = Y,
                       event_type = ifelse(D == 1, "Default", "Pre-payment"),
                       censoring = C,
                       ci_level = 0.95,
                       carry_hazard = TRUE))
#> Warning in check_censored(lifetime, censoring_indicator, support_lifetime_rv):
#> Warning: Detected censored observations at the maximum observed limit of the
#> support (lifetime == max(support_lifetime_rv)). This may lead to
#> identifiability issues or unstable hazard estimates at the tail.
```

## Step 2: Setup Recovery Curve

We define a recovery curve mapping the recovery percentage to each
remaining month of the loan’s term. In this example, we construct a
dummy recovery curve showing declining recovery over time.

``` r

rem_months <- cur_age:orig_term
recov_curve <- data.frame(
  month = rem_months,
  recovery = seq(0.40, 0.15, length.out = length(rem_months))
)
```

## Step 3: Calculate Actuarial Present Value (APV)

We set the loan parameters (original amount, monthly payment, and
reference market rate) and compute the APV:

``` r

orig_loan_amt <- 50000
mon_pmt <- 1031.84
ref_rate <- 0.05 # Annual market discount rate

apv_res <- calculate_apv(
    x = hazards,
    cur_age = cur_age,
    orig_term = orig_term,
    orig_loan_amt = orig_loan_amt,
    mon_pmt = mon_pmt,
    ref_rate = ref_rate,
    recov_curve = recov_curve,
    orig_apy = 0.15
)
#> Warning in calculate_apv(x = hazards, cur_age = cur_age, orig_term = orig_term,
#> : Lifetime observed support does not include the original loan term.
#> Extrapolating hazards.
```

- Estimated APV: \$43659.65 (13677.69)

## Step 4: Solve for Risk-Adjusted IRR

Assuming a current loan price/balance of `$37,719.47`, we calculate the
risk-adjusted monthly and annualized Internal Rate of Return (IRR) that
equates the loan price to its expected present value:

``` r

cur_bal <- 37719.47

irr_res <- solve_irr(
    x = hazards,
    abs0_bal = cur_bal,
    cur_age = cur_age,
    orig_term = orig_term,
    orig_loan_amt = orig_loan_amt,
    mon_pmt = mon_pmt,
    recov_curve = recov_curve,
    orig_apy = 0.15
)
#> Warning in solve_irr(x = hazards, abs0_bal = cur_bal, cur_age = cur_age, :
#> Lifetime observed support does not include the original loan term.
#> Extrapolating hazards.
```

- Calculated monthly rate: 0.012147 (1.2147%)
- Calculated annualized rate: 0.155909 (15.5909%)
