# Consumer automobile leases from Mercedes-Benz Auto Lease Trust 2017-A

A dataset containing performance records of 47,315 consumer automobile
leases with an original term of 36 months from the Mercedes-Benz Auto
Lease Trust (MBALT) 2017-A.

## Usage

``` r
mbalt
```

## Format

### `mbalt`

A data frame with 47,314 rows and 15 columns:

- `Zi`:

  Time-to-event, representing the observed lease termination age in
  months.

- `Yi`:

  Left-truncation time, representing the lease age in months when the
  ABS trust began making payments to investors.

- `Di`:

  Event indicator (1 = exact termination observed, 0 = right-censored).

- `base.resid`:

  Securitized residual value.

- `contract.resid`:

  Lessee purchase price option value.

- `co.lessee`:

  Co-Lessee indicator

- `credit.score`:

  Obligor credit score

- `location`:

  State or U.S.\\ territory of contract

- `lease.term`:

  Original lease term

- `pti`:

  Obligor payment-to-income

- `subvented`:

  Subvention type (cash, rate, none)

- `model`:

  Vehicle model

- `year`:

  Vehicle year

- `manufacturer`:

  Vehicle manufacturer

- `veh.value`:

  Estimated vehicle value at signing

## Source

Data was compiled from the Electronic Data Gathering, Analysis, and
Retrieval (EDGAR) system of the Securities and Exchange Commission
(SEC).

## References

Lautier, J. P., Pozdnyakov, V., & Yan, J. (2023). Estimating a discrete
distribution subject to random left-truncation with an application to
structured finance. *Econometrics and Statistics*.
[doi:10.1016/j.ecosta.2023.05.005](https://doi.org/10.1016/j.ecosta.2023.05.005)
.

Lautier, J. P., Pozdnyakov, V., & Yan, J. (2023). Pricing time-to-event
contingent cash flows: A discrete-time survival analysis approach.
*Insurance: Mathematics and Economics*, 110, 53-71.
[doi:10.1016/j.insmatheco.2023.02.003](https://doi.org/10.1016/j.insmatheco.2023.02.003)
.
