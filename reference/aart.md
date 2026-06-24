# Consumer automobile loans from Ally Auto Receivables Trust

A dataset containing time-to-event and left-truncation times for
consumer automobile loans from the Ally Auto Receivables Trust (AART).
The data were compiled from the the Electronic Data Gathering, Analysis,
and Retrieval (EDGAR) system of the Securities and Exchange Comission
(SEC).

## Usage

``` r
aart
```

## Format

### `aart`

A data frame with 2,756 rows and 12 columns:

- `Xi`:

  Time-to-event, representing the loan termination age in months.

- `Yi`:

  Left-truncation time, representing the loan age in months when the ABS
  trust began making payments to investors.

- `credit.score`:

  Obligor credit score

- `interest.rate`:

  Obligor annual percentage rate

- `pti`:

  Obligor payment-to-income

- `veh.value`:

  Estimated vehicle value at signing (\\\\log\\ scale)

- `co.sign`:

  Co-Obligor indicator (1 = `TRUE`)

- `new.used`:

  New-Used indicator (1 = `NEW`)

- `subvent.rate`:

  Subvented interest rate indicator (1 = `TRUE`)

- `subvent.cash`:

  Cash rebate indicator (1 = `TRUE`)\\

- `veh.pick.up`:

  Pick-up truck indicator (1 = `TRUE`)

- `veh.suv`:

  Sport-Utility-Vehicle indicator (1 = `TRUE`)

## Source

Data was compiled from the Electronic Data Gathering, Analysis, and
Retrieval (EDGAR) system of the Securities and Exchange Commission
(SEC). The replication data repository is available at
<https://github.com/jackson-lautier/consumer-auto-abs-parametric>.

## References

Lautier, J. P., Pozdnyakov, V., & Yan, J. (2025). Estimating the
time-to-event distribution for loan-level data within a consumer auto
loan asset-backed security. *The Annals of Applied Statistics*.
[doi:10.1214/25-AOAS2103](https://doi.org/10.1214/25-AOAS2103) .
