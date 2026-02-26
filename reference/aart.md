# Consumer automobile loans from Ally Auto Receivables Trust

A dataset containing time-to-event and left-truncation times for
consumer automobile loans from the Ally Auto Receivables Trust (AART).
The data were compiled from the SEC's EDGAR system.

## Usage

``` r
aart
```

## Format

### `aart`

A data frame with 1,553 rows and 2 columns:

- `Xi`:

  Time-to-event, representing the loan termination age in months.

- `Yi`:

  Left-truncation time, representing the loan age in months when the ABS
  trust began making payments to investors.

## Source

Data was compiled from the SEC's Electronic Data Gathering, Analysis,
and Retrieval (EDGAR) system. The replication data repository is
available at
<https://github.com/jackson-lautier/consumer-auto-abs-parametric>.

## References

Lautier, J. P., Pozdnyakov, V., & Yan, J. (2025). Estimating the
time-to-event distribution for loan-level data within a consumer auto
loan asset-backed security. *The Annals of Applied Statistics*.
[doi:10.1214/25-AOAS2103](https://doi.org/10.1214/25-AOAS2103) .
