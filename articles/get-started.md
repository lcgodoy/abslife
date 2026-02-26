# Get Started

``` r
library(abslife)
#> Welcome to the abslife package!
#> WARNING: Under construction.
library(ggplot2) ## optional
```

> This package is currently under construction!

This vignette demonstrates the core functionalities of the `abslife`
package. `abslife` provides tools for estimation of the hazard rate,
denoted \\\lambda\\, for discrete time-to-event data subject to
**left-truncation** (Jackson P. Lautier, Pozdnyakov, and Yan 2023a). The
package is designed so it can additionally handle the following common
observational data challenges:

- **+ Right-censoring** (Jackson P. Lautier, Pozdnyakov, and Yan 2023b)

- **+ Competing risks** (Jackson P. Lautier, Pozdnyakov, and Yan 2024)

In all cases, asymptotic confidence intervals for the estimated hazard
rates are readily available. The main function of the package is the
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
function, which adapts to these scenarios based on the arguments
provided.

Now, we provide specific examples with datasets shipped with the
package.

### Left-truncation

We begin by analyzing the aart dataset. This dataset represents a
scenario involving only left-truncation. It contains two columns: `Xi`
(time-to-event) and `Yi` (truncation time).

``` r
data(aart)
head(aart)
#>   Xi Yi
#> 1 39 33
#> 2 39 33
#> 3 39 33
#> 4 34 33
#> 5 35 33
#> 6 39 33
```

We estimate the hazard rate using
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

``` r
aart_hazard <- estimate_hazard(lifetime = aart$Xi,
                               trunc_time = aart$Yi,
                               ci_level = 0.95,
                               carry_hazard = TRUE) ## need 
```

> Note that we set `carry_hazard = TRUE`. This argument ensures that if
> a hazard estimate is 0, it is replaced by the last non-zero estimate.

The function returns a `data.frame` containing the evaluation times, the
hazard estimates, and the standard errors (on the log scale). It also
includes the lower and upper bounds of the confidence interval
corresponding to the `ci_level` argument (in this case, set to 0.95).

``` r
tail(aart_hazard)
#> Observed lifetime support: [44, 49] 
#> Total number of timepoints observed: 6
```

#### Summarizing and plotting

For a concise overview of the estimation, use the
[`summary()`](https://rdrr.io/r/base/summary.html) method:

``` r
summary(aart_hazard)
#>    lifetime     hazard se_log_hazard    lower_ci   upper_ci
#> 1         5 0.01904762     0.7139389 0.004768819 0.07294634
#> 6        10 0.01556420     0.3563373 0.007802303 0.03080805
#> 11       15 0.02432778     0.2322583 0.015569867 0.03782269
#> 16       20 0.01724138     0.2604539 0.010420226 0.02839956
#> 21       25 0.02956705     0.1918396 0.020490707 0.04248937
#> 26       30 0.03311966     0.1826555 0.023386182 0.04671051
#> 31       35 0.05163330     0.1466945 0.039237648 0.06766907
#> 36       40 0.73134328     0.2756152 0.613311320 0.82370364
#> 41       45 0.50000000     1.4142136 0.058866787 0.94113321
```

To visualize the hazard rate, you can use base R graphics via the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method:

``` r
plot(aart_hazard, ci_level = c(.5, .75, .85, .9, .95))
```

![](get-started_files/figure-html/plot_aart-1.png)

Alternatively, use
[`ggauto()`](http://lcgodoy.me/abslife/reference/ggauto.md) for a
`ggplot2`-powered visualization. This function is particularly useful
for visualizing multiple confidence levels simultaneously:

``` r
ggauto(aart_hazard, ci_level = c(.5, .75, .85, .9, .95))
```

![](get-started_files/figure-html/ggplot_aart-1.png)

#### Calculating the CDF

The package can also derive the Cumulative Distribution Function (CDF)
directly from the hazard estimates using
[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md). This
appends a cdf column to the results.

``` r
aart_cdf <- calc_cdf(aart_hazard)
#> Warning in calc_cdf.alife(aart_hazard): Not reporting CDF (and density) values
#> for time points where the hazard rate equals 1.
summary(aart_cdf)
#>    lifetime        cdf      density
#> 1         5 0.01904762 0.0190476190
#> 6        10 0.10628735 0.0141298442
#> 11       15 0.19848679 0.0199852375
#> 16       20 0.28713123 0.0125064696
#> 21       25 0.37058116 0.0191770702
#> 26       30 0.45756023 0.0185808099
#> 31       35 0.54922672 0.0245421009
#> 36       40 0.99118211 0.0240042534
#> 41       45 0.99975506 0.0002449414
```

We can also plot the CDF:

``` r
plot(aart_cdf)
```

![](get-started_files/figure-html/plot_cdf_aart-1.png)

### Right-censoring

The workflow for right-censored data is nearly identical. We will
demonstrate this using the `mbalt` dataset, which includes a third
column, `Di`, serving as the right-censoring indicator.

``` r
data(mbalt)
head(mbalt)
#>   Zi Yi Di
#> 1 37 29  1
#> 2 30 29  1
#> 3 37 30  1
#> 4 32 32  1
#> 5 37 32  1
#> 6 36 33  1
```

We call
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
again, but this time we provide the censoring argument. This argument
accepts a vector where `1` indicates a censored observation and `0`
indicates an observed event.

``` r
mbalt_hazard <- estimate_hazard(lifetime = mbalt$Zi,
                                trunc_time = mbalt$Yi,
                                censoring = 1 - mbalt$Di,
                                ci_level = 0.95,
                                carry_hazard = FALSE) ## need 
```

The output structure remains consistent with the previous example.

``` r
summary(mbalt_hazard)
#>    lifetime      hazard se_log_hazard     lower_ci    upper_ci
#> 1         5 0.002508511    0.26759709 0.0014862156 0.004231014
#> 6        10 0.001434144    0.20014357 0.0009692424 0.002121564
#> 11       15 0.001737138    0.14599185 0.0013054299 0.002311282
#> 16       20 0.002970103    0.09727315 0.0024558192 0.003591697
#> 21       25 0.006408170    0.06270122 0.0056713262 0.007240052
#> 26       30 0.022951205    0.03238299 0.0215702141 0.024418405
#> 31       35 0.177075260    0.01673112 0.1723472557 0.181904461
```

The plotting functions also work out of the box:

``` r
plot(mbalt_hazard)
```

![](get-started_files/figure-html/plot_mbalt-1.png)

and, for `ggplot2`:

``` r
ggauto(mbalt_hazard, ci_level = c(.5, .75, .85, .9, .95))
```

![](get-started_files/figure-html/ggplot_mbalt-1.png)

### Competing risks

Finally, `abslife` can estimate hazards in the presence of competing
risks. We use the aloans dataset, which contains data on consumer
automobile loans (Jackson P. Lautier, Pozdnyakov, and Yan 2024).

``` r
data(aloans)
head(aloans)
#>     risk_cat  Z  Y C D R  bond
#> 1   subprime 42 19 0 0 1 sdart
#> 2   subprime 50 19 0 1 0 sdart
#> 3 near_prime 23 19 0 1 0 sdart
#> 4      prime 70 19 1 0 0 sdart
#> 5   subprime 21 19 0 1 0 sdart
#> 6      prime 70 19 0 0 1 sdart
```

The relevant columns of the dataset for this example are the following:

- `Z`: time to event

- `Y`: left-truncation

- `C`: right censoring indicator

- `D`: Default indicator (1 represents default, and 0 represents
  pre-payment)

The two competing risks in this case are default and pre-payment. To
make the results more interpretable, we create a descriptive event_type
column.

``` r
aloans <- transform(aloans, event_type = ifelse(D == 1, "Defaut", "Pre-payment"))
```

The package is designed so the workflow is identical as the previous two
examples. The only change here is passing the column discriminating the
event types to the `event_type argument` in
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

``` r
aloans_hazard <- estimate_hazard(lifetime = aloans$Z,
                                 trunc_time = aloans$Y,
                                 censoring = aloans$C,
                                 event_type = aloans$event_type,
                                 ci_level = 0.95,
                                 carry_hazard = FALSE) ## need 
#> Warning in check_censored(lifetime, censoring_indicator, support_lifetime_rv):
#> Warning: Detected censored observations at the maximum limit of the support
#> (lifetime == max(support_lifetime_rv)). This may lead to identifiability issues
#> or unstable hazard estimates at the tail.
```

The output now includes a column specifying the event type for each
hazard estimate.

``` r
summary(aloans_hazard)
#>     event_type lifetime      hazard se_log_hazard    lower_ci    upper_ci
#> 1       Defaut        3 0.011520293    0.06506047 0.010155080 0.013066617
#> 2       Defaut        8 0.012729470    0.03876609 0.011809112 0.013720562
#> 3       Defaut       13 0.010477761    0.04486786 0.009604176 0.011429888
#> 4       Defaut       18 0.013350256    0.04224199 0.012302520 0.014485913
#> 5       Defaut       23 0.012722853    0.04637351 0.011630319 0.013916572
#> 6       Defaut       28 0.013859145    0.04731310 0.012647271 0.015185355
#> 7       Defaut       33 0.013032176    0.05225905 0.011778374 0.014417499
#> 8       Defaut       38 0.018664205    0.04681281 0.017055857 0.020421067
#> 9       Defaut       43 0.011084154    0.06587830 0.009754619 0.012592597
#> 10      Defaut       48 0.007673623    0.08514621 0.006501842 0.009054661
#> 11      Defaut       53 0.006133755    0.10401466 0.005008188 0.007510380
#> 12      Defaut       58 0.006600660    0.50165838 0.002479550 0.017451386
#> 13      Defaut       63 0.000000000           Inf 0.000000000         NaN
#> 14      Defaut       68 0.000000000           Inf 0.000000000         NaN
#> 15 Pre-payment        3 0.000000000           Inf 0.000000000         NaN
#> 16 Pre-payment        8 0.007686787    0.04975978 0.006977496 0.008467566
#> 17 Pre-payment       13 0.011646595    0.04258207 0.010724035 0.012647505
#> 18 Pre-payment       18 0.015630141    0.03908499 0.014494211 0.016853574
#> 19 Pre-payment       23 0.011372231    0.04901653 0.010341306 0.012504631
#> 20 Pre-payment       28 0.015052316    0.04542667 0.013787747 0.016430935
#> 21 Pre-payment       33 0.015385696    0.04815372 0.014019446 0.016882814
#> 22 Pre-payment       38 0.013687084    0.05452748 0.012316859 0.015207396
#> 23 Pre-payment       43 0.018648019    0.05098525 0.016904597 0.020567482
#> 24 Pre-payment       48 0.017776306    0.05622984 0.015950891 0.019806416
#> 25 Pre-payment       53 0.032053819    0.04610587 0.029365598 0.034979259
#> 26 Pre-payment       58 0.037953795    0.21258769 0.025348580 0.056464133
#> 27 Pre-payment       63 0.020408163    1.01036297 0.002867406 0.131138926
#> 28 Pre-payment       68 0.076923077    1.04083300 0.010719623 0.390571292
```

The plotting functions work as in the previous examples, handling the
stratification by event type automatically:

``` r
plot(aloans_hazard, ci_level = c(.5, .75, .85, .9, .95))
```

![](get-started_files/figure-html/plot_aloans-1.png)

The same holds for the `ggplot2` powered plots:

``` r
ggauto(aloans_hazard, ci_level = c(.5, .75, .85, .9, .95)) +
  theme_bw()
```

![](get-started_files/figure-html/ggplot_aloans-1.png)

## References

Lautier, Jackson P., Vladimir Pozdnyakov, and Jun Yan. 2023a.
“Estimating a Discrete Distribution Subject to Random Left-Truncation
with an Application to Structured Finance.” *Econometrics and
Statistics*. <https://doi.org/10.1016/j.ecosta.2023.05.005>.

———. 2023b. “Pricing Time-to-Event Contingent Cash Flows: A
Discrete-Time Survival Analysis Approach.” *Insurance: Mathematics and
Economics* 110: 53–71.
<https://doi.org/10.1016/j.insmatheco.2023.02.003>.

Lautier, Jackson P, Vladimir Pozdnyakov, and Jun Yan. 2024. “On the
Convergence of Credit Risk in Current Consumer Automobile Loans.”
*Journal of the Royal Statistical Society Series A: Statistics in
Society*, December, qnae137. <https://doi.org/10.1093/jrsssa/qnae137>.
