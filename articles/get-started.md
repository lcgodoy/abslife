# Get Started

``` r
library(abslife)
#> Welcome to the abslife package!
#> WARNING: Under construction.
library(ggplot2) ## optional
```

> Under construction!

In this vignette, we show the basic functionalities of the `abslife`
package. `abslife` provides tools for non-parametric estimation of the
hazard rate, denoted \\\lambda(t)\\ for time to event data subject to
left-truncation (Jackson P. Lautier, Pozdnyakov, and Yan 2023a) and
right-censoring (Jackson P. Lautier, Pozdnyakov, and Yan 2023b). The
package also allows for scenarios where competing risks (Jackson P.
Lautier, Pozdnyakov, and Yan 2024) is present. In all cases, asymptotic
confidence intervals for the estimated hazard rates are readily
available.

The main functionalities of the package are embedded in the
`estimate_hazard` function. In particular, the function is flexible
enough to be able to deal with all the cases mentioned in the previous
paragraph: left-truncation, right-censoring, and competing risks.
Moreover, the output `estimate_hazard` has specific `summary` and `plot`
methods, which are helpful in understanding the results. In the
following sections, we will use three different datasets to exemplify
the package functionalities

### Left-truncation

To load the first dataset (called `aart`), run the chunk below. In
addition to loading the dataset, we are looking at its first rows using
[`head()`](https://rdrr.io/r/utils/head.html). This dataset has two
columns, one representing the time-to-event (`Xi`) and another one the
truncation time (`Yi`).

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

We estimate the hazard rate using the
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
function and assign the result to `aart_hazard`. The `carry_hazard`
replazes 0 hazard estimates with the last non-zero hazard estimate.

``` r
aart_hazard <- estimate_hazard(time_to_event = aart$Xi,
                               trunc_time = aart$Yi,
                               ci_level = 0.95,
                               carry_hazard = TRUE) ## need 
```

The function returns a `data.frame` containing time to event, the
respective hazard estimates, and the standard errors (on the log scale).
It also includes the lower and upper bounds of the hazard rate
confidence interval, which correspond to the confidence level specified
in the `ci_level` argument. Below, we inspect the final rows using
[`tail()`](https://rdrr.io/r/utils/head.html):

``` r
tail(aart_hazard)
#>    time_to_event          fh          uh hazard se_log_hazard  lower_ci
#> 40            44 0.000000000 0.001287830    0.5     0.5000000 0.1876589
#> 41            45 0.000643915 0.001287830    0.5     0.7071068 0.1250488
#> 42            46 0.000000000 0.000643915    0.5     0.7071068 0.1250488
#> 43            47 0.000000000 0.000643915    0.5     0.7071068 0.1250488
#> 44            48 0.000000000 0.000643915    0.5     0.7071068 0.1250488
#> 45            49 0.000643915 0.000643915    1.0     0.0000000 1.0000000
#>    upper_ci
#> 40 1.332204
#> 41 1.999219
#> 42 1.999219
#> 43 1.999219
#> 44 1.999219
#> 45 1.000000
```

> I believe we don’t need to output the `fh` and `uh` columns. What do
> you think?

The [`summary()`](https://rdrr.io/r/base/summary.html) function returns
a cleaner output:

``` r
summary(aart_hazard)
#>    time_to_event     hazard se_log_hazard    lower_ci   upper_ci
#> 1              5 0.01904762    0.70034005 0.004827365 0.07515731
#> 6             10 0.01556420    0.35079121 0.007825874 0.03095429
#> 11            15 0.02432778    0.22660796 0.015603194 0.03793077
#> 16            20 0.01724138    0.25596336 0.010439882 0.02847400
#> 21            25 0.02956705    0.18616745 0.020527782 0.04258671
#> 26            30 0.03311966    0.17660603 0.023429257 0.04681803
#> 31            35 0.05163330    0.13912017 0.039310680 0.06781866
#> 36            40 0.73134328    0.07404587 0.632547761 0.84556935
#> 41            45 0.50000000    0.70710678 0.125048827 1.99921908
```

Finally, we can easily visualize the results using the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function:

``` r
plot(aart_hazard)
```

![](get-started_files/figure-html/plot_aart-1.png)

A `ggplot2` powered version of the plot is also available, and allows to
visualize multiple levels of confidence at once.

``` r
ggauto(aart_hazard, ci_level = c(.5, .75, .85, .9, .95))
```

![](get-started_files/figure-html/ggplot_aart-1.png)

An interesting feature of the package, is that it allows for calculating
the CDF associated with the time to events from using the hazard
function. To achieve that, it suffices to call the `calc_cdf` function.
That function adds a column (called `cdf`) to the output of the
`estimate_hazard` function.

``` r
aart_cdf <- calc_cdf(aart_hazard)
tail(aart_cdf)
#>    time_to_event          fh          uh hazard se_log_hazard  lower_ci
#> 40            44 0.000000000 0.001287830    0.5     0.5000000 0.1876589
#> 41            45 0.000643915 0.001287830    0.5     0.7071068 0.1250488
#> 42            46 0.000000000 0.000643915    0.5     0.7071068 0.1250488
#> 43            47 0.000000000 0.000643915    0.5     0.7071068 0.1250488
#> 44            48 0.000000000 0.000643915    0.5     0.7071068 0.1250488
#> 45            49 0.000643915 0.000643915    1.0     0.0000000 1.0000000
#>    upper_ci       cdf
#> 40 1.332204 0.9995101
#> 41 1.999219 0.9997551
#> 42 1.999219 0.9998775
#> 43 1.999219 0.9999388
#> 44 1.999219 0.9999694
#> 45 1.000000 1.0000000
```

### Right-censoring

Now, we show that we can use an almost identical workflow to deal
analyze right-censored data as well. To achieve that goal, we will load
another dataset, called `mbalt`. This dataset has three, as opposed to
two, columns. They represent the time-to-event (`Zi`), the
left-truncation time (`Yi`), and a right-censoring indicator (`Di`).

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

Once again, we estimate the hazard rate using the
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
function and this time assign its result to `mbalt_hazard`. The
`carry_hazard` replazes 0 hazard estimates with the last non-zero hazard
estimate. Notice, we added an extra argument to the function call: the
`censoring` argument, which receives an indicator variable that has 1s
if one event is right censored and 0 otherwise.

``` r
mbalt_hazard <- estimate_hazard(time_to_event = mbalt$Zi,
                                trunc_time = mbalt$Yi,
                                censoring = mbalt$Di,
                                ci_level = 0.95,
                                carry_hazard = TRUE) ## need 
```

The output is analogous to what we have seen in the previous example,
and we can easily extract some summary as follows:

``` r
summary(mbalt_hazard)
#>    time_to_event     hazard se_log_hazard   lower_ci   upper_ci
#> 27            31 0.05372423    0.02048953 0.05160948 0.05592563
#> 32            36 0.04244229    0.03464025 0.03965637 0.04542393
```

The plotting functions also work out of the box:

``` r
plot(mbalt_hazard)
```

![](get-started_files/figure-html/plot_mbalt-1.png)

A `ggplot2` powered version of the plot is also available, and allows to
visualize multiple levels of confidence at once.

``` r
ggauto(mbalt_hazard, ci_level = c(.5, .75, .85, .9, .95))
```

![](get-started_files/figure-html/ggplot_mbalt-1.png)

### Competing risks

Lastly, we show the
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
function features to deal with competing risks. To showcase those
features, we load the `aloans` dataset. This dataset contain information
on consumer automobile loans (Jackson P. Lautier, Pozdnyakov, and Yan
2024). To check what each of its columns represent, take a look at the
dataset documentation by running
[`help(aloans)`](http://lcgodoy.me/abslife/reference/aloans.md).

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

The two competing risks in this case are default and pre-payment. We
will create a more informative column to distinguish between those two
events.

``` r
aloans <- transform(aloans, event_type = ifelse(D == 1, "Defaut", "Pre-payment"))
```

The package is designed so the workflow is identical as the previous two
examples.

``` r
aloans_hazard <- estimate_hazard(time_to_event = aloans$Z,
                                 trunc_time = aloans$Y,
                                 censoring = aloans$C,
                                 event_type = aloans$event_type,
                                 ci_level = 0.95,
                                 carry_hazard = TRUE) ## need 
```

The output is analogous to what we have seen in the previous example,
but now we have an additional column specifing the event type.

``` r
summary(aloans_hazard)
#>     event_type time_to_event      hazard se_log_hazard     lower_ci    upper_ci
#> 1       Defaut             3 0.011520293    0.06431095 0.0101559826 0.013067879
#> 2       Defaut             8 0.012729470    0.03827262 0.0118095304 0.013721072
#> 3       Defaut            13 0.010477761    0.04439775 0.0096045507 0.011430360
#> 4       Defaut            18 0.013350256    0.04167804 0.0123030612 0.014486585
#> 5       Defaut            23 0.012722853    0.04578351 0.0116309047 0.013917316
#> 6       Defaut            28 0.013859145    0.04665738 0.0126479922 0.015186275
#> 7       Defaut            33 0.013032176    0.05157801 0.0117791425 0.014418505
#> 8       Defaut            38 0.018664205    0.04593909 0.0170571344 0.020422688
#> 9       Defaut            43 0.011084154    0.06514809 0.0097554744 0.012593798
#> 10      Defaut            48 0.007673623    0.08449283 0.0065024947 0.009055676
#> 11      Defaut            53 0.006133755    0.10337666 0.0050087815 0.007511399
#> 12      Defaut            58 0.006600660    0.49834710 0.0024853843 0.017529971
#> 13      Defaut            63 0.002898551    0.99854967 0.0004094623 0.020518610
#> 14      Defaut            68 0.031250000    0.98425098 0.0045399813 0.215102758
#> 15 Pre-payment             8 0.007686787    0.04937728 0.0069777408 0.008467883
#> 16 Pre-payment            13 0.011646595    0.04208613 0.0107244538 0.012648026
#> 17 Pre-payment            18 0.015630141    0.03847409 0.0144948494 0.016854354
#> 18 Pre-payment            23 0.011372231    0.04845910 0.0103418258 0.012505301
#> 19 Pre-payment            28 0.015052316    0.04474289 0.0137885345 0.016431929
#> 20 Pre-payment            33 0.015385696    0.04741284 0.0140203633 0.016883988
#> 21 Pre-payment            38 0.013687084    0.05378116 0.0123177763 0.015208610
#> 22 Pre-payment            43 0.018648019    0.05003448 0.0169060937 0.020569423
#> 23 Pre-payment            48 0.017776306    0.05523028 0.0159525240 0.019808592
#> 24 Pre-payment            53 0.032053819    0.04462800 0.0293692158 0.034983818
#> 25 Pre-payment            58 0.037953795    0.20451918 0.0254195682 0.056668570
#> 26 Pre-payment            63 0.020408163    0.98974332 0.0029331406 0.141995624
#> 27 Pre-payment            68 0.076923077    0.96076892 0.0117016919 0.505667029
```

The plotting functions work as in the previous examples:

``` r
plot(aloans_hazard)
```

![](get-started_files/figure-html/plot_aloans-1.png)

A `ggplot2` powered version of the plot is also available:

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
