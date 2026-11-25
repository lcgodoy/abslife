# abslife

> Under construction!

The goal of abslife is to …

## Installation

You can install the development version of abslife like so:

``` r

remotes::install_github("lcgodoy/abslife")
```

## Example

``` r

library(abslife)
#> Welcome to the abslife package!
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r

data(aloans)
summary(aloans)
#>       risk.cat           Z               Y               C         
#>  Length   :58118   Min.   : 3.00   Min.   : 2.00   Min.   :0.0000  
#>  N.unique :    5   1st Qu.:17.00   1st Qu.: 3.00   1st Qu.:0.0000  
#>  N.blank  :    0   Median :32.00   Median : 4.00   Median :0.0000  
#>  Min.nchar:    5   Mean   :32.82   Mean   : 4.44   Mean   :0.2348  
#>  Max.nchar:   13   3rd Qu.:53.00   3rd Qu.: 5.00   3rd Qu.:0.0000  
#>                    Max.   :70.00   Max.   :19.00   Max.   :1.0000  
#>        D                R                 bond          orig.apr     
#>  Min.   :0.0000   Min.   :0.0000   Length   :58118   Min.   :0.0249  
#>  1st Qu.:0.0000   1st Qu.:0.0000   N.unique :    4   1st Qu.:0.1470  
#>  Median :0.0000   Median :0.0000   N.blank  :    0   Median :0.1834  
#>  Mean   :0.3656   Mean   :0.3996   Min.nchar:    4   Mean   :0.1747  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   Max.nchar:    6   3rd Qu.:0.2100  
#>  Max.   :1.0000   Max.   :1.0000                     Max.   :0.2799  
#>    orig.term     orig.loan.amt      abs0.age      abs0.balance    
#>  Min.   :72.00   Min.   : 3068   Min.   : 1.00   Min.   :  686.8  
#>  1st Qu.:72.00   1st Qu.:13052   1st Qu.: 2.00   1st Qu.:12925.5  
#>  Median :72.00   Median :16181   Median : 3.00   Median :16014.2  
#>  Mean   :72.12   Mean   :17751   Mean   : 3.44   Mean   :17569.8  
#>  3rd Qu.:72.00   3rd Qu.:21006   3rd Qu.: 4.00   3rd Qu.:20786.2  
#>  Max.   :73.00   Max.   :71121   Max.   :18.00   Max.   :70139.2  
#>     calc.pmt      
#>  Min.   :  53.42  
#>  1st Qu.: 287.85  
#>  Median : 353.35  
#>  Mean   : 381.20  
#>  3rd Qu.: 446.38  
#>  Max.   :1285.23
```
