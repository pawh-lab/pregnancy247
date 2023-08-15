
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pregnancy247

## Goals

The goal of `pregnancy247` is to process and check the raw sleep diary,
Actiwatch, and activPAL data for [The University of Iowa Pregnancy
24/7](https://clinicaltrials.uihealthcare.org/studies/pregnancy-247)
research study conducted by [Kara
Whitaker](https://clas.uiowa.edu/hhp/people/kara-m-whitaker).

## Installation

You can install the development version of pregnancy247 from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("deboonstra/activpalProcessing")
remotes::install_github("deboonstra/pregnancy247")
```

There is an added installation call to a package called
[`activpalProcessing`](https://github.com/deboonstra/activpalProcessing)
because this package is no longer available on `CRAN` and an archived
copy of this package is used. So, it must be installed prior to
installing `pregnancy247`.

## Useage

This package has two unique selections of functions based on the
different needs of the Pregnancy 24/7 study team.

``` r
library(pregnancy247)
```

There are two main functions in this package, which use the other
functions in the package. These functions are `quality_check()` and
`process_data()`. The first function checks the quality of the recorded
Actiwatch data,

``` r
quality_check(subject = "1000-AB", trimester = 1)
```

while the second processes the Actiwatch, sleep diary, and activPAL
data.

``` r
process_data(subject = "1000-AB", trimester = 1, sleep_source = "./sleep.csv")
```
