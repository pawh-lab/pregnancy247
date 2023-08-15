
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pregnancy247

<!-- badges: start -->
<!-- badges: end -->

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
`activpalProcessing` because this package is no longer available on
`CRAN` and an archived copy of this package is used. So, it must be
installed prior to installing `pregnancy247`.

## Useage

This package has two unique selections of functions based on the
different needs of the Pregnancy 24/7 study team.

### Actiwatch data quality check

The first selection includes the

- `read_msleep`,
- `check_msleep`,
- `write_msleep`,
- `write_all_msleep`, and
- `quality_check`

functions. The `read_msleep` function imports the sleep or nap data
exported from the Actiwatch to be screened for valid data by
`check_msleep`, and `write_msleep` and `write_all_msleep` export the
screend data. Rules for valid data may be found in the `vingettes`.
`quality_check()` is an interface to the above functions for ease of
use. An example of the utility of this interface is presented below.

``` r
quality_check(subject = "1000-AB", trimester = 1)
```

Currently, to check the Actiwatch data appropriately, the working
directory must be the main directory containing the data of each subject
within subject-specific folders based on IDs. It is also assumed the
names of the folders in subject-specific directories are

- `Visit 1`,
- `Visit 2`, and
- `Visit 3`

### Actiwatch, activPAL, and diary data processing

The second selection of functions includes the remaining functions not
listed above to process the Actiwatch, activPAL, and diary data. The
`process_data()` function provides the main interface to the other
functions based on the needs of the Pregnancy 24/7 study. An example of
the utility of this interface is presented below.

``` r
process_data(subject = "1000-AB", trimester = 1, sleep_source = "./sleep.csv")
```

Currently, to process these data appropriately, you need to have your
working directory set the directory containing the

- `DATA_Iowa_activPAL`,
- `DATA_Pitt_activPAL`, and
- `DATA_WVU_activPAL`

sub-directories of data. If you are viewing the package as a basis of
code for your own research study, you will need to make changes to how
the functions depend on your file system.
