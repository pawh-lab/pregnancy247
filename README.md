
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pregnancy247

<!-- badges: start -->
<!-- badges: end -->

The goal of `pregnancy247` is to process the raw sleep diary, Actiwatch,
and activPAL data for [The University of Iowa Pregnancy
24/7](https://clinicaltrials.uihealthcare.org/studies/pregnancy-247)
research study conducted by [Kara
Whitaker](https://clas.uiowa.edu/hhp/people/kara-m-whitaker).

This package has two unique selections of functions based on the
different needs of the Pregnancy 24/7 study team. The first selection
includes the

- `read_msleep`,
- `check_msleep`,
- `write_msleep`,
- `write_all_msleep`, and
- `quality_check`

functions. The `read_msleep` function imports the sleep or nap data
exported from the Actiwatch to be screened for valid data by
`check_msleep`, and `write_msleep` and `write_all_msleep` export the
screend data. Rules for valid data may be found in the `vingettes`.
`quality_check` is an interface to the above functions for ease of use.

The second selection of functions includes the remaining functions not
listed above. The `process_data` function provides the main interface to
other functions based on the needs of the Pregnancy 24/7 study.
Currently, for this package to operate appropriately, you need to have
your working directory set the directory containing the

- `DATA_Iowa_activPAL`,
- `DATA_Pitt_activPAL`, and
- `DATA_WVU_activPAL`

sub-directories of data. If you are viewing the package as a basis of
code for your own research study, you will need to make changes to how
the functions depend on your file system.

This package will be upgraded to version `1.0.0` when `process_data`,
`check_dir`, and `loc` take a parameter called `dirs`, which should be a
character vector containing the sub-directories within the main project
directory that store the data. Additionally, when the package contains
more functional examples and vingettes.

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
