
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `discoverr`

The goal of `discoverr` is to automate the first data discovery steps.

## Installation

You can install `discoverr` with:

``` r
devtools::install_github("edo91/discoverr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(discoverr)
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
```

``` r
plot_var(iris)
#> Loading required package: ggfancy
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-3.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-4.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-5.png" width="100%" />

``` r
plot_2var(iris)
#> y is assumed to be the last column of data
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-3-3.png" width="100%" /><img src="man/figures/README-unnamed-chunk-3-4.png" width="100%" />
