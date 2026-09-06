
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vbdata

<!-- badges: start -->

<!-- badges: end -->

`vbdata` is an R package that provides an Quarto-powered app allowing to
encode data from a volleyball game score sheet and to extract the data
as `.csv`.

It is aimed in future development to offer analytics solution for the
data extracted with the app.

## Installation

You can install the development version of `vbdata` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("gorinsimon/vbdata")
```

## Usage

After having installed the package, you need to make sure that Quarto is
installed on your machine (see <https://quarto.org/docs/get-started/>).
Next, run the following command to run the app locally.

``` r
vbdata::run_vb_game_encoder()
```
