---
output: github_document
---




# SIVAshiny

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Interface shiny pour la visualisation des données

## Installation

You can install the development version of SIVAshiny from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Eaux-et-Vilaine/SIVAshiny")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(SIVAshiny)
SIVAshiny()
```

![image](https://user-images.githubusercontent.com/26055877/208050138-956f556e-5a12-4c34-b7ce-1ddec98b12dc.png)


You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" title="plot of chunk pressure" alt="plot of chunk pressure" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
