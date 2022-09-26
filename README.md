
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/KonstantinLang/BAplot.svg?branch=main)](https://travis-ci.com/KonstantinLang/BAplot)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/KonstantinLang/BAplot?branch=main&svg=true)](https://ci.appveyor.com/project/KonstantinLang/BAplot)
<!-- badges: end -->

## Bland-Altman

R functions to compute Bland-Altman statistics and to visualize those statistics.

## Installation

### Required packages

- [{remotes}](https://cran.r-project.org/package=remotes) package

Just make sure it is/they are installed:

```r
install.packages(c("remotes"))
```

### Instructions

Install source package from this repo. 3<sup>rd</sup>-party packages required or suggested by {BAplot} will be installed and/or upgraded automatically.

```r
remotes::install_github(repo = "KonstantinLang/BAplot", ref = "main", dependencies = TRUE)
```

## Issue tracker

Report bugs etc. at https://github.com/KonstantinLang/BAplot/issues.
