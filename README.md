
<!-- badges: start -->
  [![R-CMD-check](https://github.com/KonstantinLang/BAplot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KonstantinLang/BAplot/actions/workflows/R-CMD-check.yaml)
  [![pkgdown](https://github.com/KonstantinLang/BAplot/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/KonstantinLang/BAplot/actions/workflows/pkgdown.yaml)
  [![](https://img.shields.io/github/last-commit/KonstantinLang/BAplot.svg)](https://github.com/KonstantinLang/BAplot/commits/main)
<!-- badges: end -->

## Bland-Altman

A Bland-Altman plot is a method of data plotting used in analyzing the agreement between two different methods. It was popularised in medical statistics by J. Martin Bland and Douglas G. Altman. [[1]](#1) [[2]](#2)

For more details see https://en.wikipedia.org/wiki/Bland%E2%80%93Altman_plot

This package provides functions to plot Bland-Altman statistics based on {[ggplot2](https://github.com/tidyverse/ggplot2)} functionality.

## Installation

Make sure {remotes} is installed:

```r
install.packages(c("remotes"))
```

Install source package from this repo. 3<sup>rd</sup>-party packages required or suggested by {BAplot} will be installed and/or upgraded automatically.

```r
remotes::install_github("KonstantinLang/ggBA")
```

## Issue tracker

Report bugs etc. at https://github.com/KonstantinLang/BAplot/issues.

## References

<span id="#1">[1]</span>: Altman DG, Bland JM (1983). "Measurement in medicine: the analysis of method comparison studies". The Statistician. 32 (3): 307-317. doi:10.2307/2987937. JSTOR 2987937.  
<span id="#2">[2]</span>: Bland JM, Altman DG (1986). "Statistical methods for assessing agreement between two methods of clinical measurement" (PDF). Lancet. 327 (8476): 307-10. CiteSeerX 10.1.1.587.8931. doi:10.1016/S0140-6736(86)90837-8. PMID 2868172. S2CID 2844897
