## ggBA

**ggBA** helps you compute and visualize Bland-Altman agreement
statistics with a tidy, ggplot2-friendly workflow.

It supports identity, log, and logit scales so you can analyze absolute
differences, ratios, or proportion-scale agreement in a consistent API.

## Website and guides

Package site: <https://konstantinlang.github.io/ggBA/>

Included vignettes:

1.  `getting_started`: first analysis in a few steps
2.  `compare_methods`: side-by-side comparisons across scales
3.  `grouped_analysis`: stratified analysis and faceted visualization

## Installation

Install the development version from GitHub:

``` r

install.packages("remotes")
remotes::install_github("KonstantinLang/ggBA")
```

## Quick example

``` r

library(tidyr)
library(ggBA)

tbl <- temperature |>
  pivot_wider(names_from = method, values_from = temperature)

ba_stat(tbl, infrared, rectal)
ba_plot(tbl, infrared, rectal)
```

## Development checks

For a full local CRAN-style check (including manual/PDF checks), install
system tools first (`pdflatex`, `qpdf`, and optionally `tidy`), then
run:

``` sh
R CMD build .
R CMD check --as-cran ggBA_*.tar.gz
```

## Issue tracker

Report issues at <https://github.com/KonstantinLang/ggBA/issues>.

## References

\[1\]: Altman DG, Bland JM (1983). “Measurement in medicine: the
analysis of method comparison studies”. The Statistician. 32 (3):
307-317. <doi:10.2307/2987937>. JSTOR 2987937.  
\[2\]: Bland JM, Altman DG (1986). “Statistical methods for assessing
agreement between two methods of clinical measurement” (PDF). Lancet.
327 (8476): 307-10. CiteSeerX 10.1.1.587.8931.
<doi:10.1016/S0140-6736(86)90837-8>. PMID 2868172. S2CID 2844897
