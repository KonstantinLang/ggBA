
## Bland-Altman

A Bland-Altman plot is a method of data plotting used in analyzing the agreement between two different methods. It was popularised in medical statistics by J. Martin Bland and Douglas G. Altman. [1] [2]

For more details see https://en.wikipedia.org/wiki/Bland%E2%80%93Altman_plot

This package provides functions to compute Bland-Altman statistics and to visualize them.

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

## References

[1]: Altman DG, Bland JM (1983). "Measurement in medicine: the analysis of method comparison studies". The Statistician. 32 (3): 307-317. doi:10.2307/2987937. JSTOR 2987937.  
[2]: Bland JM, Altman DG (1986). "Statistical methods for assessing agreement between two methods of clinical measurement" (PDF). Lancet. 327 (8476): 307-10. CiteSeerX 10.1.1.587.8931. doi:10.1016/S0140-6736(86)90837-8. PMID 2868172. S2CID 2844897
