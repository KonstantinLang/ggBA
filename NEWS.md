# BAplot 0.3.0

* Added `transform` parameter to `ba_stat()` and `ba_plot()`, supporting
  `"identity"` (default), `"log"`, and `"logit"` transformations; both
  functions now delegate mean/difference computation to `ba_mean_diff()`.

# BAplot 0.2.0

* Added `ba_mean_diff()`: helper function to compute mean and difference (or
  ratio) of two variables with optional `"identity"`, `"log"`, or `"logit"`
  transformation.
* Added comprehensive unit tests for `ba_stat()`, `ba_plot()`, and
  `ba_mean_diff()` using **testthat** 3rd edition.
* Added GitHub Actions workflow for automated `R CMD check`.
* Added **pkgdown** site workflow.
* Updated Roxygen documentation throughout.

# BAplot 0.1.0

* Initial release.
* `ba_stat()`: compute Bland-Altman statistics (bias, limits of agreement, and
  their confidence intervals) with optional grouping.
* `ba_plot()`: visualize Bland-Altman statistics using **ggplot2**.
* `temperature` dataset: simulated rodent body temperature measurements by two
  methods (rectal and infrared) across multiple visits and treatment groups.
