#' Derive mean and difference for Bland-Altman analysis
#'
#' A helper function that computes the mean and difference (or ratio) of two
#' variables after applying a transformation.  Supported transformations are
#' `"identity"` (no transformation), `"log"` (natural logarithm, ratio back on
#' the original scale), and `"logit"` (logit transformation).
#'
#' @param data A data frame.
#' @param var1 1st variable to compare (unquoted).
#' @param var2 2nd variable to compare (unquoted).
#' @param transform Transformation to apply before computing mean and
#'   difference.  One of `"identity"` (default), `"log"`, or `"logit"`.
#'
#' @return The input data frame with two additional columns:
#' \describe{
#'   \item{avg}{Mean of the (transformed) paired observations.}
#'   \item{dfce}{Difference of the (transformed) paired observations.  For
#'     `"log"` this equals `log(var1 / var2)`.}
#' }
#'
#' @seealso [ba_stat()], [ba_plot()]
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' tbl <- temperature %>% pivot_wider(names_from = method, values_from = temperature)
#'
#' # identity (default) – same as used inside ba_stat / ba_plot
#' ba_mean_diff(tbl, var1 = infrared, var2 = rectal)
#'
#' # log transformation
#' ba_mean_diff(tbl, var1 = infrared, var2 = rectal, transform = "log")
ba_mean_diff <- function(
  data      = stop("data must be specified"),
  var1      = stop("variable must be specified"),
  var2      = stop("variable must be specified"),
  transform = c("identity", "log", "logit")
) {

  stopifnot(inherits(data, "data.frame"))
  transform <- match.arg(transform)

  trans_fn <- switch(
    transform,
    identity = function(x) x,
    log      = function(x) log(x),
    logit    = function(x) log(x / (1 - x))
  )

  data %>%
    dplyr::mutate(
      .v1  = trans_fn({{var1}}),
      .v2  = trans_fn({{var2}}),
      avg  = (.v1 + .v2) / 2,
      dfce = .v1 - .v2
    ) %>%
    dplyr::select(-.v1, -.v2)
}
