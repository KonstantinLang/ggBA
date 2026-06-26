#' Generate rodents temperature data
#'
#' @param seed a single value, interpreted as an integer, or \code{NULL}
#'
#' @return \code{\link{tibble}} with columns
#' \item{animalID}{unique animal ID: 1-90, `integer`}
#' \item{treatment}{one of: healthy, vehicle, low dose, mid dose, high dose, SoC, `factor`}
#' \item{method}{measurement method: rectal, infrared, `factor`}
#' \item{visit}{one of: baseline, visit 1, visit 2, visit 3, end of treatment, `factor`}
#' \item{temperature}{body temperature in °C, `numeric`}
#'
#' @examples
#' temperature <- BAplot:::gen_temp_data()
gen_temp_data <- function(seed = NULL) {
  set.seed(seed = seed)

  tbl <-
    tibble::tibble(
      animalID  = 1:90,
      treatment = rep(
        x     = c("healthy", "vehicle", "low dose", "mid dose", "high dose", "SoC"),
        times = 15
      )
    ) |>
    tidyr::expand_grid(
      visit  = c("baseline", paste("visit", 1:3), "end of treatment"),
      method = c("rectal", "infrared")
    ) |>
    dplyr::mutate(dplyr::across(.cols = tidyselect::vars_select_helpers$where(is.character), .fns = forcats::fct_inorder)) |>
    dplyr::mutate(
      aux = as.integer(visit) - 1,
      temperature = {
        x <- stats::rnorm(n = dplyr::n(), mean = 0, sd = 1)
        # SD adjustment
        x <- dplyr::if_else(treatment == "vehicle", x * 1.2, x)
        x <- dplyr::if_else(treatment %in% c("low dose", "mid dose") & as.integer(visit) < 4L, x * 1.1, x)
        x <- dplyr::if_else(method == "infrared", x * 1.1, x)
        # mean adjustment
        x <- dplyr::if_else(treatment != "healthy", x + 2, x)
        x <- dplyr::if_else(treatment == "low dose", x - aux * 0.125, x)
        x <- dplyr::if_else(treatment == "mid dose", x - aux * 0.25, x)
        x <- dplyr::if_else(treatment %in% c("high dose", "SoC"), x - aux * 0.5, x)
        x <- dplyr::if_else(method == "infrared", x + 0.25, x)
        x + 35
      }
    ) |>
    dplyr::select(-aux)

  return(tbl)
}
