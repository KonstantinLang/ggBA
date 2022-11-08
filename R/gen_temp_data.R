#' Generate rodents temperature data
#'
#' @param seed a single value, interpreted as an integer, or \code{NULL}
#'
#' @return \code{\link{tibble}} with columns
#' \item{animalID}{unique animal ID: 1-90, `integer`}
#' \item{treatment}{one of: healthy, untreated, low dose, mid dose, high dose, positive control, `factor`}
#' \item{method}{measurement method: rectal, infrared, `factor`}
#' \item{visit}{one of: baseline, visit1, visit2, visit3, end of treatment, `factor`}
#' \item{tempareture}{body temparature in °C, `numeric`}
#'
#' @examples
#' temperature <- gen_data()
gen_temp_data <- function(seed = NULL) {
  set.seed(seed = seed)

  tbl <-
    tibble::tibble(
      animalID  = 1:90,
      treatment = rep(
        x     = c("healthy", "vehicle", "low dose", "mid dose", "high dose", "SoC"),
        times = 15
      )
    ) %>%
    tidyr::expand_grid(
      visit  = c("baseline", paste("visit", 1:3), "end of treatment"),
      method = c("rectal", "infrared")
    ) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect:::where(is.character), .fns = forcats::fct_inorder)) %>%
    dplyr::mutate(
      aux = as.integer(visit) - 1,
      temperature =
        stats::rnorm(n = dplyr::n(), mean = 0, sd = 1) %>%
        # SD adjustment
        dplyr::if_else(treatment == "vehicle", . * 1.2, .) %>%
        dplyr::if_else(treatment %in% c("low dose", "mid dose") & as.integer(visit) < 4L, . * 1.1, .) %>%
        dplyr::if_else(method == "infrared", . * 1.1, .) %>%
        # mean adjustment
        dplyr::if_else(treatment != "healthy", . + 2, .) %>%
        dplyr::if_else(treatment == "low dose", . - aux * 0.125, .) %>%
        dplyr::if_else(treatment == "mid dose", . - aux * 0.25, .) %>%
        dplyr::if_else(treatment %in% c("high dose", "SoC"), . - aux * 0.5, .) %>%
        dplyr::if_else(method == "infrared", . + 0.25, .) %>%
        {. + 35}
    ) %>%
    dplyr::select(-aux)

  return(tbl)
}
