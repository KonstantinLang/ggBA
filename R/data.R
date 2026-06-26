#' @title Rodent Temperature Data
#'
#' @description A data set with rodent data comparing two different temperature measurement methods of healthy
#' and treated animals.
#'
#' @format An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with
#' 900 rows and 5 columns:
#' \describe{
#'   \item{animalID}{unique animal ID: 1-90, `integer`}
#'   \item{treatment}{one of: healthy, vehicle, low dose, mid dose, high dose, SoC, `factor`}
#'   \item{method}{measurement method: rectal, infrared, `factor`}
#'   \item{visit}{one of: baseline, visit 1, visit 2, visit 3, end of treatment, `factor`}
#'   \item{temperature}{body temperature in °C, `numeric`}
#' }
"temperature"
