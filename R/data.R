#' @title Rodent Temperature Data
#'
#' @description A data set with rodent data comparing two different temperature measurement methods of healthy
#' and treated animals.
#'
#' @return
#' \item{animalID}{unique animal ID: 1-90, `integer`}
#' \item{treatment}{one of: healthy, untreated, low dose, mid dose, high dose, positive control, `factor`}
#' \item{method}{measurement method: rectal, infrared, `factor`}
#' \item{visit}{one of: baseline, visit1, visit2, visit3, end of treatment, `factor`}
#' \item{tempareture}{body temparature in °C, `numeric`}
"temperature"
