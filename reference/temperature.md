# Rodent Temperature Data

A data set with rodent data comparing two different temperature
measurement methods of healthy and treated animals.

## Usage

``` r
temperature
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 900
rows and 5 columns:

- animalID:

  unique animal ID: 1-90, `integer`

- treatment:

  one of: healthy, vehicle, low dose, mid dose, high dose, SoC, `factor`

- method:

  measurement method: rectal, infrared, `factor`

- visit:

  one of: baseline, visit 1, visit 2, visit 3, end of treatment,
  `factor`

- temperature:

  body temperature in °C, `numeric`
