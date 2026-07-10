# Generate rodents temperature data

Generate rodents temperature data

## Usage

``` r
gen_temp_data(seed = NULL)
```

## Arguments

- seed:

  a single value, interpreted as an integer, or `NULL`

## Value

[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
columns

- animalID:

  unique animal ID: 1-90, `integer`

- treatment:

  one of: healthy, untreated, low dose, mid dose, high dose, positive
  control, `factor`

- method:

  measurement method: rectal, infrared, `factor`

- visit:

  one of: baseline, visit1, visit2, visit3, end of treatment, `factor`

- tempareture:

  body temparature in °C, `numeric`

## Examples

``` r
temperature <- ggBA:::gen_temp_data()
```
