# Bland-Altman statistics

The function is computing all relevant Bland-Altman statistics,
including bias, lower and upper limits of agreement and their confidence
limits.

## Usage

``` r
ba_stat(
  data = stop("data must be specified"),
  var1 = stop("variable must be specified"),
  var2 = stop("variable must be specified"),
  group = NULL,
  alpha = 0.05,
  transform = c("identity", "log", "logit")
)
```

## Arguments

- data:

  A data frame

- var1:

  1st variable to compare (unquoted)

- var2:

  2nd variable to compare (unquoted)

- group:

  grouping variable (unquoted)

- alpha:

  alpha level for the intervals

- transform:

  Transformation to apply before computing statistics. One of
  `"identity"` (default), `"log"`, or `"logit"`. Delegates to
  [ba_mean_diff](https://konstantinlang.github.io/ggBA/reference/ba_mean_diff.md).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
three variables n (number of observations), parameter and value is
returned.

## See also

[ba_plot](https://konstantinlang.github.io/ggBA/reference/ba_plot.md)

## Examples

``` r
library(tidyr)
tbl <- temperature |> pivot_wider(names_from = method, values_from = temperature)

# simple example
ba_stat(data = tbl, var1 = infrared, var2 = rectal)
#> # A tibble: 9 × 3
#>       n parameter   value
#>   <int> <chr>       <dbl>
#> 1   450 bias       0.234 
#> 2   450 lloa      -2.85  
#> 3   450 uloa       3.32  
#> 4   450 bias.lcl   0.0879
#> 5   450 lloa.lcl  -3.10  
#> 6   450 uloa.lcl   3.07  
#> 7   450 bias.ucl   0.379 
#> 8   450 lloa.ucl  -2.60  
#> 9   450 uloa.ucl   3.57  

## example with grouping
ba_stat(data = tbl, var1 = infrared, var2 = rectal, group = treatment) |>
  pivot_wider(names_from = parameter, values_from = value)
#> # A tibble: 6 × 11
#>   treatment     n   bias  lloa  uloa bias.lcl lloa.lcl uloa.lcl bias.ucl
#>   <fct>     <int>  <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 healthy      75 0.202  -2.51  2.92  -0.116     -3.06     2.37    0.521
#> 2 vehicle      75 0.0383 -3.23  3.30  -0.345     -3.88     2.65    0.422
#> 3 low dose     75 0.0353 -3.16  3.23  -0.339     -3.80     2.58    0.410
#> 4 mid dose     75 0.634  -2.45  3.72   0.273     -3.07     3.09    0.996
#> 5 high dose    75 0.173  -3.15  3.50  -0.218     -3.82     2.83    0.563
#> 6 SoC          75 0.319  -2.50  3.14  -0.0125    -3.07     2.57    0.650
#> # ℹ 2 more variables: lloa.ucl <dbl>, uloa.ucl <dbl>
```
