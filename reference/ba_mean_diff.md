# Derive mean and difference for Bland-Altman analysis

A helper function that computes the mean and difference (or ratio) of
two variables after applying a transformation. Supported transformations
are `"identity"` (no transformation), `"log"` (natural logarithm, ratio
back on the original scale), and `"logit"` (logit transformation).

## Usage

``` r
ba_mean_diff(
  data = stop("data must be specified"),
  var1 = stop("variable must be specified"),
  var2 = stop("variable must be specified"),
  transform = c("identity", "log", "logit")
)
```

## Arguments

- data:

  A data frame.

- var1:

  1st variable to compare (unquoted).

- var2:

  2nd variable to compare (unquoted).

- transform:

  Transformation to apply before computing mean and difference. One of
  `"identity"` (default), `"log"`, or `"logit"`.

## Value

The input data frame with two additional columns:

- avg:

  Mean of the (transformed) paired observations.

- dfce:

  Difference of the (transformed) paired observations. For `"log"` this
  equals `log(var1 / var2)`.

## See also

[ba_stat](https://konstantinlang.github.io/ggBA/reference/ba_stat.md),
[ba_plot](https://konstantinlang.github.io/ggBA/reference/ba_plot.md)

## Examples

``` r
library(tidyr)
tbl <- temperature %>% pivot_wider(names_from = method, values_from = temperature)

# identity (default) - same as used inside ba_stat / ba_plot
ba_mean_diff(tbl, var1 = infrared, var2 = rectal)
#> # A tibble: 450 × 7
#>    animalID treatment visit            rectal infrared   avg   dfce
#>       <int> <fct>     <fct>             <dbl>    <dbl> <dbl>  <dbl>
#>  1        1 healthy   baseline           33.9     34.8  34.4  0.932
#>  2        1 healthy   visit 1            34.6     36.5  35.5  1.92 
#>  3        1 healthy   visit 2            35.4     36.2  35.8  0.780
#>  4        1 healthy   visit 3            35.4     35.7  35.6  0.310
#>  5        1 healthy   end of treatment   35.7     35.1  35.4 -0.616
#>  6        2 vehicle   baseline           37.6     38.6  38.1  0.969
#>  7        2 vehicle   visit 1            35.1     38.5  36.8  3.39 
#>  8        2 vehicle   visit 2            37.4     36.0  36.7 -1.42 
#>  9        2 vehicle   visit 3            37.9     36.1  37.0 -1.87 
#> 10        2 vehicle   end of treatment   38.7     36.4  37.6 -2.32 
#> # ℹ 440 more rows

# log transformation
ba_mean_diff(tbl, var1 = infrared, var2 = rectal, transform = "log")
#> # A tibble: 450 × 7
#>    animalID treatment visit            rectal infrared   avg     dfce
#>       <int> <fct>     <fct>             <dbl>    <dbl> <dbl>    <dbl>
#>  1        1 healthy   baseline           33.9     34.8  3.54  0.0271 
#>  2        1 healthy   visit 1            34.6     36.5  3.57  0.0540 
#>  3        1 healthy   visit 2            35.4     36.2  3.58  0.0218 
#>  4        1 healthy   visit 3            35.4     35.7  3.57  0.00873
#>  5        1 healthy   end of treatment   35.7     35.1  3.57 -0.0174 
#>  6        2 vehicle   baseline           37.6     38.6  3.64  0.0254 
#>  7        2 vehicle   visit 1            35.1     38.5  3.60  0.0922 
#>  8        2 vehicle   visit 2            37.4     36.0  3.60 -0.0387 
#>  9        2 vehicle   visit 3            37.9     36.1  3.61 -0.0505 
#> 10        2 vehicle   end of treatment   38.7     36.4  3.63 -0.0618 
#> # ℹ 440 more rows
```
