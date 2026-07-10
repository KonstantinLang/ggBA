# Bland-Altman plot

Plot Bland-Altman statistics

## Usage

``` r
ba_plot(
  data = stop("data must be specified"),
  var1 = stop("variable must be specified"),
  var2 = stop("variable must be specified"),
  label = NULL,
  group = NULL,
  colour = NULL,
  shape = NULL,
  xlab = "Average",
  ylab = "Difference",
  title = NULL,
  caption = NULL,
  alpha = 0.05,
  point_size = 3,
  point_alpha = 0.5,
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

- label:

  data label (unquoted)

- group:

  grouping variable used for faceting (unquoted)

- colour:

  colour aesthetic for scatter points (unquoted)

- shape:

  shape aesthetic for scatter points (unquoted)

- xlab:

  The text for the x-axis label

- ylab:

  The text for the y-axis label

- title:

  plot title

- caption:

  plot caption

- alpha:

  alpha level for the intervals

- point_size:

  size of the scatter points (passed to
  [ggplot2::geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html))

- point_alpha:

  opacity of the scatter points (passed to
  [ggplot2::geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html))

- transform:

  Transformation applied before computing statistics. One of
  `"identity"` (default), `"log"`, or `"logit"`. Delegates to
  [ba_mean_diff](https://konstantinlang.github.io/ggBA/reference/ba_mean_diff.md).

## Value

[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

## See also

[ba_stat](https://konstantinlang.github.io/ggBA/reference/ba_stat.md)

## Examples

``` r
library(tidyr)
tbl <- temperature |> pivot_wider(names_from = method, values_from = temperature)

# simple example
ba_plot(data = tbl, var1 = infrared, var2 = rectal)
#> Warning: Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
#> ℹ Please use `cross_join()` instead.
#> ℹ The deprecated feature was likely used in the ggBA package.
#>   Please report the issue to the authors.


# with colors
ba_plot(data = tbl, var1 = infrared, var2 = rectal, colour = visit)


# with colors and faceting
ba_plot(data = tbl, var1 = infrared, var2 = rectal, group = treatment, colour = visit)
```
