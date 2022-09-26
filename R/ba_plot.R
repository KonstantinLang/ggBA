#' Bland-Altman plot
#'
#' ...
#'
#' @param data A data frame
#' @param var1 1st variable to compare (unquoted)
#' @param var2 2nd variable to compare (unquoted)
#' @param label data label (unquote)
#' @param group grouping variable used for faceting (unquoted)
#' @param colour colour asthetic for scatter points (unquoted)
#' @param shape shape asthetic for scatter points (unquoted)
#' @param xlab The text for the x-axis label
#' @param ylab The text for the y-axis label
#' @param title plot title
#' @param caption plot caption
#' @param alpha alpha level for the intervals
#'
#' @return ggplot2 object
#'
#' @seealso ba_stat
#'
#' @examples
#' ## simple example
#' ba_plot(data = iris, var1 = Petal.Length, var2 = Petal.Width)
#'
#' ## with faceting
#' ba_plot(data = iris, var1 = Petal.Length, var2 = Petal.Width, group = Species)
ba_plot <- function(
  data, var1, var2, label = NULL, group = NULL, colour = NULL, shape = NULL,
  xlab = "Average", ylab = "Difference", title = NULL, caption = NULL,
  alpha = 0.05
) {

  # compute differences and averages between paired obs
  tbl_0 <-
    data %>%
    mutate(
      dfce = {{var1}} - {{var2}},       # difference
      avg  = ({{var1}} + {{var2}}) / 2  # average
    )

  # derive all relevant statistics: bias, LoA, and confidence intervals
  tbl_stat <-
    ba_stat(data = data, var1 = {{var1}}, var2 = {{var2}}, group = {{group}}, alpha = alpha) %>%
    mutate(
      ltyp = str_detect(string = parameter, pattern = "\\."),
      lsiz = (parameter != "bias")
    ) %>%
    {
      if(as_label(enquo(group)) != "NULL") {
        left_join(
          x = .,
          y = tbl_0 %>%
            group_by({{group}}) %>%
            summarise(
              xmn = min(avg, na.rm = TRUE),
              xmx = max(avg, na.rm = TRUE),
              ymn = min(dfce, na.rm = TRUE),
              ymx = max(dfce, na.rm = TRUE),
              .groups = "keep"
            ) %>%
            ungroup()
        ) %>%
          left_join(
            y = {.} %>%
              group_by({{group}}) %>%
              summarise(scale = ceiling(log10(min(abs(value))))) %>%
              ungroup()
          )
      } else {
        bind_cols(
          .,
          tbl_0 %>%
            summarise(
              xmn = min(avg, na.rm = TRUE),
              xmx = max(avg, na.rm = TRUE),
              ymn = min(dfce, na.rm = TRUE),
              ymx = max(dfce, na.rm = TRUE)
            ),
          {.} %>% summarise(scale = ceiling(log10(min(abs(value)))))
        )
      }
    } %>%
    mutate(
      tx    = xmx + .12 * (xmx-xmn),
      ty    = value + (ymx-ymn) * .01,
      aux   = 3 - scale,
      dplac = if_else(aux < 0, 0, aux) %>% if_else(. > 3, 3, .),
      tlbl  = sprintf(fmt = paste0("%.", dplac, "f"), value)
    )

  tbl_1 <-
    {
      if(as_label(enquo(group)) != "NULL") {
        left_join(
          x = tbl_0,
          y = tbl_stat %>%
            pivot_wider(id_cols = {{group}}, names_from = parameter, values_from = value)
        )
      } else {
        bind_cols(
          tbl_0,
          tbl_stat %>%
            pivot_wider(id_cols = {{group}}, names_from = parameter, values_from = value)
        )
      }
    }

  # create BA-plot
  gg_ba <-
    tbl_1 %>%
    ggplot(mapping = aes(x = avg, y = dfce, colour = {{colour}}, shape = {{shape}})) +
    {
      if(as_label(enquo(group)) != "NULL") facet_wrap(facets = vars({{group}}), scales = "free")
    } +
    geom_hline(yintercept = 0, size = 1, colour = "#0091DF") +
    geom_point(size = 3, alpha = 0.5) +
    {
      if(as_label(enquo(label)) != "NULL") {
        geom_text(
          mapping     = aes(label = if_else(dfce < lloa | dfce > uloa, as.character({{label}}), "")),
          size        = 2,
          show.legend = FALSE
        )
      }
    } +
    geom_hline(
      mapping     = aes(yintercept = value, linetype = ltyp, size = lsiz),
      data        = tbl_stat,
      show.legend = FALSE
    ) +
    geom_text(
      mapping     = aes(x = tx, y = ty, label = tlbl),
      size        = 2,
      colour      = "#808080",
      hjust       = "right",
      vjust       = "bottom",
      inherit.aes = FALSE,
      data        = tbl_stat
    ) +
    scale_size_manual(values = c(1, 0.5)) +
    labs(
      x = xlab, y = ylab, colour = NULL, shape = NULL,
      title = title, caption = caption
    ) +
    theme(panel.grid = element_blank(), legend.position = "bottom")

  return(gg_ba)
}
