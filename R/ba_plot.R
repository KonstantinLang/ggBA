#' Bland-Altman plot
#'
#' Plot Bland-Altman statistics
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
#' @seealso [ba_stat]
#'
#' @export
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
    dplyr::mutate(
      dfce = {{var1}} - {{var2}},       # difference
      avg  = ({{var1}} + {{var2}}) / 2  # average
    )

  # derive all relevant statistics: bias, LoA, and confidence intervals
  tbl_stat <-
    BAplot::ba_stat(data = data, var1 = {{var1}}, var2 = {{var2}}, group = {{group}}, alpha = alpha) %>%
    dplyr::mutate(
      ltyp = stringr::str_detect(string = parameter, pattern = "\\."),
      lsiz = (parameter != "bias")
    ) %>%
    {
      if(rlang::as_label(rlang::enquo(group)) != "NULL") {
        dplyr::left_join(
          x = .,
          y = tbl_0 %>%
            dplyr::group_by({{group}}) %>%
            dplyr::summarise(
              xmn = min(avg, na.rm = TRUE),
              xmx = max(avg, na.rm = TRUE),
              ymn = min(dfce, na.rm = TRUE),
              ymx = max(dfce, na.rm = TRUE),
              .groups = "keep"
            ) %>%
            dplyr::ungroup()
        ) %>%
          dplyr::left_join(
            y = {.} %>%
              dplyr::group_by({{group}}) %>%
              dplyr::summarise(scale = ceiling(log10(min(abs(value))))) %>%
              dplyr::ungroup()
          )
      } else {
        dplyr::bind_cols(
          .,
          tbl_0 %>%
            dplyr::summarise(
              xmn = min(avg, na.rm = TRUE),
              xmx = max(avg, na.rm = TRUE),
              ymn = min(dfce, na.rm = TRUE),
              ymx = max(dfce, na.rm = TRUE)
            ),
          {.} %>% dplyr::summarise(scale = ceiling(log10(min(abs(value)))))
        )
      }
    } %>%
    dplyr::mutate(
      tx    = xmx + .12 * (xmx-xmn),
      ty    = value + (ymx-ymn) * .01,
      aux   = 3 - scale,
      dplac = dplyr::if_else(aux < 0, 0, aux) %>% if_else(. > 3, 3, .),
      tlbl  = sprintf(fmt = paste0("%.", dplac, "f"), value)
    )

  tbl_1 <-
    {
      if(rlang::as_label(rlang::enquo(group)) != "NULL") {
        dplyr::left_join(
          x = tbl_0,
          y = tbl_stat %>%
            tidyr::pivot_wider(id_cols = {{group}}, names_from = parameter, values_from = value)
        )
      } else {
        dplyr::bind_cols(
          tbl_0,
          tbl_stat %>%
            tidyr::pivot_wider(id_cols = {{group}}, names_from = parameter, values_from = value)
        )
      }
    }

  # create BA-plot
  gg_ba <-
    tbl_1 %>%
    ggplot2::ggplot(mapping = aes(x = avg, y = dfce, colour = {{colour}}, shape = {{shape}})) +
    {
      if(rlang::as_label(rlang::enquo(group)) != "NULL") ggplot2::facet_wrap(facets = vars({{group}}), scales = "free")
    } +
    ggplot2::geom_hline(yintercept = 0, size = 1, colour = "#0091DF") +
    ggplot2::geom_point(size = 3, alpha = 0.5) +
    {
      if(rlang::as_label(rlang::enquo(label)) != "NULL") {
        ggplot2::geom_text(
          mapping     = ggplot2::aes(label = if_else(dfce < lloa | dfce > uloa, as.character({{label}}), "")),
          size        = 2,
          show.legend = FALSE
        )
      }
    } +
    ggplot2::geom_hline(
      mapping     = ggplot2::aes(yintercept = value, linetype = ltyp, size = lsiz),
      data        = tbl_stat,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      mapping     = ggplot2::aes(x = tx, y = ty, label = tlbl),
      size        = 2,
      colour      = "#808080",
      hjust       = "right",
      vjust       = "bottom",
      inherit.aes = FALSE,
      data        = tbl_stat
    ) +
    ggplot2::scale_size_manual(values = c(1, 0.5)) +
    ggplot2::labs(
      x = xlab, y = ylab, colour = NULL, shape = NULL,
      title = title, caption = caption
    ) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")

  return(gg_ba)
}
