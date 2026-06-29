#' @title Bland-Altman plot
#'
#' @description Plot Bland-Altman statistics
#'
#' @param data A data frame
#' @param var1 1st variable to compare (unquoted)
#' @param var2 2nd variable to compare (unquoted)
#' @param label data label (unquoted)
#' @param group grouping variable used for faceting (unquoted)
#' @param colour colour aesthetic for scatter points (unquoted)
#' @param shape shape aesthetic for scatter points (unquoted)
#' @param xlab The text for the x-axis label
#' @param ylab The text for the y-axis label
#' @param title plot title
#' @param caption plot caption
#' @param alpha alpha level for the intervals
#' @param point_size size of the scatter points (passed to [ggplot2::geom_point])
#' @param point_alpha opacity of the scatter points (passed to [ggplot2::geom_point])
#'
#' @return [ggplot2::ggplot] object
#'
#' @seealso [ba_stat]
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' tbl <- temperature %>% pivot_wider(names_from = method, values_from = temperature)
#'
#' # simple example
#' ba_plot(data = tbl, var1 = infrared, var2 = rectal)
#'
#' # with colors
#' ba_plot(data = tbl, var1 = infrared, var2 = rectal, colour = visit)
#'
#' # with colors and faceting
#' ba_plot(data = tbl, var1 = infrared, var2 = rectal, group = treatment, colour = visit)
ba_plot <- function(
  data    = stop("data must be specified"),
  var1    = stop("variable must be specified"),
  var2    = stop("variable must be specified"),
  label   = NULL,
  group   = NULL,
  colour  = NULL,
  shape   = NULL,
  xlab    = "Average",
  ylab    = "Difference",
  title   = NULL,
  caption = NULL,
  alpha   = 0.05,
  point_size  = 3,
  point_alpha = 0.5
) {

  stopifnot(inherits(data, "data.frame"))

  v1 <- tryCatch(dplyr::pull(data, {{ var1 }}), error = function(e) NULL)
  v2 <- tryCatch(dplyr::pull(data, {{ var2 }}), error = function(e) NULL)
  if (!is.null(v1) && !is.numeric(v1))
    rlang::abort("`var1` must refer to a numeric column.")
  if (!is.null(v2) && !is.numeric(v2))
    rlang::abort("`var2` must refer to a numeric column.")

  glbl <- rlang::as_label(rlang::enquo(group))
  glbl <- if (glbl == "NULL") as.character() else glbl

  # compute differences and averages between paired obs
  tbl_0 <-
    data %>%
    dplyr::mutate(
      dfce = {{var1}} - {{var2}},       # difference
      avg  = ({{var1}} + {{var2}}) / 2  # average
    )

  # derive all relevant statistics: bias, LoA, and confidence intervals
  tbl_stat <-
    ba_stat(data = data, var1 = {{var1}}, var2 = {{var2}}, group = {{group}}, alpha = alpha) %>%
    dplyr::mutate(
      ltyp = stringr::str_detect(string = parameter, pattern = "(lcl|ucl)$"),
      lsiz = (parameter != "bias")
    ) %>%
    dplyr::left_join(
      y  = tbl_0 %>%
        dplyr::group_by({{group}}) %>%
        dplyr::summarise(
          xmn = min(avg, na.rm = TRUE),
          xmx = max(avg, na.rm = TRUE),
          ymn = min(dfce, na.rm = TRUE),
          ymx = max(dfce, na.rm = TRUE),
          .groups = "keep"
        ) %>%
        dplyr::ungroup(),
      by = glbl
    ) %>%
    dplyr::left_join(
      y  = {.} %>%
        dplyr::group_by({{group}}) %>%
        dplyr::summarise(scale = {
          nz <- abs(value[value != 0])
          if (length(nz) == 0L) 0 else ceiling(log10(min(nz)))
        }) %>%
        dplyr::ungroup(),
      by = glbl
    ) %>%
    dplyr::mutate(
      tx    = xmx + .12 * (xmx-xmn),
      ty    = value + (ymx-ymn) * .01,
      aux   = 3 - scale,
      dplac = dplyr::case_when(
        aux < 0 ~ 0,
        aux > 3 ~ 3,
        TRUE ~ aux
      ),
      tlbl  = sprintf(fmt = paste0("%.", dplac, "f"), value)
    )

  tbl_1 <-
    dplyr::left_join(
      x  = tbl_0,
      y  = tbl_stat %>%
        tidyr::pivot_wider(id_cols = {{group}}, names_from = parameter, values_from = value),
      by = glbl
    )

  # create BA-plot
  gg_point <-
    tbl_1 %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = avg, y = dfce, colour = {{colour}}, shape = {{shape}})) +
    {
      if(length(glbl) > 0)
        ggplot2::facet_wrap(facets = ggplot2::vars({{group}}), scales = "free")
    } +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1, colour = "#0091DF") +
    ggplot2::geom_point(size = point_size, alpha = point_alpha)

  gg_ba <-
    gg_point +
    ggplot2::geom_hline(
      mapping     = ggplot2::aes(yintercept = value, linetype = ltyp, linewidth = lsiz),
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
    ggplot2::scale_linewidth_manual(values = c(1, 0.5)) +
    ggplot2::labs(
      x = xlab, y = ylab, colour = NULL, shape = NULL,
      title = title, caption = caption
    ) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")

  return(gg_ba)
}
