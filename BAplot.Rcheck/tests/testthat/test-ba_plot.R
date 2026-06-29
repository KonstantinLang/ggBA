tbl <-
  temperature %>%
  tidyr::pivot_wider(names_from = method, values_from = temperature)

gg <- ba_plot(data = tbl, var1 = rectal, var2 = infrared)

test_that("error: no input", {expect_error(ba_plot())})

test_that("error: non-data-frame input", {
  expect_error(ba_plot(data = list(a = 1:3, b = 1:3), var1 = a, var2 = b))
})

test_that("silent", {expect_silent(object = gg)})

test_that("type list", {expect_type(object = gg, type = "list")})

test_that("S3 class gg", {expect_s3_class(object = gg, class = "gg")})

test_that("S3 class ggplot", {expect_s3_class(object = gg, class = "ggplot")})

test_that("custom axis labels are applied", {
  gg_lab <- ba_plot(data = tbl, var1 = rectal, var2 = infrared,
                    xlab = "Mean", ylab = "Diff")
  expect_equal(gg_lab$labels$x, "Mean")
  expect_equal(gg_lab$labels$y, "Diff")
})

test_that("grouping produces facets", {
  gg_grp <- ba_plot(data = tbl, var1 = rectal, var2 = infrared, group = treatment)
  expect_true(inherits(gg_grp$facet, "FacetWrap"))
})

test_that("alpha parameter changes CI width", {
  stat_90 <- ba_stat(data = tbl, var1 = rectal, var2 = infrared, alpha = 0.10)
  stat_05 <- ba_stat(data = tbl, var1 = rectal, var2 = infrared, alpha = 0.05)
  lloa_90 <- stat_90$value[stat_90$parameter == "lloa"]
  lloa_05 <- stat_05$value[stat_05$parameter == "lloa"]
  expect_true(lloa_90 > lloa_05)
})
