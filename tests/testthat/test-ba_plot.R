tbl <-
  temperature %>%
  tidyr::pivot_wider(names_from = method, values_from = temperature)

gg <- ba_plot(data = tbl, var1 = rectal, var2 = infrared)

test_that("error: no input", {expect_error(ba_plot())})

test_that("silent", {expect_silent(object = gg)})

test_that("type list", {expect_type(object = gg, type = "list")})

test_that("S3 class gg", {expect_s3_class(object = gg, class = "gg")})

test_that("S3 class ggplot", {expect_s3_class(object = gg, class = "ggplot")})
