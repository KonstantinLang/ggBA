tbl <-
  temperature %>%
  tidyr::pivot_wider(names_from = method, values_from = temperature)

tbl_stat <- ba_stat(data = tbl, var1 = rectal, var2 = infrared)

test_that("error: no input", {expect_error(ba_stat())})

test_that("silent", {expect_silent(object = tbl_stat)})

test_that("type list", {expect_type(object = tbl_stat, type = "list")})

test_that("S3 class tbl_df", {expect_s3_class(object = tbl_stat, class = "tbl_df")})

test_that(
  "correct names",
  {expect_equal(object = names(tbl_stat), expected = c("n", "parameter", "value"))}
)

test_that(
  "correct parameters",
  {expect_equal(
    object   = unique(tbl_stat$parameter),
    expected = c("bias", "lloa", "uloa", "bias.lcl", "lloa.lcl", "uloa.lcl", "bias.ucl", "lloa.ucl", "uloa.ucl")
  )}
)
