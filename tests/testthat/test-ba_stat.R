tbl <-
  temperature %>%
  tidyr::pivot_wider(names_from = method, values_from = temperature)

tbl_stat <- ba_stat(data = tbl, var1 = rectal, var2 = infrared)

test_that("error: no input", {expect_error(ba_stat())})

test_that("error: non-data-frame input", {
  expect_error(ba_stat(data = list(a = 1), var1 = a, var2 = a))
})

test_that("error: alpha out of range", {
  expect_error(ba_stat(data = tbl, var1 = rectal, var2 = infrared, alpha = 1.5))
  expect_error(ba_stat(data = tbl, var1 = rectal, var2 = infrared, alpha = 0))
})

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

test_that("bias equals mean of differences", {
  expected_bias <- mean(tbl$rectal - tbl$infrared, na.rm = TRUE)
  actual_bias   <- tbl_stat$value[tbl_stat$parameter == "bias"]
  expect_equal(actual_bias, expected_bias, tolerance = 1e-10)
})

test_that("LoA are symmetric around bias", {
  bias <- tbl_stat$value[tbl_stat$parameter == "bias"]
  lloa <- tbl_stat$value[tbl_stat$parameter == "lloa"]
  uloa <- tbl_stat$value[tbl_stat$parameter == "uloa"]
  expect_equal(bias - lloa, uloa - bias, tolerance = 1e-10)
})

test_that("n equals number of complete observations", {
  n_expected <- sum(!is.na(tbl$rectal - tbl$infrared))
  n_actual   <- unique(tbl_stat$n)
  expect_equal(n_actual, n_expected)
})

test_that("grouping: returns one set of statistics per group level", {
  tbl_grouped <- ba_stat(data = tbl, var1 = rectal, var2 = infrared, group = treatment)
  n_groups    <- dplyr::n_distinct(tbl$treatment)
  n_params    <- length(unique(tbl_stat$parameter))
  expect_equal(nrow(tbl_grouped), n_groups * n_params)
  expect_true("treatment" %in% names(tbl_grouped))
})

# --- Edge cases ---

test_that("all-zero differences: bias and LoA are zero, no NaN/Inf", {
  tbl_zero <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
  result   <- ba_stat(data = tbl_zero, var1 = x, var2 = y)
  bias     <- result$value[result$parameter == "bias"]
  expect_equal(bias, 0)
  expect_true(all(is.finite(result$value)))
})

test_that("single observation: n=1 produces a result without error", {
  tbl_one <- data.frame(x = 5, y = 3)
  expect_no_error(ba_stat(data = tbl_one, var1 = x, var2 = y))
  result <- ba_stat(data = tbl_one, var1 = x, var2 = y)
  expect_equal(result$value[result$parameter == "bias"], 2)
})

test_that("all-NA input: n=0, bias is NaN, function does not crash", {
  tbl_na <- data.frame(x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_))
  expect_no_error(ba_stat(data = tbl_na, var1 = x, var2 = y))
  result <- ba_stat(data = tbl_na, var1 = x, var2 = y)
  expect_equal(unique(result$n), 0L)
  expect_true(is.nan(result$value[result$parameter == "bias"]))
})

test_that("mixed NA/non-NA: n counts only complete observations", {
  tbl_mix <- data.frame(x = c(1, 2, NA, 4), y = c(1, 2, 3, NA))
  result  <- ba_stat(data = tbl_mix, var1 = x, var2 = y)
  expect_equal(unique(result$n), 2L)
})
