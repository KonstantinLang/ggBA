tbl <-
  temperature %>%
  tidyr::pivot_wider(names_from = method, values_from = temperature)

test_that("error: no input", {expect_error(ba_mean_diff())})

test_that("error: non-data-frame input", {
  expect_error(ba_mean_diff(data = list(a = 1:3, b = 1:3), var1 = a, var2 = b))
})

test_that("identity transform adds avg and dfce columns", {
  result <- ba_mean_diff(tbl, var1 = rectal, var2 = infrared)
  expect_true(all(c("avg", "dfce") %in% names(result)))
})

test_that("identity: avg equals (var1 + var2) / 2", {
  result <- ba_mean_diff(tbl, var1 = rectal, var2 = infrared)
  expected_avg <- (tbl$rectal + tbl$infrared) / 2
  expect_equal(result$avg, expected_avg, tolerance = 1e-10)
})

test_that("identity: dfce equals var1 - var2", {
  result <- ba_mean_diff(tbl, var1 = rectal, var2 = infrared)
  expected_dfce <- tbl$rectal - tbl$infrared
  expect_equal(result$dfce, expected_dfce, tolerance = 1e-10)
})

test_that("log transform: dfce equals log(var1) - log(var2)", {
  result <- ba_mean_diff(tbl, var1 = rectal, var2 = infrared, transform = "log")
  expected_dfce <- log(tbl$rectal) - log(tbl$infrared)
  expect_equal(result$dfce, expected_dfce, tolerance = 1e-10)
})

test_that("logit transform returns finite values for proportions in (0,1)", {
  set.seed(42)
  prop_tbl <- data.frame(p1 = runif(20, 0.1, 0.9), p2 = runif(20, 0.1, 0.9))
  result <- ba_mean_diff(prop_tbl, var1 = p1, var2 = p2, transform = "logit")
  expect_true(all(is.finite(result$dfce)))
})

test_that("invalid transform raises error", {
  expect_error(ba_mean_diff(tbl, var1 = rectal, var2 = infrared, transform = "sqrt"))
})

test_that("number of rows unchanged", {
  result <- ba_mean_diff(tbl, var1 = rectal, var2 = infrared)
  expect_equal(nrow(result), nrow(tbl))
})
