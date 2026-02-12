test_that("r4sub_theme returns a bs_theme object", {
  theme <- r4sub_theme()
  expect_s3_class(theme, "bs_theme")
})

test_that("r4sub_theme accepts custom colors", {
  theme <- r4sub_theme(primary = "#000000", danger = "#FF0000")
  expect_s3_class(theme, "bs_theme")
})

test_that("band_colors returns named character vector", {
  bc <- r4subui:::band_colors()
  expect_true(is.character(bc))
  expect_true("ready" %in% names(bc))
  expect_true("high_risk" %in% names(bc))
})

test_that("result_colors returns named character vector", {
  rc <- r4subui:::result_colors()
  expect_true(is.character(rc))
  expect_true(all(c("pass", "warn", "fail", "na") %in% names(rc)))
})

test_that("severity_colors returns named character vector", {
  sc <- r4subui:::severity_colors()
  expect_true(is.character(sc))
  expect_true(all(c("info", "low", "medium", "high", "critical") %in% names(sc)))
})
