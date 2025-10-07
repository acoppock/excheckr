# tests/testthat/test-mode.R

test_that("numeric mode works", {
  x <- c(1, 2, 2, 3, 3, 3, NA)
  expect_equal(mode(x), 3)
})

test_that("numeric mode handles NAs with na.rm = TRUE", {
  x <- c(1, 1, 2, NA, NA)
  expect_equal(mode(x, na.rm = TRUE), 1)
})

test_that("numeric mode treats NA as a value when na.rm = FALSE", {
  x <- c(1, 1, 2, NA, NA, NA)
  result <- mode(x, na.rm = FALSE)
  expect_true(is.na(result))
  expect_type(result, "double")  # numeric NA
})

test_that("character mode works", {
  x <- c("a", "b", "a", "c")
  expect_equal(mode(x), "a")
})

test_that("factor mode works and preserves levels", {
  x <- factor(c("low", "high", "low", NA), levels = c("low", "high"))
  result <- mode(x)
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), "low")
  expect_equal(levels(result), c("low", "high"))
})

test_that("all missing returns NA of appropriate type", {
  expect_true(is.na(mode(c(NA, NA))))
})

test_that("empty input returns NA", {
  expect_true(is.na(mode(numeric(0))))
})

test_that("ties return first occurring value", {
  x <- c(1, 2, 1, 2)  # tie between 1 and 2
  expect_equal(mode(x), 1)
})
