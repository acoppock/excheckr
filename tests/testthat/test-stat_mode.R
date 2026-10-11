# tests/testthat/test-stat_mode.R

test_that("numeric mode works", {
  x <- c(1, 2, 2, 3, 3, 3, NA)
  expect_equal(stat_mode(x), 3)
})

test_that("numeric mode handles NAs with na.rm = TRUE", {
  x <- c(1, 1, 2, NA, NA)
  expect_equal(stat_mode(x, na.rm = TRUE), 1)
})

test_that("numeric mode treats NA as a value when na.rm = FALSE", {
  x <- c(1, 1, 2, NA, NA, NA)
  result <- stat_mode(x, na.rm = FALSE)
  expect_true(is.na(result))
  expect_type(result, "double")  # numeric NA
})

test_that("character mode works", {
  x <- c("a", "b", "a", "c")
  expect_equal(stat_mode(x), "a")
})

test_that("factor mode works and preserves levels", {
  x <- factor(c("low", "high", "low", NA), levels = c("low", "high"))
  result <- stat_mode(x)
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), "low")
  expect_equal(levels(result), c("low", "high"))
})

test_that("all missing returns NA of appropriate type", {
  expect_true(is.na(stat_mode(c(NA, NA))))
})

test_that("empty input returns NA", {
  expect_true(is.na(stat_mode(numeric(0))))
})

test_that("ties return first occurring value", {
  x <- c(1, 2, 1, 2)  # tie between 1 and 2
  expect_equal(stat_mode(x), 1)
})

test_that("na.rm = FALSE: non-NA value wins when it is more frequent than NA", {
  x <- c(1, 1, 1, NA, NA)
  result <- stat_mode(x, na.rm = FALSE)
  expect_equal(result, 1)
})
test_that("na.rm = FALSE: an NA tied for most frequent is returned", {
  expect_true(is.na(stat_mode(c(1, 1, NA, NA), na.rm = FALSE)))
  expect_true(is.na(stat_mode(c(NA, NA, 1, 1), na.rm = FALSE)))
})

test_that("na.rm = FALSE: ties among values break by first occurrence", {
  expect_equal(stat_mode(c(2, 2, 1, 1, NA), na.rm = FALSE), 2)
})

test_that("na.rm = FALSE keeps the type of character and factor input", {
  expect_equal(stat_mode(c("a", "a", "a", NA), na.rm = FALSE), "a")
  expect_identical(stat_mode(c("a", NA, NA), na.rm = FALSE), NA_character_)

  x <- factor(c("low", "low", NA), levels = c("low", "high"))
  result <- stat_mode(x, na.rm = FALSE)
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), "low")
  expect_equal(levels(result), c("low", "high"))
})
