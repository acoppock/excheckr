library(testthat)

test_that("explicit columns override auto-selection", {
  dat <- data.frame(
    X_a = c(1, NA, 2),
    X_b = c(3, NA, 5),
    X_c = c(NA, 1, 2)
  )

  result <- covariate_missingness(dat, X_b, X_c)

  expect_equal(as.character(result$summary$variable), c("X_b", "X_c"))
  expect_equal(as.integer(result$summary$total_missing_cases), c(1, 1))
  expect_equal(as.numeric(result$summary$fraction_missing_cases), c(1/3, 1/3), tolerance = 1e-8)
})

test_that("auto-selects X_ columns when none passed", {
  dat <- data.frame(
    X_a = c(1, NA, 2),
    X_b = c(3, NA, 5),
    Y_c = c(1, 2, 3)  # should not be selected
  )

  result <- covariate_missingness(dat)

  expect_equal(as.character(result$summary$variable), c("X_a", "X_b"))
  expect_equal(as.integer(result$summary$total_missing_cases), c(1, 1))
  expect_equal(as.numeric(result$summary$fraction_missing_cases), c(1/3, 1/3), tolerance = 1e-8)
})

test_that("columns ending with _nona or _missing are excluded from auto-selection", {
  dat <- data.frame(
    X_a = c(1, NA, 2),
    X_b_nona = c(3, 4, 5),
    X_c_missing = c(NA, 1, 2)
  )

  result <- covariate_missingness(dat)

  expect_equal(as.character(result$summary$variable), c("X_a"))
  expect_equal(as.integer(result$summary$total_missing_cases), c(1))
  expect_equal(as.numeric(result$summary$fraction_missing_cases), c(1/3), tolerance = 1e-8)
})

test_that("handles zero missing values correctly", {
  dat <- data.frame(
    X_a = c(1, 2, 3),
    X_b = c(4, 5, 6)
  )

  result <- covariate_missingness(dat)

  expect_equal(as.character(result$summary$variable), c("X_a", "X_b"))
  expect_equal(as.integer(result$summary$total_missing_cases), c(0, 0))
  expect_equal(as.numeric(result$summary$fraction_missing_cases), c(0, 0), tolerance = 1e-8)
})

test_that("returns NULL with warning if no variables selected", {
  dat <- data.frame(
    Y_a = c(1, 2, 3)
  )

  expect_warning(result <- covariate_missingness(dat), "No variables selected")
  expect_null(result)
})

