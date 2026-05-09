# tests/testthat/test-scale_by_control.R

make_scale_dat <- function() {
  data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    D_belief_01 = c(0.2, 0.4, 0.3, 0.6, 0.8, 0.7),
    Y_attitude_01 = c(0.3, 0.5, 0.4, 0.4, 0.6, 0.5)
  )
}

# --- Basic behavior ---

test_that("appends _s columns for D_ and Y_ prefixes", {
  result <- scale_by_control(make_scale_dat(), treatment = "Z")
  expect_true("D_belief_s" %in% names(result))
  expect_true("Y_attitude_s" %in% names(result))
})

test_that("control-group SD of _s variable is 1.0", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  result <- scale_by_control(dat, treatment = "Z")
  ctrl_sd <- sd(result$Y_x_s[result$Z == 0])
  expect_equal(ctrl_sd, 1.0, tolerance = 1e-10)
})

test_that("_01 suffix is stripped from _s variable name", {
  result <- scale_by_control(make_scale_dat(), treatment = "Z")
  expect_true("D_belief_s" %in% names(result))
  expect_false("D_belief_01_s" %in% names(result))
  expect_true("Y_attitude_s" %in% names(result))
  expect_false("Y_attitude_01_s" %in% names(result))
})

test_that("returns data frame with same number of rows", {
  dat <- make_scale_dat()
  result <- scale_by_control(dat, treatment = "Z")
  expect_equal(nrow(result), nrow(dat))
})

test_that("original columns are preserved", {
  dat <- make_scale_dat()
  result <- scale_by_control(dat, treatment = "Z")
  expect_true(all(names(dat) %in% names(result)))
})

# --- custom control_value ---

test_that("custom control_value works", {
  dat <- data.frame(
    Z = c(1L, 1L, 1L, 2L, 2L, 2L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  result <- scale_by_control(dat, treatment = "Z", control_value = 1)
  expect_true("Y_x_s" %in% names(result))
  ctrl_sd <- sd(result$Y_x_s[result$Z == 1])
  expect_equal(ctrl_sd, 1.0, tolerance = 1e-10)
})

# --- explicit outcomes ---

test_that("explicit outcomes override auto-selection", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
    Y_y = c(2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
  )
  result <- scale_by_control(dat, treatment = "Z", outcomes = "Y_x")
  expect_true("Y_x_s" %in% names(result))
  expect_false("Y_y_s" %in% names(result))
})

# --- auto-selection exclusions ---

test_that("_missing and _s columns are excluded from auto-selection", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
    Y_x_missing = c(0L, 0L, 0L, 0L, 0L, 0L),
    Y_already_s = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
  )
  result <- scale_by_control(dat, treatment = "Z")
  expect_false("Y_x_missing_s" %in% names(result))
  expect_false("Y_already_s_s" %in% names(result))
})

# --- warnings for degenerate control groups ---

test_that("warns and skips when control SD is zero", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(5.0, 5.0, 5.0, 6.0, 7.0, 8.0)
  )
  expect_warning(result <- scale_by_control(dat, treatment = "Z"), "zero")
  expect_false("Y_x_s" %in% names(result))
})

test_that("warns and skips when control SD is NA", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(NA_real_, NA_real_, NA_real_, 1.0, 2.0, 3.0)
  )
  expect_warning(result <- scale_by_control(dat, treatment = "Z"), "NA")
  expect_false("Y_x_s" %in% names(result))
})

# --- custom prefixes ---

test_that("custom prefixes argument works", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    O_outcome = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  result <- scale_by_control(dat, treatment = "Z", prefixes = "O_")
  expect_true("O_outcome_s" %in% names(result))
})
