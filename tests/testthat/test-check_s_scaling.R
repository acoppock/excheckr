# tests/testthat/test-check_s_scaling.R

make_scaled_dat <- function() {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    D_belief_01 = c(0.2, 0.4, 0.3, 0.6, 0.8, 0.7),
    Y_attitude_01 = c(0.3, 0.5, 0.4, 0.4, 0.6, 0.5)
  )
  scale_by_control(dat, treatment = "Z")
}

# --- Return structure ---

test_that("returns a tibble with expected columns", {
  result <- check_s_scaling(make_scaled_dat(), treatment = "Z")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("variable", "control_sd", "treatment_sd", "sd_ratio", "control_sd_ok") %in% names(result)))
  expect_false("study_id" %in% names(result))
})

test_that("study_id appended when provided", {
  result <- check_s_scaling(make_scaled_dat(), treatment = "Z", study_id = "test_study")
  expect_true("study_id" %in% names(result))
  expect_equal(unique(result$study_id), "test_study")
})

# --- control_sd_ok ---

test_that("control_sd_ok is TRUE after scale_by_control", {
  result <- check_s_scaling(make_scaled_dat(), treatment = "Z")
  expect_true(all(result$control_sd_ok))
})

test_that("control_sd is 1.0 within tolerance", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  dat <- scale_by_control(dat, treatment = "Z")
  result <- check_s_scaling(dat, treatment = "Z")
  expect_equal(result$control_sd[[1]], 1.0, tolerance = 1e-10)
})

test_that("control_sd_ok is FALSE for incorrectly scaled variable", {
  # Values 1, 3, 5 in control group give SD = 2, not 1
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x_s = c(1.0, 3.0, 5.0, 7.0, 9.0, 11.0)
  )
  result <- check_s_scaling(dat, treatment = "Z")
  expect_false(result$control_sd_ok)
})

# --- sd_ratio ---

test_that("sd_ratio equals treatment_sd / control_sd", {
  result <- check_s_scaling(make_scaled_dat(), treatment = "Z")
  expect_equal(result$sd_ratio, result$treatment_sd / result$control_sd, tolerance = 1e-10)
})

# --- auto-selection ---

test_that("auto-selects D_ and Y_ _s columns", {
  result <- check_s_scaling(make_scaled_dat(), treatment = "Z")
  expect_setequal(result$variable, c("D_belief_s", "Y_attitude_s"))
})

# --- explicit outcomes ---

test_that("explicit outcomes override auto-selection", {
  dat <- make_scaled_dat()
  result <- check_s_scaling(dat, treatment = "Z", outcomes = "D_belief_s")
  expect_equal(result$variable, "D_belief_s")
  expect_equal(nrow(result), 1)
})

# --- custom control_value ---

test_that("custom control_value works", {
  dat <- data.frame(
    Z = c(1L, 1L, 1L, 2L, 2L, 2L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  dat <- scale_by_control(dat, treatment = "Z", control_value = 1)
  result <- check_s_scaling(dat, treatment = "Z", control_value = 1)
  expect_true(all(result$control_sd_ok))
})

# --- custom prefixes ---

test_that("custom prefixes argument works", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    O_outcome = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  dat <- scale_by_control(dat, treatment = "Z", prefixes = "O_")
  result <- check_s_scaling(dat, treatment = "Z", prefixes = "O_")
  expect_equal(result$variable, "O_outcome_s")
})

# --- edge cases ---

test_that("warns and returns NULL when no _s variables found", {
  dat <- data.frame(
    Z = c(0L, 0L, 0L, 1L, 1L, 1L),
    Y_x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )
  expect_warning(result <- check_s_scaling(dat, treatment = "Z"), "_s")
  expect_null(result)
})

test_that("a control_value matching no row errors rather than returning NA", {
  dat <- data.frame(Z = factor(rep(c("C", "T"), 3)), Y_x_s = rnorm(6))
  expect_error(check_s_scaling(dat, treatment = "Z"), "no rows have")
})
