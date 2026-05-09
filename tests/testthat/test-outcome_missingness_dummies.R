# tests/testthat/test-outcome_missingness_dummies.R

test_that("correctly selects Y_columns", {
  # Deterministic example data
  dat <- data.frame(
     Y_attitude = rep(c(1, 2, 3, 4, 5, NA), c(10, 20, 30, 40, 50, 50)),
     Y_behavior = rep(c(0, 1, NA), c(100, 50, 50)),
     X = rep(1, 200)
  )

  code <- write_outcome_missingness_dummies_code(dat)

  expected <-
    "dat <-
  dat |>
  mutate(
    Y_attitude_missing = if_else(is.na(Y_attitude), 1, 0),
    Y_behavior_missing = if_else(is.na(Y_behavior), 1, 0)
  )"

  expect_equal(code, expected)
  expect_type(code, "character")
  expect_true(length(code) == 1)
})

test_that("dataset name is dynamically used", {
  mydata <- data.frame(Y = c(1, NA, 3))
  code <- write_outcome_missingness_dummies_code(mydata, Y)

  expect_match(code, "^mydata <-")
})


test_that("ignores columns ending with _missing when auto-selecting", {
  dat <- data.frame(
    Y_a = c(1, NA, 2),
    Y_a_missing = c(0, 1, 0)
  )

  code <- write_outcome_missingness_dummies_code(dat)

  expect_true(grepl("Y_a_missing", code))
  expect_false(grepl("Y_a_missing_missing", code)) # previously _missing excluded
})

test_that("explicit columns override auto-selection", {
  dat <- data.frame(
    Y_a = c(1, NA, 2),
    Y_b = c(3, NA, 5)
  )

  code <- write_outcome_missingness_dummies_code(dat, Y_b)

  expect_true(grepl("Y_b_missing", code))
  expect_false(grepl("Y_a_missing", code))
})

test_that("warns and returns NULL when no Y_ variables found", {
  dat <- data.frame(X_age = 1:3)
  expect_warning(res <- write_outcome_missingness_dummies_code(dat), "No variables selected")
  expect_null(res)
})

