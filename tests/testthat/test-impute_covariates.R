# tests/testthat/test-write_covariate_imputation_code.R

test_that("numeric and factor variables produce correct imputation code", {
  # Deterministic example data
  dat <- data.frame(
    X_pid_3 = factor(rep(c("A", "B", NA), c(4, 5, 1))),
    X_income = c(100, 200, NA, 400, 500, NA, 700, 800, 900, NA)
  )

  code <- write_covariate_imputation_code(dat, X_pid_3, X_income)

  expected <-
    "dat <-
  dat |>
  mutate(
X_pid_3_nona = replace_na(X_pid_3, stat_mode(X_pid_3)),
X_pid_3_missing = if_else(is.na(X_pid_3), 1, 0),
X_income_nona = replace_na(X_income, median(X_income, na.rm = TRUE)),
X_income_missing = if_else(is.na(X_income), 1, 0)
  )"

  expect_equal(code, expected)
})

test_that("function invisibly returns a string", {
  dat <- data.frame(
    X_char = c("a", "b", NA),
    X_num = c(1, NA, 3)
  )

  result <- write_covariate_imputation_code(dat, X_char, X_num)

  expect_type(result, "character")
  expect_true(length(result) == 1)
})

test_that("function handles all-numeric input", {
  dat <- data.frame(X_num = c(1, 2, NA, 4))
  code <- write_covariate_imputation_code(dat, X_num)

  expect_match(code, "median\\(X_num, na.rm = TRUE\\)")
})

test_that("function handles all-character input", {
  dat <- data.frame(X_char = c("a", NA, "b"))
  code <- write_covariate_imputation_code(dat, X_char)

  expect_match(code, "stat_mode\\(X_char\\)")
})

test_that("dataset name is dynamically used", {
  mydata <- data.frame(X = c(1, NA, 3))
  code <- write_covariate_imputation_code(mydata, X)

  expect_match(code, "^mydata <-")
})


test_that("automatically selects X_ columns if none explicitly passed", {
  dat <- data.frame(
    X_a = c(1, 2, NA),
    X_b = c(NA, 4, 5),
    Y_c = c(1, 2, 3)  # should not be selected
  )

  code <- write_covariate_imputation_code(dat)

  expect_true(grepl("X_a_nona", code))
  expect_true(grepl("X_b_nona", code))
  expect_false(grepl("Y_c_nona", code))
})

test_that("ignores columns ending with _nona or _missing when auto-selecting", {
  dat <- data.frame(
    X_a = c(1, NA, 2),
    X_b_nona = c(3, 4, 5),
    X_c_missing = c(NA, 1, 2)
  )

  code <- write_covariate_imputation_code(dat)

  expect_true(grepl("X_a_nona", code))
  expect_false(grepl("X_b_nona_nona", code))       # previously _nona excluded
  expect_false(grepl("X_c_missing_nona", code))    # previously _missing excluded
})

test_that("explicit columns override auto-selection", {
  dat <- data.frame(
    X_a = c(1, NA, 2),
    X_b = c(3, NA, 5)
  )

  code <- write_covariate_imputation_code(dat, X_b)

  expect_true(grepl("X_b_nona", code))
  expect_false(grepl("X_a_nona", code))
})

