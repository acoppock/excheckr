# tests/testthat/test-check_balance.R

make_balance_dat <- function(seed = 42, n = 100) {
  set.seed(seed)
  data.frame(
    Z       = rep(c(0L, 1L), n / 2),
    X_age   = rnorm(n, 50, 10),
    X_income = rnorm(n, 50000, 10000)
  )
}

# --- Return structure ----------------------------------------------------------

test_that("returns a named list with covariate_tests and joint_test", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z)

  expect_type(res, "list")
  expect_named(res, c("covariate_tests", "joint_test"))
})

test_that("covariate_tests has expected columns", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z)

  expect_true(all(c("covariate", "level", "F_stat", "df1", "df2", "p_value", "nobs") %in%
                    names(res$covariate_tests)))
})

test_that("joint_test is a single-row tibble with expected columns", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z)

  expect_equal(nrow(res$joint_test), 1)
  expect_true(all(c("F_stat", "df1", "df2", "p_value", "nobs") %in%
                    names(res$joint_test)))
})

# --- Covariate counts ---------------------------------------------------------

test_that("one row in covariate_tests per numeric covariate", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z, covariates = c("X_age", "X_income"))

  expect_equal(nrow(res$covariate_tests), 2)
  expect_equal(as.character(res$covariate_tests$covariate), c("X_age", "X_income"))
})

test_that("auto-selects X_ columns when covariates is NULL", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z)

  expect_setequal(
    as.character(res$covariate_tests$covariate),
    c("X_age", "X_income")
  )
})

test_that("factor covariate produces one row per non-reference level", {
  set.seed(1)
  n <- 120
  dat <- data.frame(
    Z      = rep(c(0L, 1L), n / 2),
    X_pid  = factor(rep(c("D", "R", "I"), n / 3))
  )

  res <- check_balance(dat, Z, covariates = "X_pid")

  # factor with 3 levels → 2 non-reference dummies
  expect_equal(nrow(res$covariate_tests), 2)
  expect_equal(unique(as.character(res$covariate_tests$covariate)), "X_pid")
  expect_false(any(is.na(res$covariate_tests$level)))
})

# --- Multi-armed treatment ----------------------------------------------------

test_that("three-arm treatment produces a valid joint_test", {
  set.seed(2)
  n <- 150
  dat <- data.frame(
    Z     = rep(c("control", "treat1", "treat2"), n / 3),
    X_age = rnorm(n, 50, 10)
  )

  res <- check_balance(dat, Z)

  expect_equal(nrow(res$joint_test), 1)
  expect_true(res$joint_test$F_stat >= 0)
  expect_true(res$joint_test$p_value >= 0 & res$joint_test$p_value <= 1)
})

# --- Console output ------------------------------------------------------------

test_that("produces no console output", {
  dat <- make_balance_dat()
  expect_silent(check_balance(dat, Z))
})

# --- Edge cases ----------------------------------------------------------------

test_that("warns and returns NULL when no covariates are found", {
  dat <- data.frame(Z = 0:1, Y = c(1.0, 2.0))

  expect_warning(res <- check_balance(dat, Z), "No covariates selected")
  expect_null(res)
})
