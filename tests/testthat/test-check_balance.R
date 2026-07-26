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

test_that("three-arm treatment produces a valid joint_test (multinomial LR)", {
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
  # df2 should be NA for multinomial LR test
  expect_true(is.na(res$joint_test$df2))
})

test_that("binary treatment joint test is unchanged (F-test)", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z)

  expect_equal(nrow(res$joint_test), 1)
  expect_true(res$joint_test$F_stat >= 0)
  expect_true(res$joint_test$p_value >= 0 & res$joint_test$p_value <= 1)
  # df2 should NOT be NA for binary F-test

  expect_false(is.na(res$joint_test$df2))
})

# --- Console output ------------------------------------------------------------

test_that("produces no console output (binary treatment)", {
  dat <- make_balance_dat()
  expect_silent(check_balance(dat, Z))
})

test_that("produces no console output (multi-arm treatment)", {
  set.seed(2)
  n <- 150
  dat <- data.frame(
    Z     = rep(c("control", "treat1", "treat2"), n / 3),
    X_age = rnorm(n, 50, 10)
  )
  expect_silent(check_balance(dat, Z))
})

# --- Singular matrix robustness -----------------------------------------------

test_that("constant covariate within group returns NA joint_test with a warning", {
  n <- 30
  dat <- data.frame(
    Z     = rep(c("control", "treat1", "treat2"), n / 3),
    X_age = rep(50, n)  # zero variance: all same value
  )

  expect_warning(res <- check_balance(dat, Z))

  expect_equal(nrow(res$joint_test), 1)
  expect_true(is.na(res$joint_test$F_stat))
  expect_true(is.na(res$joint_test$p_value))
})

# --- Empty treatment arm after complete-case filtering ------------------------

test_that("empty arm after complete-case filtering returns NA joint_test with a warning", {
  set.seed(1)
  n <- 30
  dat <- data.frame(
    Z     = factor(c(rep("control", 10), rep("treat1", 10), rep("treat2", 10))),
    X_age = c(rnorm(10, 50, 10), rnorm(10, 50, 10), rep(NA_real_, 10))
  )

  expect_warning(
    res <- check_balance(dat, Z),
    regexp = "empty after removing incomplete cases"
  )

  expect_equal(nrow(res$joint_test), 1)
  expect_true(is.na(res$joint_test$F_stat))
  expect_true(is.na(res$joint_test$p_value))
})

# --- Edge cases ----------------------------------------------------------------

test_that("warns and returns NULL when no covariates are found", {
  dat <- data.frame(Z = 0:1, Y = c(1.0, 2.0))

  expect_warning(res <- check_balance(dat, Z), "No covariates selected")
  expect_null(res)
})

# --- Tidyselect covariates path -----------------------------------------------

test_that("tidyselect expression for covariates works", {
  dat <- make_balance_dat()
  res <- check_balance(dat, Z, covariates = dplyr::starts_with("X_"))
  expect_setequal(
    as.character(res$covariate_tests$covariate),
    c("X_age", "X_income")
  )
})

# --- character (non-factor) covariate ----------------------------------------

test_that("character covariate is converted to factor before dummy expansion", {
  set.seed(5)
  n <- 120
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_region = rep(c("North", "South", "East"), n / 3)  # character, not factor
  )
  res <- check_balance(dat, Z, covariates = "X_region")
  expect_equal(unique(as.character(res$covariate_tests$covariate)), "X_region")
  expect_equal(nrow(res$covariate_tests), 2)  # 3 levels → 2 dummies
})

# --- quiet = FALSE output -----------------------------------------------------

test_that("quiet = FALSE produces output for binary treatment", {
  dat <- make_balance_dat()
  expect_output(check_balance(dat, Z, quiet = FALSE))
})

test_that("quiet = FALSE produces output for multi-arm treatment", {
  set.seed(2)
  n <- 150
  dat <- data.frame(
    Z = rep(c("control", "treat1", "treat2"), n / 3),
    X_age = rnorm(n, 50, 10)
  )
  expect_output(check_balance(dat, Z, quiet = FALSE))
})

# --- Randomization inference --------------------------------------------------

test_that("RI path with declaration returns a valid p-value", {
  skip_if_not_installed("ri2")
  skip_if_not_installed("randomizr")

  set.seed(3)
  n <- 60
  dat <- data.frame(
    Z     = randomizr::complete_ra(n, conditions = c("C", "T1", "T2")),
    X_age = rnorm(n, 50, 10)
  )

  decl <- randomizr::declare_ra(N = n, conditions = c("C", "T1", "T2"))
  res <- check_balance(dat, Z, declaration = decl, sims = 100)

  expect_equal(nrow(res$joint_test), 1)
  expect_true(res$joint_test$p_value >= 0 & res$joint_test$p_value <= 1)
  expect_true(is.na(res$joint_test$df1))
  expect_true(is.na(res$joint_test$df2))
})


# study_id, flatten, and the "complete" declaration shorthand ----

balance_fixture <- function(seed = 42) {
  set.seed(seed)
  dat <- data.frame(
    Z        = rep(c(0L, 1L), 100),
    X_age    = rnorm(200, 50, 10),
    X_party  = factor(sample(c("D", "R", "I"), 200, replace = TRUE))
  )
  dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)
  dat
}

test_that("study_id is appended to both returned tibbles", {
  out <- check_balance(balance_fixture(), Z, study_id = "smith_2024_study_1")

  expect_true(all(out$covariate_tests$study_id == "smith_2024_study_1"))
  expect_equal(out$joint_test$study_id, "smith_2024_study_1")
})

test_that("study_id is absent by default", {
  out <- check_balance(balance_fixture(), Z)
  expect_false("study_id" %in% names(out$covariate_tests))
  expect_false("study_id" %in% names(out$joint_test))
})

test_that("flatten stacks the covariate and joint tests into one tibble", {
  nested <- check_balance(balance_fixture(), Z)
  flat <- check_balance(balance_fixture(), Z, flatten = TRUE)

  expect_s3_class(flat, "data.frame")
  expect_equal(nrow(flat), nrow(nested$covariate_tests) + 1)
  expect_equal(sum(flat$test == "joint"), 1)
  expect_true(is.na(flat$covariate[flat$test == "joint"]))
  expect_equal(
    flat$p_value[flat$test == "joint"],
    nested$joint_test$p_value
  )
})

test_that("flatten carries study_id through", {
  flat <- check_balance(balance_fixture(), Z, study_id = "s1", flatten = TRUE)
  expect_true(all(flat$study_id == "s1"))
})

test_that("declaration = 'complete' runs randomization inference", {
  skip_if_not_installed("randomizr")
  skip_if_not_installed("ri2")

  set.seed(9)
  dat <- data.frame(
    Z     = factor(rep(c("C", "T1", "T2"), each = 60)),
    X_age = rnorm(180)
  )
  out <- check_balance(dat, Z, declaration = "complete", sims = 25)

  expect_equal(nrow(out$joint_test), 1)
  expect_gte(out$joint_test$p_value, 0)
  expect_lte(out$joint_test$p_value, 1)
  expect_true(is.na(out$joint_test$df1))
})


# the statistic column ----

test_that("binary treatment reports a genuine F statistic", {
  out <- check_balance(balance_fixture(), Z)
  expect_equal(out$joint_test$statistic, "F")
  expect_true(all(out$covariate_tests$statistic == "F"))
  expect_true(is.finite(out$joint_test$df2))
})

test_that("multi-arm treatment reports LR/df, not an F", {
  set.seed(1)
  dat <- data.frame(
    Z = factor(rep(c("C", "T1", "T2"), length.out = 201)),
    X_age = rnorm(201, 50, 10)
  )
  out <- check_balance(dat, Z)

  expect_equal(out$joint_test$statistic, "LR/df")
  expect_true(is.na(out$joint_test$df2))
  # covariate-by-covariate tests remain genuine F tests
  expect_true(all(out$covariate_tests$statistic == "F"))
})

test_that("the RI path reports a raw LR statistic", {
  skip_if_not_installed("randomizr")
  skip_if_not_installed("ri2")

  set.seed(9)
  dat <- data.frame(
    Z = factor(rep(c("C", "T1", "T2"), each = 60)),
    X_age = rnorm(180)
  )
  out <- check_balance(dat, Z, declaration = "complete", sims = 25)

  expect_equal(out$joint_test$statistic, "LR")
  expect_true(is.na(out$joint_test$df1))
  expect_true(is.na(out$joint_test$df2))
})

test_that("statistic distinguishes the paths once results are stacked", {
  set.seed(3)
  binary <- check_balance(balance_fixture(), Z, study_id = "a", flatten = TRUE)
  multi <- check_balance(
    data.frame(Z = factor(rep(c("C", "T1", "T2"), length.out = 201)),
               X_age = rnorm(201)),
    Z, study_id = "b", flatten = TRUE
  )
  stacked <- rbind(binary, multi)
  joint <- stacked[stacked$test == "joint", ]

  # Without `statistic` these two rows would be indistinguishable quantities
  # sitting in one F_stat column.
  expect_equal(sort(joint$statistic), c("F", "LR/df"))
})
