# tests/testthat/test-check_attrition.R

make_attrition_dat <- function(seed = 42) {
  set.seed(seed)
  n <- 150
  dat <- data.frame(
    Z_party = rep(c("No cue", "Democrat cue", "Republican cue"), n / 3),
    Y       = rnorm(n)
  )
  dat$Y[sample(n, 20)] <- NA
  dat
}

# --- Return structure -----------------------------------------------------------

test_that("returns a data frame with expected columns", {
  dat <- make_attrition_dat()
  res <- check_attrition(dat, Z_party, outcomes = "Y")

  expect_s3_class(res, "data.frame")
  expect_true(all(c("outcome", "F_stat", "df1", "df2", "p_value", "nobs") %in% names(res)))
})

# --- Binary (numeric 0/1) treatment --------------------------------------------

test_that("binary numeric treatment: one row per outcome", {
  set.seed(1)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )

  res <- check_attrition(dat, Z, outcomes = "Y")

  expect_equal(nrow(res), 1)
  expect_equal(res$outcome, "Y")
  expect_true(res$F_stat >= 0)
  expect_true(res$p_value >= 0 & res$p_value <= 1)
})

# --- Multi-level character treatment ------------------------------------------

test_that("multi-arm treatment: one row per outcome with omnibus F-stat", {
  dat <- make_attrition_dat()
  res <- check_attrition(dat, Z_party, outcomes = "Y")

  # omnibus F-test collapses all arms into one row per outcome
  expect_equal(nrow(res), 1)
  expect_equal(res$outcome, "Y")
  expect_true(res$F_stat >= 0)
  expect_true(res$p_value >= 0 & res$p_value <= 1)
})

test_that("result has no term column (omnibus, not arm-by-arm)", {
  dat <- make_attrition_dat()
  res <- check_attrition(dat, Z_party, outcomes = "Y")

  expect_false("term" %in% names(res))
})

# --- Outcome selection ---------------------------------------------------------

test_that("auto-selects Y columns (including bare Y) when outcomes is NULL", {
  set.seed(2)
  n <- 100
  dat <- data.frame(
    Z   = rep(c(0L, 1L), n / 2),
    Y_a = rnorm(n),
    Y_b = rnorm(n)
  )
  dat$Y_a[1:10] <- NA
  dat$Y_b[1:15] <- NA

  res <- check_attrition(dat, Z)

  expect_setequal(unique(res$outcome), c("Y_a", "Y_b"))
})

test_that("bare Y column is found by auto-selection", {
  set.seed(5)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y = c(rnorm(90), rep(NA, 10))
  )

  res <- check_attrition(dat, Z)

  expect_equal(res$outcome, "Y")
})

test_that("explicit character outcomes override auto-selection", {
  set.seed(3)
  n <- 100
  dat <- data.frame(
    Z   = rep(c(0L, 1L), n / 2),
    Y_a = rnorm(n),
    Y_b = rnorm(n)
  )
  dat$Y_b[1:10] <- NA

  res <- check_attrition(dat, Z, outcomes = "Y_b")

  expect_equal(unique(res$outcome), "Y_b")
  expect_false("Y_a" %in% res$outcome)
})

# --- Tidyselect outcomes path -------------------------------------------------

test_that("tidyselect expression for outcomes works", {
  set.seed(7)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y_a = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5)),
    Y_b = rnorm(n)
  )
  res <- check_attrition(dat, Z, outcomes = dplyr::starts_with("Y_a"))
  expect_equal(unique(res$outcome), "Y_a")
})

# --- Tidyselect covariates path -----------------------------------------------

test_that("tidyselect expression for covariates works", {
  set.seed(8)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )
  res <- check_attrition(dat, Z, outcomes = "Y",
    covariates = dplyr::starts_with("X_"))
  expect_type(res, "list")
  expect_named(res, c("coefficients", "f_test"))
})

# --- Console output ------------------------------------------------------------

test_that("produces no console output by default", {
  dat <- make_attrition_dat()
  expect_silent(check_attrition(dat, Z_party, outcomes = "Y"))
})

test_that("quiet = FALSE produces output", {
  dat <- make_attrition_dat()
  expect_output(check_attrition(dat, Z_party, outcomes = "Y", quiet = FALSE))
})

# --- Zero attrition ------------------------------------------------------------

test_that("outcome with no missingness returns F_stat = 0 and p_value = 1", {
  set.seed(6)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y = rnorm(n)  # no NAs
  )

  res <- check_attrition(dat, Z, outcomes = "Y")

  expect_equal(res$F_stat, 0)
  expect_equal(res$p_value, 1)
})

# --- Edge cases ----------------------------------------------------------------

test_that("warns and returns NULL when no outcomes are found", {
  dat <- data.frame(Z = 0:1, X_age = c(25, 30))

  expect_warning(res <- check_attrition(dat, Z), "No outcomes selected")
  expect_null(res)
})

test_that("uses existing Y_missing column when present", {
  set.seed(4)
  n <- 100
  dat <- data.frame(
    Z         = rep(c(0L, 1L), n / 2),
    Y         = rnorm(n),
    Y_missing = as.integer(sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)))
  )

  res <- check_attrition(dat, Z, outcomes = "Y")

  expect_equal(nrow(res), 1)
  expect_equal(res$outcome, "Y")
})

# --- Covariates path (Lin model + F-test) -------------------------------------

test_that("covariates path returns list with coefficients and f_test", {
  set.seed(10)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = "X_age")

  expect_type(res, "list")
  expect_named(res, c("coefficients", "f_test"))
})

test_that("coefficients tibble has outcome and term columns", {
  set.seed(11)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = "X_age")

  expect_true(all(c("outcome", "term", "estimate") %in% names(res$coefficients)))
})

test_that("f_test has one row per outcome", {
  set.seed(12)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y_a = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5)),
    Y_b = c(rnorm(40), rep(NA, 10), rnorm(40), rep(NA, 10))
  )

  res <- check_attrition(dat, Z, outcomes = c("Y_a", "Y_b"), covariates = "X_age")

  expect_equal(nrow(res$f_test), 2)
  expect_setequal(res$f_test$outcome, c("Y_a", "Y_b"))
})

test_that("covariates path: zero-attrition outcome yields F_stat = 0 and p_value = 1", {
  set.seed(13)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = rnorm(n)
  )

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = "X_age")

  frow <- res$f_test[res$f_test$outcome == "Y", ]
  expect_equal(frow$F_stat, 0)
  expect_equal(frow$p_value, 1)
})

test_that("covariates path: quiet = FALSE produces console output", {
  set.seed(14)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )

  expect_output(
    check_attrition(dat, Z, outcomes = "Y", covariates = "X_age", quiet = FALSE)
  )
})

test_that("covariates path: empty character covariates warns and falls back to simple", {
  set.seed(15)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )

  expect_warning(
    res <- check_attrition(dat, Z, outcomes = "Y", covariates = character(0)),
    "No covariates selected"
  )
  expect_s3_class(res, "data.frame")
})

test_that("covariates path: uses existing _missing column", {
  set.seed(16)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = rnorm(n),
    Y_missing = as.integer(sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)))
  )

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = "X_age")

  expect_type(res, "list")
  expect_named(res, c("coefficients", "f_test"))
})

test_that("covariates path: f_test p_value is a valid p-value", {
  set.seed(17)
  n <- 120
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = c(rnorm(55), rep(NA, 5), rnorm(55), rep(NA, 5))
  )

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = "X_age")

  expect_true(res$f_test$p_value >= 0 & res$f_test$p_value <= 1)
})


# study_id ----

attrition_fixture <- function(seed = 42) {
  set.seed(seed)
  n <- 300
  dat <- data.frame(Z = rep(c(0L, 1L), n / 2), X_age = rnorm(n, 50, 10))
  dat$Y_attitude <- rnorm(n)
  dat$Y_attitude[which(rbinom(n, 1, ifelse(dat$Z == 1, 0.3, 0.1)) == 1)] <- NA
  dat
}

test_that("check_attrition appends study_id in the simple branch", {
  out <- check_attrition(attrition_fixture(), Z, outcomes = "Y_attitude",
                         study_id = "smith_2024_study_1")
  expect_true(all(out$study_id == "smith_2024_study_1"))
})

test_that("check_attrition appends study_id in the covariate branch", {
  out <- check_attrition(attrition_fixture(), Z, outcomes = "Y_attitude",
                         covariates = "X_age", study_id = "smith_2024_study_1")
  expect_true(all(out$coefficients$study_id == "smith_2024_study_1"))
  expect_true(all(out$f_test$study_id == "smith_2024_study_1"))
})

test_that("check_attrition omits study_id by default", {
  out <- check_attrition(attrition_fixture(), Z, outcomes = "Y_attitude")
  expect_false("study_id" %in% names(out))
})
