# tests/testthat/test-check_attrition_lasso.R

skip_if_not_installed("glmnet")
skip_if_not_installed("estimatr")

make_lasso_dat <- function(seed = 42, n = 300) {
  set.seed(seed)
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    X_income = rnorm(n, 50000, 10000)
  )
  dat$Y_outcome <- 0.3 * dat$Z + 0.5 * scale(dat$X_age)[, 1] + rnorm(n)
  dat$Y_outcome[sample(n, 30)] <- NA
  dat
}

# --- Return structure ---

test_that("returns a tibble with expected columns", {
  dat <- make_lasso_dat()
  res <- check_attrition_lasso(dat, Z,
    outcomes = "Y_outcome", covariates = c("X_age", "X_income"))
  expect_s3_class(res, "data.frame")
  expected_cols <- c(
    "outcome", "n_assigned", "n_missing", "pct_missing",
    "p_simple", "n_lasso1", "n_lasso2", "n_selected",
    "selected_covariates", "df1", "epv", "epv_adequate",
    "p_interacted", "flag_simple", "flag_interacted", "flag"
  )
  expect_true(all(expected_cols %in% names(res)))
})

test_that("one row per outcome", {
  dat <- make_lasso_dat()
  set.seed(1)
  dat$Y_second <- rnorm(nrow(dat))
  dat$Y_second[sample(nrow(dat), 20)] <- NA

  res <- check_attrition_lasso(dat, Z,
    outcomes = c("Y_outcome", "Y_second"),
    covariates = c("X_age", "X_income"))
  expect_equal(nrow(res), 2)
  expect_setequal(res$outcome, c("Y_outcome", "Y_second"))
})

# --- p_simple validity ---

test_that("p_simple is a valid p-value", {
  dat <- make_lasso_dat()
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome")
  expect_true(res$p_simple >= 0 & res$p_simple <= 1)
})

# --- Zero missing: trivial short-circuit ---

test_that("zero missing returns trivial row with p_simple = 1 and flag = FALSE", {
  dat <- make_lasso_dat()
  dat$Y_complete <- rnorm(nrow(dat))
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_complete",
    covariates = c("X_age"))
  expect_equal(res$n_missing, 0L)
  expect_equal(res$p_simple, 1)
  expect_false(res$flag)
  expect_false(res$epv_adequate)
})

# --- Auto-selection of Y_ columns ---

test_that("auto-selects Y_ columns when outcomes is NULL", {
  dat <- make_lasso_dat()
  names(dat)[names(dat) == "Y_outcome"] <- "Y_x"
  res <- check_attrition_lasso(dat, Z, covariates = c("X_age", "X_income"))
  expect_equal(res$outcome, "Y_x")
})

# --- NA treatment rows dropped ---

test_that("drops unassigned rows (NA treatment)", {
  dat <- make_lasso_dat()
  dat$Z[1:5] <- NA
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome")
  expect_equal(res$n_assigned, nrow(dat) - 5L)
})

# --- Missing covariate columns ---

test_that("warns about covariate columns not found in data", {
  dat <- make_lasso_dat()
  expect_warning(
    res <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome",
      covariates = c("X_age", "nonexistent_col")),
    "dropping covariates not found"
  )
})

# --- No covariates: only simple test ---

test_that("no covariates: epv_adequate FALSE, p_interacted NA, n_selected 0", {
  dat <- make_lasso_dat()
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome", covariates = NULL)
  expect_false(res$epv_adequate)
  expect_true(is.na(res$p_interacted))
  expect_equal(res$n_selected, 0L)
})

# --- flag is OR of flag_simple and flag_interacted ---

test_that("flag equals flag_simple OR flag_interacted", {
  dat <- make_lasso_dat()
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome",
    covariates = c("X_age"))
  expect_equal(res$flag, res$flag_simple | res$flag_interacted)
})

# --- Console output ---

test_that("quiet = TRUE (default) produces no output", {
  dat <- make_lasso_dat()
  expect_silent(
    check_attrition_lasso(dat, Z, outcomes = "Y_outcome",
      covariates = c("X_age"))
  )
})

test_that("quiet = FALSE produces output", {
  dat <- make_lasso_dat()
  expect_output(
    check_attrition_lasso(dat, Z, outcomes = "Y_outcome", quiet = FALSE)
  )
})

# --- No outcomes found ---

test_that("warns and returns NULL when no outcomes found", {
  dat <- data.frame(Z = 0:1, X_age = c(25, 30))
  expect_warning(res <- check_attrition_lasso(dat, Z), "No outcomes selected")
  expect_null(res)
})

# --- lasso_se argument ---

test_that("lasso_se = lambda.min runs without error", {
  dat <- make_lasso_dat()
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome",
    covariates = c("X_age", "X_income"),
    lasso_se = "lambda.min")
  expect_s3_class(res, "data.frame")
})

# --- Very few missing (fewer than 5): LASSO 2 skipped ---

test_that("fewer than 5 missing: n_lasso2 is 0", {
  set.seed(7)
  dat <- make_lasso_dat(n = 300)
  dat$Y_rare <- rnorm(300)
  dat$Y_rare[1:3] <- NA  # only 3 missing
  res <- check_attrition_lasso(dat, Z, outcomes = "Y_rare",
    covariates = c("X_age", "X_income"))
  expect_equal(res$n_lasso2, 0L)
})


# study_id ----

test_that("check_attrition_lasso appends study_id", {
  skip_if_not_installed("glmnet")

  set.seed(42)
  n <- 500
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    X_income = rnorm(n, 50000, 10000)
  )
  dat$Y_outcome <- 0.3 * dat$Z + 0.5 * scale(dat$X_age) + rnorm(n)
  dat$Y_outcome[which(rbinom(n, 1, 0.1) == 1)] <- NA

  out <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome",
                               covariates = c("X_age", "X_income"),
                               study_id = "smith_2024_study_1")
  expect_true(all(out$study_id == "smith_2024_study_1"))

  bare <- check_attrition_lasso(dat, Z, outcomes = "Y_outcome",
                                covariates = c("X_age", "X_income"))
  expect_false("study_id" %in% names(bare))
})
