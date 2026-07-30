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

# --- .method portability (fit$k was estimatr-specific) -------------------------

test_that(".method = stats::lm populates both tables", {
  set.seed(91)
  n <- 200
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n), X_inc = rnorm(n),
                    X_party = factor(sample(c("D", "R", "I"), n, TRUE)))

  res <- check_balance(dat, Z, .method = stats::lm)

  # fit$k is NULL on an lm, and NULL - 1L is integer(0), which silently
  # collapsed the surrounding tibble to zero rows
  expect_gt(nrow(res$covariate_tests), 0)
  expect_equal(nrow(res$joint_test), 1)
  expect_false(is.na(res$joint_test$p_value))
  expect_true(all(res$covariate_tests$df1 >= 1))
})

test_that("df1 matches the estimatr-specific formula on full-rank fits", {
  set.seed(92)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))
  res <- check_balance(dat, Z)

  fit <- estimatr::lm_robust(X_a ~ Z, data = dat)
  expect_equal(res$covariate_tests$df1[1], as.integer(fit$k - 1L))
})

test_that("an aliased term is charged no degree of freedom", {
  set.seed(93)
  n <- 200
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n))
  dat$X_dup <- dat$X_a                       # exactly collinear
  fit <- estimatr::lm_robust(Z ~ X_a + X_dup, data = dat)
  expect_lt(model_df1(fit), fit$k - 1L)
  expect_equal(model_df1(fit), 1L)
})

# --- Joint test guard against a failed matrix inversion -----------------------

test_that("the joint test is suppressed when Wald and classical p-values diverge", {
  # The numerical pathology this guards against is not reliably reproducible from
  # simulated data; it was found on four real study datasets and is verified
  # against them. What is testable here is the guard mechanism: forcing the
  # threshold to zero makes any ordinary gap between the two forms trip it.
  set.seed(94)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n), X_c = rnorm(n))

  expect_warning(
    res <- check_balance(dat, Z, max_orders_apart = -Inf, quiet = TRUE),
    "joint test suppressed"
  )
  expect_true(is.na(res$joint_test$p_value))
  expect_false(res$joint_test$estimable)
  # the covariate-by-covariate tests are unaffected by the joint-test guard
  expect_equal(nrow(res$covariate_tests), 3)
  expect_true(all(!is.na(res$covariate_tests$p_value)))
})

test_that("the suppression warning names both p-values and the conditioning", {
  set.seed(941)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))

  expect_warning(
    check_balance(dat, Z, max_orders_apart = -Inf, quiet = TRUE),
    "classical F p-value"
  )
  expect_warning(
    check_balance(dat, Z, max_orders_apart = -Inf, quiet = TRUE),
    "condition number"
  )
})

test_that("a well-conditioned joint test is left alone", {
  set.seed(95)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n), X_c = rnorm(n))

  expect_silent(res <- check_balance(dat, Z))
  expect_false(is.na(res$joint_test$p_value))
  expect_true(res$joint_test$estimable)
})

test_that("max_orders_apart = Inf disables the guard", {
  set.seed(96)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))

  expect_silent(
    res <- check_balance(dat, Z, max_orders_apart = Inf, quiet = TRUE)
  )
  expect_false(is.na(res$joint_test$p_value))
})

test_that("classical_f_pvalue agrees with lm's own F test", {
  set.seed(97)
  n <- 300
  dat <- data.frame(.Zn = rbinom(n, 1, 0.5), X_a = rnorm(n), X_b = rnorm(n))
  dat$.Zn <- as.numeric(0.3 * dat$X_a > rnorm(n))

  fl <- stats::lm(.Zn ~ X_a + X_b, data = dat)
  expect_equal(classical_f_pvalue(fl, dat$.Zn), unname(broom::glance(fl)$p.value),
               tolerance = 1e-10)

  # lm_robust shares OLS point estimates, so the classical F built from its
  # fitted values matches lm's exactly
  fr <- estimatr::lm_robust(.Zn ~ X_a + X_b, data = dat)
  expect_equal(classical_f_pvalue(fr, dat$.Zn), unname(broom::glance(fl)$p.value),
               tolerance = 1e-10)
})

test_that("classical_f_pvalue aligns on the rows a fit actually used", {
  set.seed(98)
  n <- 200
  dat <- data.frame(.Zn = rbinom(n, 1, 0.5), X_a = rnorm(n), X_b = rnorm(n))
  dat$X_a[1:20] <- NA                       # the fit drops 20 rows

  fl <- stats::lm(.Zn ~ X_a + X_b, data = dat)
  expect_equal(classical_f_pvalue(fl, dat$.Zn), unname(broom::glance(fl)$p.value),
               tolerance = 1e-10)
})
