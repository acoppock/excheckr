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
  expect_named(res, c("simple", "coefficients", "f_test"))
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

test_that("outcome with no missingness is reported as unestimable, not as p = 1", {
  set.seed(6)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y = rnorm(n)  # no NAs
  )

  res <- check_attrition(dat, Z, outcomes = "Y")

  expect_false(res$estimable)
  expect_true(is.na(res$F_stat))
  expect_true(is.na(res$p_value))
  # p = 1 would put a spike at 1 into the uniform-reference diagnostics, which
  # is what summarize_check_pvalues then reads as a badly non-uniform collection
  expect_false(isTRUE(res$p_value == 1))
})

test_that("outcome missing for everyone is also unestimable", {
  n <- 100
  dat <- data.frame(Z = rep(c(0L, 1L), n / 2), Y = NA_real_)

  res <- check_attrition(dat, Z, outcomes = "Y")

  expect_false(res$estimable)
  expect_true(is.na(res$p_value))
})

test_that("unestimable rows are dropped by the p-value summaries and counted", {
  set.seed(61)
  n <- 100
  dat <- data.frame(Z = rep(c(0L, 1L), n / 2), Y_none = rnorm(n), Y_some = rnorm(n))
  dat$Y_some[1:20] <- NA

  res <- check_attrition(dat, Z)
  summary <- summarize_check_pvalues(res)

  expect_equal(summary$n_tests, 1)
  expect_equal(summary$n_dropped, 1)
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
  expect_named(res, c("simple", "coefficients", "f_test"))
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

test_that("covariates path: zero-attrition outcome is unestimable, not p = 1", {
  set.seed(13)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    X_age = rnorm(n, 50, 10),
    Y = rnorm(n)
  )

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = "X_age")

  frow <- res$f_test[res$f_test$outcome == "Y", ]
  expect_false(frow$estimable)
  expect_true(is.na(frow$F_stat))
  expect_true(is.na(frow$p_value))
})

test_that("covariates path: a covariate prefixed by the treatment name is not tested as treatment", {
  set.seed(131)
  n <- 400
  dat <- data.frame(Z = rep(c(0L, 1L), n / 2), Zeal = rnorm(n), X_age = rnorm(n))
  dat$Y <- rnorm(n)
  dat$Y[1:40] <- NA

  res <- check_attrition(dat, Z, outcomes = "Y", covariates = c("Zeal", "X_age"))

  # Z plus its two interactions: 3 terms. Prefix matching would have counted
  # Zeal_c as a treatment term and reported 4.
  expect_equal(res$f_test$df1, 3L)
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
  expect_named(res, c("simple", "coefficients", "f_test"))
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

test_that("estimable holds the invariant estimable == !is.na(p_value)", {
  set.seed(71)
  n <- 200
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n))
  dat$Y_none <- rnorm(n)                       # no attrition: unestimable
  dat$Y_some <- rnorm(n); dat$Y_some[1:40] <- NA
  dat$Y_all <- NA_real_                        # everybody missing: unestimable

  res <- check_attrition(dat, Z, outcomes = c("Y_none", "Y_some", "Y_all"))
  expect_equal(res$estimable, !is.na(res$p_value))
  expect_equal(res$estimable, c(FALSE, TRUE, FALSE))

  lin <- check_attrition(dat, Z, outcomes = c("Y_none", "Y_some", "Y_all"),
                         covariates = "X_age")$f_test
  expect_equal(lin$estimable, !is.na(lin$p_value))
})

# --- Both tests returned from one call ----------------------------------------

test_that("covariates path also returns the covariate-free test", {
  set.seed(81)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n), X_inc = rnorm(n))
  dat$Y_a <- rnorm(n)
  dat$Y_a[rbinom(n, 1, 0.1 + 0.15 * dat$Z) == 1] <- NA

  res <- check_attrition(dat, Z, outcomes = "Y_a", covariates = c("X_age", "X_inc"))

  expect_named(res, c("simple", "coefficients", "f_test"))
  expect_equal(nrow(res$simple), 1)
  expect_equal(res$simple$df1, 1L)   # binary treatment: one degree of freedom
  expect_equal(res$f_test$df1, 3L)   # treatment plus two interactions
})

test_that("the returned simple test is identical to calling without covariates", {
  set.seed(82)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n))
  dat$Y_a <- rnorm(n); dat$Y_a[1:60] <- NA
  dat$Y_b <- rnorm(n); dat$Y_b[1:30] <- NA

  standalone <- check_attrition(dat, Z, covariates = NULL)
  bundled <- check_attrition(dat, Z, covariates = "X_age")$simple

  expect_equal(standalone, bundled)
})

test_that("study_id is appended to all three elements", {
  set.seed(83)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n))
  dat$Y_a <- rnorm(n); dat$Y_a[1:40] <- NA

  res <- check_attrition(dat, Z, outcomes = "Y_a", covariates = "X_age",
                         study_id = "smith_2024_study_1")

  for (el in c("simple", "coefficients", "f_test")) {
    expect_true("study_id" %in% names(res[[el]]))
    expect_equal(unique(res[[el]]$study_id), "smith_2024_study_1")
  }
})

test_that("the simple test survives an outcome the interacted test cannot fit", {
  set.seed(84)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n))
  dat$X_dup <- dat$X_a                        # rank-deficient interacted model
  dat$Y_a <- rnorm(n); dat$Y_a[1:40] <- NA

  res <- suppressWarnings(
    check_attrition(dat, Z, outcomes = "Y_a", covariates = c("X_a", "X_dup"))
  )
  expect_false(res$f_test$estimable)           # interacted test defeated
  expect_true(res$simple$estimable)            # covariate-free test still fine
  expect_false(is.na(res$simple$p_value))
})

# --- status distinguishes a pass from a missing test --------------------------

test_that("status separates no_attrition from the uninformative cases", {
  set.seed(91)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2))
  dat$Y_clean <- rnorm(n)                       # nobody missing: a pass
  dat$Y_all <- NA_real_                         # everybody missing: uninformative
  dat$Y_some <- rnorm(n); dat$Y_some[1:60] <- NA

  res <- check_attrition(dat, Z, outcomes = c("Y_clean", "Y_all", "Y_some"))

  expect_equal(res$status, c("no_attrition", "all_missing", "tested"))
  expect_equal(res$estimable, res$status == "tested")
  expect_equal(res$n_missing, c(0L, n, 60L))
})

test_that("n_missing lets both rates be computed without the raw data", {
  set.seed(92)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2))
  dat$Y_clean <- rnorm(n)
  dat$Y_some <- rnorm(n); dat$Y_some[1:60] <- NA

  res <- check_attrition(dat, Z)

  # corpus-health denominator: everything except the uninformative rows
  informative <- res[res$status %in% c("tested", "no_attrition"), ]
  expect_equal(nrow(informative), 2)

  # uniformity denominator: tested rows only
  expect_equal(sum(res$status == "tested"), 1)
})

test_that("a no-attrition row is a pass, not a gap, in the corpus-health rate", {
  set.seed(93)
  n <- 200
  dat <- data.frame(Z = rep(0:1, n / 2))
  for (j in 1:9) dat[[paste0("Y_clean", j)]] <- rnorm(n)
  dat$Y_bad <- rnorm(n)
  dat$Y_bad[rbinom(n, 1, 0.05 + 0.4 * dat$Z) == 1] <- NA

  res <- check_attrition(dat, Z)
  flagged <- sum(res$p_value <= 0.05, na.rm = TRUE)

  expect_equal(flagged, 1)
  # counting the nine clean outcomes as passes gives 1/10, not 1/1
  informative <- sum(res$status %in% c("tested", "no_attrition"))
  expect_equal(informative, 10)
  expect_equal(flagged / informative, 0.1)
  expect_equal(flagged / sum(res$status == "tested"), 1)
})

test_that("the Lin path's f_test also carries status", {
  set.seed(94)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n))
  dat$Y_clean <- rnorm(n)
  dat$Y_some <- rnorm(n); dat$Y_some[1:60] <- NA

  res <- check_attrition(dat, Z, covariates = "X_age")

  expect_true("status" %in% names(res$f_test))
  expect_equal(res$f_test$status[res$f_test$outcome == "Y_clean"], "no_attrition")
  expect_equal(res$f_test$status[res$f_test$outcome == "Y_some"], "tested")
  expect_equal(res$f_test$estimable, res$f_test$status == "tested")
})
