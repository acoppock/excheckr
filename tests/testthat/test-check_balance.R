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
    regexp = "lost every observation to missing covariate values"
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

test_that("a wide covariate battery warns and says to narrow it", {
  set.seed(94)
  n <- 60
  dat <- data.frame(Z = rep(0:1, n / 2))
  for (j in 1:8) dat[[paste0("X_", j)]] <- rnorm(n)   # 8 covariates, 36 variance params

  ws <- character(0)
  withCallingHandlers(
    res <- check_balance(dat, Z, quiet = TRUE),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("variance parameters", ws)))
  expect_true(any(grepl("Narrow the battery", ws)))

  # nothing is suppressed: the caller asked for this test and gets it
  expect_false(is.na(res$joint_test$p_value))
  expect_true(res$joint_test$estimable)
})

test_that("a comfortable ratio does not warn", {
  set.seed(941)
  n <- 600
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))
  expect_silent(check_balance(dat, Z, quiet = TRUE))
})

test_that("min_obs_per_vparam controls the warning and alters nothing", {
  set.seed(942)
  n <- 600
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))

  quiet_run <- check_balance(dat, Z, quiet = TRUE)
  noisy <- suppressWarnings(check_balance(dat, Z, min_obs_per_vparam = 1e6, quiet = TRUE))
  expect_warning(check_balance(dat, Z, min_obs_per_vparam = 1e6, quiet = TRUE),
                 "variance parameters")
  # the warning changes no number
  expect_equal(quiet_run$joint_test$p_value, noisy$joint_test$p_value)
})

test_that("p_value_classical is reported alongside, on every joint path", {
  set.seed(943)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))
  binary <- check_balance(dat, Z, quiet = TRUE)
  expect_true("p_value_classical" %in% names(binary$joint_test))
  expect_false(is.na(binary$joint_test$p_value_classical))

  # multi-arm uses a likelihood ratio, which inverts nothing, so there is no
  # counterpart and the column is NA rather than absent
  dat$Zm <- rep(c("C", "T1", "T2"), length.out = n)
  multi <- check_balance(dat, Zm, covariates = c("X_a", "X_b"), quiet = TRUE)
  expect_true("p_value_classical" %in% names(multi$joint_test))
  expect_true(is.na(multi$joint_test$p_value_classical))
})

test_that("the two p-values agree when the ratio is comfortable", {
  set.seed(944)
  n <- 2000
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n), X_c = rnorm(n))
  r <- check_balance(dat, Z, quiet = TRUE)
  expect_lt(abs(log10(r$joint_test$p_value) - log10(r$joint_test$p_value_classical)), 1)
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

# --- Aliased covariates: warn and suggest, never prune -------------------------

test_that("an exactly duplicated covariate warns and names its partner", {
  set.seed(101)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_female = rbinom(n, 1, 0.5), X_age = rnorm(n))
  dat$X_woman <- dat$X_female                      # literally the same column

  expect_warning(
    res <- check_balance(dat, Z, covariates = c("X_female", "X_age", "X_woman"),
                         quiet = TRUE),
    "rank deficient"
  )
  expect_warning(
    check_balance(dat, Z, covariates = c("X_female", "X_age", "X_woman"), quiet = TRUE),
    "X_woman is exactly determined by X_female"
  )
  # it suggests the set to keep, and does NOT prune
  expect_warning(
    check_balance(dat, Z, covariates = c("X_female", "X_age", "X_woman"), quiet = TRUE),
    'covariates = c\\("X_female", "X_age"\\)'
  )
  # every covariate the caller asked for is still tested individually
  expect_setequal(unique(res$covariate_tests$covariate),
                  c("X_female", "X_age", "X_woman"))
})

test_that("a coarsening of another covariate is identified", {
  set.seed(102)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2))
  dat$X_party_3 <- factor(sample(c("left", "center", "right"), n, TRUE))
  dat$X_republican <- as.integer(dat$X_party_3 == "right")   # determined by the factor

  expect_warning(
    check_balance(dat, Z, covariates = c("X_party_3", "X_republican"), quiet = TRUE),
    "exactly determined by"
  )
})

test_that("the warning is the only signal that a covariate was dropped", {
  # This is why warning matters more than the earlier framing suggested. lm_robust
  # does not refuse a rank-deficient design: it drops the aliased column and reports
  # an F on the remainder, so the caller gets a p-value for a covariate set they did
  # not specify, with nothing in the returned object to say so.
  set.seed(103)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n))
  dat$X_dup <- dat$X_a

  res <- suppressWarnings(
    check_balance(dat, Z, covariates = c("X_a", "X_dup"), quiet = TRUE)
  )
  reduced <- check_balance(dat, Z, covariates = "X_a", quiet = TRUE)

  # a p-value comes back, and it is the one for the REDUCED set
  expect_false(is.na(res$joint_test$p_value))
  expect_equal(res$joint_test$p_value, reduced$joint_test$p_value)
  expect_equal(res$joint_test$df1, reduced$joint_test$df1)

  # nothing in the result records the drop, hence the warning
  expect_true(res$joint_test$estimable)
})

test_that("an unobserved factor level makes the joint test unestimable outright", {
  set.seed(1031)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n))
  # a declared level with no observations yields an all-zero indicator column
  dat$X_f <- factor(sample(c("a", "b"), n, TRUE), levels = c("a", "b", "unused"))

  res <- suppressWarnings(
    check_balance(dat, Z, covariates = c("X_a", "X_f"), quiet = TRUE)
  )
  expect_true(is.na(res$joint_test$p_value) || res$joint_test$estimable)
})

test_that("a full-rank covariate set does not warn", {
  set.seed(104)
  n <- 300
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n),
                    X_f = factor(sample(c("x", "y", "z"), n, TRUE)))
  expect_silent(check_balance(dat, Z, quiet = TRUE))
})

test_that("check_attrition warns about aliased covariates too", {
  set.seed(105)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n))
  dat$X_dup <- dat$X_a
  dat$Y_o <- rnorm(n); dat$Y_o[1:60] <- NA

  ws <- character(0)
  withCallingHandlers(
    check_attrition(dat, Z, outcomes = "Y_o", covariates = c("X_a", "X_dup")),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("rank deficient", ws)))
  expect_true(any(grepl("X_dup is exactly determined by X_a", ws)))
})

# --- Unused treatment levels in a subset --------------------------------------

test_that("a declared-but-unused arm does not kill the joint test", {
  # The shape that broke a real corpus: a study-level factor declares five arms and
  # a party-by-topic subset uses three. No data is missing.
  set.seed(111)
  n <- 900
  dat <- data.frame(
    Z = factor(rep(c("none", "in_support", "out_support"), length.out = n),
               levels = c("none", "in_oppose", "in_support", "out_oppose", "out_support")),
    X_a = rnorm(n), X_b = rnorm(n), X_c = rnorm(n)
  )
  expect_equal(nlevels(dat$Z), 5)
  expect_equal(sum(table(dat$Z) > 0), 3)
  expect_false(anyNA(dat))

  res <- check_balance(dat, Z, quiet = TRUE)

  expect_false(is.na(res$joint_test$p_value))
  expect_true(res$joint_test$estimable)
  # df1 counts present arms, not declared ones: (3-1) * 3 covariates
  expect_equal(res$joint_test$df1, 6L)
})

test_that("an unused arm no longer produces a missing-data warning", {
  set.seed(112)
  n <- 600
  dat <- data.frame(
    Z = factor(rep(c("a", "b"), length.out = n), levels = c("a", "b", "never_used")),
    X_a = rnorm(n), X_b = rnorm(n)
  )
  expect_silent(check_balance(dat, Z, quiet = TRUE))
})

test_that("a two-arm subset of a multi-arm factor uses the binary F path", {
  set.seed(113)
  n <- 600
  dat <- data.frame(
    Z = factor(rep(c("a", "b"), length.out = n), levels = c("a", "b", "c", "d")),
    X_a = rnorm(n), X_b = rnorm(n)
  )
  res <- check_balance(dat, Z, quiet = TRUE)
  # declared levels would have routed this to the multinomial path
  expect_equal(res$joint_test$statistic, "F")
  expect_false(is.na(res$joint_test$df2))
})

test_that("an arm genuinely emptied by missing covariates still warns, and says so", {
  set.seed(114)
  n <- 300
  dat <- data.frame(
    Z = factor(rep(c("a", "b", "c"), length.out = n)),
    X_a = rnorm(n), X_b = rnorm(n)
  )
  # every observation in arm c loses a covariate
  dat$X_a[dat$Z == "c"] <- NA

  expect_warning(res <- check_balance(dat, Z, quiet = TRUE),
                 "lost every observation to missing covariate values")
  expect_true(is.na(res$joint_test$p_value))
  expect_false(res$joint_test$estimable)
})

# --- Diagnostics live in the data, not only in warnings ------------------------

test_that("the joint test carries status and obs_per_param on every path", {
  set.seed(121)
  n <- 600
  dat <- data.frame(Z = rep(0:1, n / 2), X_a = rnorm(n), X_b = rnorm(n))

  binary <- check_balance(dat, Z, quiet = TRUE)$joint_test
  expect_true(all(c("status", "obs_per_param", "p_value_classical") %in% names(binary)))
  expect_equal(binary$status, "tested")
  expect_equal(binary$estimable, binary$status == "tested")
  # Wald path counts variance parameters
  expect_equal(binary$obs_per_param, n / (binary$df1 * (binary$df1 + 1) / 2))

  dat$Zm <- rep(c("a", "b", "c"), length.out = n)
  multi <- check_balance(dat, Zm, covariates = c("X_a", "X_b"), quiet = TRUE)$joint_test
  expect_equal(multi$status, "tested")
  # likelihood-ratio path counts coefficients, since it inverts nothing
  expect_equal(multi$obs_per_param, as.numeric(multi$nobs) / multi$df1)
})

test_that("status names the reason a joint test is absent", {
  set.seed(122)
  n <- 300
  dat <- data.frame(Z = factor(rep(c("a", "b", "c"), length.out = n)),
                    X_a = rnorm(n), X_b = rnorm(n))
  dat$X_a[dat$Z == "c"] <- NA          # arm c loses every observation

  res <- suppressWarnings(check_balance(dat, Z, quiet = TRUE))
  expect_equal(res$joint_test$status, "arm_lost_to_missingness")
  expect_false(res$joint_test$estimable)
  expect_true(is.na(res$joint_test$p_value))
})

test_that("the thin-battery diagnostic survives into saved output", {
  # The failure this guards: a warning raised inside map() inside mutate() is
  # collapsed by dplyr to a bare count, and write_rds keeps none of it.
  set.seed(123)
  n <- 60
  dat <- data.frame(Z = rep(0:1, n / 2))
  for (j in 1:8) dat[[paste0("X_", j)]] <- rnorm(n)

  res <- suppressWarnings(check_balance(dat, Z, quiet = TRUE))
  f <- tempfile(fileext = ".rds")
  saveRDS(res$joint_test, f)
  reloaded <- readRDS(f)

  expect_lt(reloaded$obs_per_param, 10)        # readable after a round trip
  expect_false(is.na(reloaded$p_value_classical))
  expect_equal(reloaded$status, "tested")
})

test_that("obs_per_param is NA on the randomization-inference path", {
  skip_if_not_installed("ri2")
  skip_if_not_installed("randomizr")
  set.seed(124)
  n <- 80
  dat <- data.frame(Z = randomizr::complete_ra(n, conditions = c("a", "b")),
                    X_a = rnorm(n), X_b = rnorm(n))
  decl <- randomizr::declare_ra(N = n, conditions = c("a", "b"))
  j <- check_balance(dat, Z, declaration = decl, sims = 100, quiet = TRUE)$joint_test
  expect_true(is.na(j$obs_per_param))
  expect_equal(j$status, "tested")
})
