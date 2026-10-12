# tests/testthat/test-helper_functions.R
#
# The guards in helper_functions.r that the checks built on them never reach:
# the early returns for data too thin to compute on, and the shapes run_by_strata
# must handle when a check returns nothing for some strata.

# --- weighted_mean / weighted_sd ----------------------------------------------

test_that("weighted_mean and weighted_sd reduce to mean and sd with unit weights", {
  x <- c(2, 4, 9, 1, NA)
  w <- rep(1, 5)
  rows <- rep(TRUE, 5)

  expect_equal(weighted_mean(x, w, rows), mean(x, na.rm = TRUE))
  expect_equal(weighted_sd(x, w, rows), sd(x, na.rm = TRUE))
})

test_that("weighted_mean is NA when no row is usable", {
  expect_true(is.na(weighted_mean(c(NA, 1), c(1, NA), c(TRUE, TRUE))))
  expect_true(is.na(weighted_mean(1:3, rep(1, 3), rep(FALSE, 3))))
})

test_that("weighted_sd is NA with fewer than two usable rows", {
  expect_true(is.na(weighted_sd(c(1, NA, 3), rep(1, 3), c(TRUE, TRUE, FALSE))))
})

test_that("weighted_sd is NA when the weights leave no denominator", {
  # sum(w) - 1 is the denominator, so weights summing to 1 or less leave none.
  expect_true(is.na(weighted_sd(c(1, 5), c(0.4, 0.4), c(TRUE, TRUE))))
})

# --- warn_aliased_covariates -----------------------------------------------------

test_that("fewer than two covariates cannot be aliased", {
  dat <- data.frame(X_a = rnorm(10))
  expect_no_warning(res <- warn_aliased_covariates(dat, "X_a"))
  expect_false(res)
})

test_that("a covariate set that cannot be built returns quietly", {
  dat <- data.frame(X_a = rnorm(10))
  expect_no_warning(res <- warn_aliased_covariates(dat, c("X_a", "X_missing")))
  expect_false(res)
})

test_that("a full-rank covariate set returns FALSE without warning", {
  set.seed(201)
  dat <- data.frame(X_a = rnorm(50), X_b = rnorm(50))
  expect_no_warning(res <- warn_aliased_covariates(dat, c("X_a", "X_b")))
  expect_false(res)
})

test_that("covariates aliased only with the intercept name no partner", {
  # Both are constant, so the intercept is the only retained column and there is
  # nothing to regress the dropped ones on.
  dat <- data.frame(X_a = rep(1, 20), X_b = rep(2, 20))
  expect_warning(
    res <- warn_aliased_covariates(dat, c("X_a", "X_b")),
    "X_a is a linear combination of the others"
  )
  expect_true(res)
})

test_that("a near-duplicate is flagged without claiming an exact partner", {
  # Close enough for qr() to call the set rank deficient, not close enough for
  # the residual check to call the dependence exact.
  set.seed(202)
  dat <- data.frame(X_a = rnorm(200))
  dat$X_b <- dat$X_a + rnorm(200, sd = 5e-8)

  ws <- character(0)
  withCallingHandlers(
    warn_aliased_covariates(dat, c("X_a", "X_b"), fn_name = "check_attrition"),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_length(ws, 1)
  expect_match(ws, "^check_attrition: the covariate set is rank deficient")
  expect_match(ws, "X_b is a linear combination of the others")
  expect_no_match(ws, "exactly determined")
  expect_match(ws, 'covariates = c\\("X_a"\\)')
})

# --- run_by_strata / prepend_stratum_keys -----------------------------------------

strata_dat <- data.frame(g = c("a", "a", "b", "b", "c"), x = 1:5)

test_that("strata a check returns nothing for are dropped", {
  res <- run_by_strata(strata_dat, rlang::quo(g), function(d) {
    if (sum(d$x) >= 5) tibble::tibble(total = sum(d$x)) else NULL
  })
  expect_equal(res$g, c("b", "c"))
  expect_equal(res$total, c(7L, 5L))
})

test_that("a check that returns nothing for every stratum returns NULL", {
  expect_null(run_by_strata(strata_dat, rlang::quo(g), function(d) NULL))
})

test_that("a list element empty in every stratum binds to an empty tibble", {
  res <- run_by_strata(strata_dat, rlang::quo(g), function(d) {
    list(totals = tibble::tibble(total = sum(d$x)), extras = NULL)
  })
  expect_named(res, c("totals", "extras"))
  expect_equal(nrow(res$totals), 3)
  expect_equal(res$extras, tibble::tibble())
})

test_that("a zero-row result gets no stratum columns prepended", {
  empty <- tibble::tibble(total = integer(0))
  res <- prepend_stratum_keys(empty, data.frame(g = "a"))
  expect_identical(res, empty)

  res <- prepend_stratum_keys(
    list(totals = tibble::tibble(total = 3L), extras = empty),
    data.frame(g = "a")
  )
  expect_equal(res$totals, tibble::tibble(g = "a", total = 3L))
  expect_identical(res$extras, empty)
})

test_that("selecting no grouping columns is an error", {
  expect_error(
    run_by_strata(strata_dat, rlang::quo(c()), identity),
    "no grouping columns selected"
  )
})

# --- classical_f_pvalue / model_df1 ------------------------------------------------

test_that("classical_f_pvalue is NA for a fit with no fitted values", {
  expect_true(is.na(classical_f_pvalue(list(), 1:5)))
})

test_that("classical_f_pvalue is NA when the fit's rows cannot be aligned", {
  fit <- list(fitted.values = c(a = 0.2, b = 0.4, c = 0.6))
  expect_true(is.na(classical_f_pvalue(fit, 1:5)))
})

test_that("classical_f_pvalue is NA with fewer than three usable rows", {
  fit <- list(fitted.values = c(0.2, 0.4, NA), coefficients = c(1, 2))
  expect_true(is.na(classical_f_pvalue(fit, c(0, 1, 1))))
})

test_that("classical_f_pvalue is NA for an intercept-only fit", {
  y <- c(0, 1, 1, 0, 1)
  expect_true(is.na(classical_f_pvalue(lm(y ~ 1), y)))
})

test_that("classical_f_pvalue is NA for a perfect fit", {
  # lm() leaves residuals of order 1e-15 even on an exact line, so the fit is
  # stated directly to put the residual sum of squares at exactly zero.
  y <- c(1, 3, 5, 7, 9)
  fit <- list(fitted.values = y, coefficients = c(1, 2))
  expect_true(is.na(classical_f_pvalue(fit, y)))
})

test_that("model_df1 is NA when the coefficients cannot be read", {
  expect_true(is.na(model_df1("not a fit")))
})

# --- multinomial_lr_joint_test ---------------------------------------------------

test_that("a multinomial fit that errors is reported as not estimable", {
  # 500 covariates across three arms need 1002 network weights, past nnet's
  # default MaxNWts of 1000, so multinom() stops rather than fitting.
  set.seed(203)
  dat <- data.frame(Z = factor(rep(c("A", "B", "C"), length.out = 60)))
  covariates <- paste0("X_", 1:500)
  for (cname in covariates) dat[[cname]] <- rnorm(60)

  expect_warning(
    res <- multinomial_lr_joint_test(dat, "Z", covariates),
    "multinomial model failed to converge"
  )
  expect_equal(res$status, "no_convergence")
  expect_false(res$estimable)
})

test_that("classical_f_pvalue is NA when the fit does worse than the mean", {
  # Only an OLS fit guarantees rss <= tss. A weighted fit, whose weights this
  # ignores, need not, and a negative F has no p-value.
  y <- 1:5
  fit <- list(fitted.values = c(5, 4, 3, 2, 1), coefficients = c(1, 2))
  expect_true(is.na(classical_f_pvalue(fit, y)))
})

test_that("ri_joint_test says how to install ri2 when it is missing", {
  local_mocked_bindings(requireNamespace = function(...) FALSE)
  dat <- data.frame(Z = factor(rep(c("C", "T"), 10)), X_age = rnorm(20))
  expect_error(
    ri_joint_test(dat, "Z", "X_age", declaration = NULL),
    "install.packages\\('ri2'\\)"
  )
})
