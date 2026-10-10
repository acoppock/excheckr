# tests/testthat/test-joint_tests.R
#
# The two multi-arm joint tests, called directly. check_balance() exercises both,
# but only through whatever data reaches them, so the statistic itself and every
# not-estimable branch are pinned here.

make_multiarm_dat <- function(seed = 11, n = 180) {
  set.seed(seed)
  data.frame(
    Z = factor(rep(c("C", "T1", "T2"), length.out = n)),
    X_age = rnorm(n, 50, 10),
    X_income = rnorm(n, 50, 5)
  )
}

nnet_lr <- function(dat, covariates) {
  full <- nnet::multinom(
    stats::reformulate(covariates, response = "Z"),
    data = dat, trace = FALSE
  )
  null <- nnet::multinom(Z ~ 1, data = dat, trace = FALSE)
  as.numeric(2 * (stats::logLik(full) - stats::logLik(null)))
}

# --- multinomial_lr_joint_test ------------------------------------------------

test_that("the multinomial LR statistic matches fitting both models with nnet", {
  dat <- make_multiarm_dat()
  res <- multinomial_lr_joint_test(dat, "Z", c("X_age", "X_income"))
  lr <- nnet_lr(dat, c("X_age", "X_income"))

  # (K - 1) * p = 2 * 2 coefficients are restricted under the null
  expect_equal(res$df1, 4L)
  expect_equal(res$F_stat * res$df1, lr, tolerance = 1e-6)
  expect_equal(
    res$p_value,
    stats::pchisq(res$F_stat * res$df1, df = 4, lower.tail = FALSE)
  )
  expect_equal(res$statistic, "LR/df")
  expect_equal(res$nobs, 180L)
  expect_equal(res$obs_per_param, 180 / 4)
  expect_equal(res$status, "tested")
  expect_true(res$estimable)
  expect_true(is.na(res$df2))
  expect_true(is.na(res$p_value_classical))
})

test_that("a declared level with no observations is charged no degrees of freedom", {
  dat <- make_multiarm_dat()
  dat$Z <- factor(dat$Z, levels = c("C", "T1", "T2", "T3"))

  res <- multinomial_lr_joint_test(dat, "Z", c("X_age", "X_income"))

  expect_equal(res$df1, 4L)
  expect_equal(res$status, "tested")
})

test_that("rows with a missing covariate are dropped before fitting", {
  dat <- make_multiarm_dat()
  dat$X_age[1:30] <- NA

  res <- multinomial_lr_joint_test(dat, "Z", c("X_age", "X_income"))

  expect_equal(res$nobs, 150L)
  expect_equal(
    res$F_stat * res$df1,
    nnet_lr(dat[-(1:30), ], c("X_age", "X_income")),
    tolerance = 1e-6
  )
})

test_that("an arm lost to missing covariates is named and not tested", {
  dat <- make_multiarm_dat()
  dat$X_age[dat$Z == "T2"] <- NA

  expect_warning(
    res <- multinomial_lr_joint_test(dat, "Z", c("X_age", "X_income")),
    "treatment group\\(s\\) T2 lost every observation"
  )
  expect_equal(res$status, "arm_lost_to_missingness")
  expect_false(res$estimable)
  expect_true(is.na(res$p_value))
})

test_that("a single arm is not tested", {
  dat <- make_multiarm_dat()
  dat <- dat[dat$Z == "C", ]

  expect_warning(
    res <- multinomial_lr_joint_test(dat, "Z", "X_age"),
    "only 1 treatment arm"
  )
  expect_equal(res$status, "single_arm")
  expect_false(res$estimable)
})

test_that("a constant covariate is not tested", {
  dat <- make_multiarm_dat()
  dat$X_const <- 1

  expect_warning(
    res <- multinomial_lr_joint_test(dat, "Z", c("X_age", "X_const")),
    "constant covariate"
  )
  expect_equal(res$status, "constant_covariate")
  expect_false(res$estimable)
  expect_true(is.na(res$df1))
})

# --- ri_joint_test --------------------------------------------------------------

test_that("the RI statistic's closed-form null matches the fitted null model", {
  skip_if_not_installed("randomizr")
  skip_if_not_installed("ri2")

  dat <- make_multiarm_dat(n = 90)
  declaration <- randomizr::declare_ra(
    N = 90, m_each = c(30, 30, 30), conditions = c("C", "T1", "T2")
  )

  set.seed(12)
  res <- ri_joint_test(dat, "Z", c("X_age", "X_income"), declaration, sims = 20)

  # The null log-likelihood is sum_k n_k log(n_k / n) rather than a multinom fit,
  # and the observed statistic must not move because of it.
  expect_equal(res$F_stat, nnet_lr(dat, c("X_age", "X_income")), tolerance = 1e-6)
  expect_equal(res$statistic, "LR")
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_equal(res$nobs, 90L)
  expect_true(is.na(res$df1))
  expect_true(is.na(res$obs_per_param))
})

test_that("the closed-form null holds with unequal arms", {
  skip_if_not_installed("randomizr")
  skip_if_not_installed("ri2")

  set.seed(13)
  n <- 100
  dat <- data.frame(
    Z = factor(rep(c("C", "T"), c(95, 5))),
    X_age = rnorm(n)
  )
  declaration <- randomizr::declare_ra(
    N = n, m_each = c(95, 5), conditions = c("C", "T")
  )

  res <- ri_joint_test(dat, "Z", "X_age", declaration, sims = 20)

  expect_equal(res$F_stat, nnet_lr(dat, "X_age"), tolerance = 1e-6)
})

test_that("the RI and asymptotic paths compute the same LR statistic", {
  skip_if_not_installed("randomizr")
  skip_if_not_installed("ri2")

  dat <- make_multiarm_dat(n = 90)
  declaration <- randomizr::declare_ra(
    N = 90, m_each = c(30, 30, 30), conditions = c("C", "T1", "T2")
  )

  set.seed(14)
  ri <- ri_joint_test(dat, "Z", c("X_age", "X_income"), declaration, sims = 20)
  lr <- multinomial_lr_joint_test(dat, "Z", c("X_age", "X_income"))

  expect_equal(ri$F_stat, lr$F_stat * lr$df1, tolerance = 1e-6)
})
