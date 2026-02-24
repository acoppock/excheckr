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
  expect_true(all(c("outcome", "term", "estimate", "std.error", "p.value") %in% names(res)))
})

# --- Binary (numeric 0/1) treatment --------------------------------------------

test_that("binary numeric treatment: one row per outcome, term equals treatment name", {
  set.seed(1)
  n <- 100
  dat <- data.frame(
    Z = rep(c(0L, 1L), n / 2),
    Y = c(rnorm(45), rep(NA, 5), rnorm(45), rep(NA, 5))
  )

  res <- check_attrition(dat, Z, outcomes = "Y")

  expect_equal(nrow(res), 1)
  expect_equal(res$term, "Z")
  expect_equal(res$outcome, "Y")
})

# --- Multi-level character treatment (regression-fix) -------------------------

test_that("character treatment with K levels returns K-1 rows, all terms prefixed by treatment name", {
  dat <- make_attrition_dat()
  res <- check_attrition(dat, Z_party, outcomes = "Y")

  # 3-level Z_party → K-1 = 2 treatment rows
  expect_equal(nrow(res), 2)
  expect_true(all(startsWith(res$term, "Z_party")))
  expect_equal(unique(res$outcome), "Y")
})

test_that("no (Intercept) row appears in result", {
  dat <- make_attrition_dat()
  res <- check_attrition(dat, Z_party, outcomes = "Y")

  expect_false("(Intercept)" %in% res$term)
})

# --- Outcome selection ---------------------------------------------------------

test_that("auto-selects Y_ columns when outcomes is NULL", {
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

# --- Console output ------------------------------------------------------------

test_that("produces no console output", {
  dat <- make_attrition_dat()
  expect_silent(check_attrition(dat, Z_party, outcomes = "Y"))
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
