# tests/testthat/test-write_check_code.R

make_attrition_code_dat <- function() {
  set.seed(1)
  dat <- data.frame(
    Z = rep(c(0L, 1L), 50),
    Y_outcome = rnorm(100)
  )
  dat$Y_outcome[1:10] <- NA
  dat
}

make_balance_code_dat <- function() {
  set.seed(2)
  data.frame(
    Z = rep(c(0L, 1L), 50),
    X_age = rnorm(100, 50, 10),
    X_income = rnorm(100, 50000, 10000)
  )
}

# ── write_attrition_check_code ────────────────────────────────────────────────

test_that("returns a single string invisibly", {
  dat <- make_attrition_code_dat()
  code <- write_attrition_check_code(dat, Z)
  expect_type(code, "character")
  expect_length(code, 1)
})

test_that("includes outcome variable name", {
  dat <- make_attrition_code_dat()
  code <- write_attrition_check_code(dat, Z)
  expect_match(code, "Y_outcome")
})

test_that("includes treatment variable name", {
  dat <- make_attrition_code_dat()
  code <- write_attrition_check_code(dat, Z)
  expect_match(code, "\\bZ\\b")
})

test_that("includes dataset name", {
  mydat <- make_attrition_code_dat()
  code <- write_attrition_check_code(mydat, Z)
  expect_match(code, "mydat")
})

test_that("generates as.integer assignment when _missing column absent", {
  dat <- make_attrition_code_dat()
  code <- write_attrition_check_code(dat, Z)
  expect_match(code, "as\\.integer")
})

test_that("uses existing _missing column and skips as.integer when present", {
  dat <- make_attrition_code_dat()
  dat$Y_outcome_missing <- as.integer(is.na(dat$Y_outcome))
  code <- write_attrition_check_code(dat, Z)
  expect_false(grepl("as\\.integer", code))
  expect_match(code, "Y_outcome_missing")
})

test_that("auto-selects Y_ columns", {
  dat <- make_attrition_code_dat()
  set.seed(3)
  dat$Y_second <- rnorm(100)
  dat$Y_second[11:20] <- NA
  code <- write_attrition_check_code(dat, Z)
  expect_match(code, "Y_outcome")
  expect_match(code, "Y_second")
})

test_that("explicit outcomes restrict to named variables", {
  dat <- make_attrition_code_dat()
  dat$Y_other <- rnorm(100)
  code <- write_attrition_check_code(dat, Z, outcomes = "Y_outcome")
  expect_match(code, "Y_outcome")
  expect_false(grepl("Y_other", code))
})

test_that("warns and returns NULL when no outcomes found", {
  dat <- data.frame(Z = 0:1, X_age = c(25, 30))
  expect_warning(res <- write_attrition_check_code(dat, Z), "No outcomes selected")
  expect_null(res)
})

test_that("passes extra arguments through to generated code", {
  dat <- make_attrition_code_dat()
  code <- write_attrition_check_code(dat, Z, clusters = cluster_id)
  expect_match(code, "clusters")
})

test_that("produces console output", {
  dat <- make_attrition_code_dat()
  expect_output(write_attrition_check_code(dat, Z))
})

test_that("tidyselect expression for outcomes works", {
  dat <- make_attrition_code_dat()
  code <- write_attrition_check_code(dat, Z, outcomes = dplyr::starts_with("Y_"))
  expect_match(code, "Y_outcome")
})

# ── write_balance_check_code ──────────────────────────────────────────────────

test_that("returns a single string invisibly", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z)
  expect_type(code, "character")
  expect_length(code, 1)
})

test_that("includes covariate names", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z)
  expect_match(code, "X_age")
  expect_match(code, "X_income")
})

test_that("includes treatment variable name", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z)
  expect_match(code, "\\bZ\\b")
})

test_that("includes dataset name", {
  mydat <- make_balance_code_dat()
  code <- write_balance_check_code(mydat, Z)
  expect_match(code, "mydat")
})

test_that("includes joint test code for binary treatment", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z)
  expect_match(code, "\\.Z_numeric|joint")
})

test_that("explicit covariates restrict to named variables", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z, covariates = "X_age")
  expect_match(code, "X_age")
  expect_false(grepl("X_income", code))
})

test_that("warns and returns NULL when no covariates found", {
  dat <- data.frame(Z = 0:1, Y = c(1.0, 2.0))
  expect_warning(res <- write_balance_check_code(dat, Z), "No covariates selected")
  expect_null(res)
})

test_that("handles factor covariate by generating dummy variable code", {
  dat <- data.frame(
    Z = rep(c(0L, 1L), 50),
    X_party = factor(rep(c("D", "R", "I"), length.out = 100))
  )
  code <- write_balance_check_code(dat, Z, covariates = "X_party")
  expect_match(code, "X_party")
})

test_that("character covariate handled same as factor", {
  dat <- data.frame(
    Z = rep(c(0L, 1L), 50),
    X_region = rep(c("North", "South", "East"), length.out = 100)
  )
  code <- write_balance_check_code(dat, Z, covariates = "X_region")
  expect_match(code, "X_region")
})

test_that("multi-arm treatment emits check_balance call for joint test", {
  dat <- data.frame(
    Z = rep(c("C", "T1", "T2"), length.out = 90),
    X_age = rnorm(90, 50, 10)
  )
  code <- write_balance_check_code(dat, Z)
  expect_match(code, "check_balance")
})

test_that("passes extra arguments through to generated code", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z, clusters = cluster_id)
  expect_match(code, "clusters")
})

test_that("produces console output", {
  dat <- make_balance_code_dat()
  expect_output(write_balance_check_code(dat, Z))
})

test_that("tidyselect expression for covariates works", {
  dat <- make_balance_code_dat()
  code <- write_balance_check_code(dat, Z, covariates = dplyr::starts_with("X_"))
  expect_match(code, "X_age")
  expect_match(code, "X_income")
})
