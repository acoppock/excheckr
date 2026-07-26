# tests/testthat/test-check_schema.R

good_dat <- function() {
  data.frame(
    resp_id = 1:4,
    topic   = c("a", "a", "b", "b"),
    weights = 1,
    X_age   = c(20, 30, 40, 50),
    Z_party = c(0L, 1L, 0L, 1L),
    Y       = c(0, 1, 1, 0)
  )
}

# --- return structure ---

test_that("returns a report tibble with one row per check", {
  res <- check_schema(good_dat(), key = c("resp_id", "topic"))
  expect_s3_class(res, "data.frame")
  expect_true(all(c("check", "severity", "pass", "detail") %in% names(res)))
  expect_true(all(c("key_unique", "no_labelled", "has_treatment", "has_outcome") %in% res$check))
})

test_that("a conforming frame passes every check", {
  res <- check_schema(good_dat(), key = c("resp_id", "topic"))
  expect_true(all(res$pass))
})

test_that("study_id appended when provided", {
  res <- check_schema(good_dat(), key = c("resp_id", "topic"), study_id = "s1")
  expect_true("study_id" %in% names(res))
  expect_equal(unique(res$study_id), "s1")
})

# --- error-severity checks ---

test_that("duplicate key fails key_unique", {
  d <- good_dat(); d$topic <- "a"  # (resp_id, topic) still unique via resp_id
  d <- rbind(d, d[1, ])            # now a true duplicate row
  res <- check_schema(d, key = c("resp_id", "topic"))
  expect_false(res$pass[res$check == "key_unique"])
})

test_that("missing key column fails key_present and does not error", {
  d <- good_dat(); d$topic <- NULL
  res <- check_schema(d, key = c("resp_id", "topic"))
  expect_false(res$pass[res$check == "key_present"])
})

test_that("leaked haven_labelled fails no_labelled", {
  d <- good_dat()
  d$X_age <- structure(d$X_age, class = "haven_labelled")
  res <- check_schema(d, key = c("resp_id", "topic"))
  expect_false(res$pass[res$check == "no_labelled"])
  expect_match(res$detail[res$check == "no_labelled"], "X_age")
})

test_that("missing weights fails when required and passes when not", {
  d <- good_dat(); d$weights <- NULL
  expect_false(check_schema(d, key = c("resp_id", "topic"))$pass[
    check_schema(d, key = c("resp_id", "topic"))$check == "weights_present"])
  res_norw <- check_schema(d, key = c("resp_id", "topic"), require_weights = FALSE)
  expect_false(any(res_norw$check == "weights_present"))
})

test_that("missing treatment or outcome fails", {
  d <- good_dat(); d$Z_party <- NULL
  res <- check_schema(d, key = c("resp_id", "topic"))
  expect_false(res$pass[res$check == "has_treatment"])
})

# --- warn-severity checks ---

test_that("undeclared column warns, declared metadata does not", {
  d <- good_dat(); d$year_of_birth <- 1980
  res <- check_schema(d, key = c("resp_id", "topic"))
  expect_false(res$pass[res$check == "no_extra_columns"])
  expect_match(res$detail[res$check == "no_extra_columns"], "year_of_birth")
  res2 <- check_schema(d, key = c("resp_id", "topic"), meta = "year_of_birth")
  expect_true(res2$pass[res2$check == "no_extra_columns"])
})

test_that("constant treatment and all-NA column warn", {
  d <- good_dat(); d$Z_party <- 1L; d$X_age <- NA_real_
  res <- check_schema(d, key = c("resp_id", "topic"))
  expect_false(res$pass[res$check == "treatment_varies"])
  expect_false(res$pass[res$check == "no_all_na_columns"])
})

# --- assert_schema ---

test_that("assert_schema is silent on a clean frame and stops on error", {
  expect_invisible(assert_schema(good_dat(), key = c("resp_id", "topic")))
  d <- good_dat(); d$X_age <- structure(d$X_age, class = "haven_labelled")
  expect_error(assert_schema(d, key = c("resp_id", "topic")), "no_labelled")
})

test_that("assert_schema warns but does not stop on warn-only issues", {
  d <- good_dat(); d$leaked <- 1
  expect_warning(assert_schema(d, key = c("resp_id", "topic")), "no_extra_columns")
})

# --- primitives ---

test_that("assert_key_unique errors on dup and missing key", {
  expect_invisible(assert_key_unique(good_dat(), key = c("resp_id", "topic")))
  expect_error(assert_key_unique(good_dat(), key = "nope"), "missing")
  d <- rbind(good_dat(), good_dat()[1, ])
  expect_error(assert_key_unique(d, key = c("resp_id", "topic")), "not unique")
})

test_that("assert_no_labelled errors on a labelled column", {
  expect_invisible(assert_no_labelled(good_dat()))
  d <- good_dat(); d$X_age <- structure(d$X_age, class = "haven_labelled")
  expect_error(assert_no_labelled(d), "X_age")
})
