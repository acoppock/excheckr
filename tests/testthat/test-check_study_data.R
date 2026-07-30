# tests/testthat/test-check_study_data.R

# ── check_y_bounds ──────────────────────────────────────────────────────────

make_y_dat <- function() {
  data.frame(
    Y_support   = c(0, 0.5, 1),
    Y_oppose    = c(0, 1.2, 0.8),   # out of bounds
    Y_support_s = c(-1, 0, 1),      # standardised: excluded by default
    Y_support_missing = c(0, 1, 0), # missing indicator: excluded by default
    X_age       = c(25, 30, 35)     # non-Y column
  )
}

# --- Return structure ---

test_that("check_y_bounds: returns a tibble with expected columns", {
  res <- check_y_bounds(make_y_dat())
  expect_s3_class(res, "data.frame")
  expect_true(all(c("variable", "min", "max", "in_bounds") %in% names(res)))
  expect_false("study_id" %in% names(res))
})

test_that("check_y_bounds: study_id appended when provided", {
  res <- check_y_bounds(make_y_dat(), study_id = "my_study")
  expect_true("study_id" %in% names(res))
  expect_true(all(res$study_id == "my_study"))
})

# --- Default auto-selection ---

test_that("check_y_bounds: default selects Y_ columns only", {
  res <- check_y_bounds(make_y_dat())
  expect_setequal(res$variable, c("Y_support", "Y_oppose"))
})

test_that("check_y_bounds: default excludes _s suffix", {
  expect_false("Y_support_s" %in% check_y_bounds(make_y_dat())$variable)
})

test_that("check_y_bounds: default excludes _missing suffix", {
  expect_false("Y_support_missing" %in% check_y_bounds(make_y_dat())$variable)
})

# --- in_bounds flag ---

test_that("check_y_bounds: in_bounds is TRUE for [0,1] and FALSE outside", {
  res <- check_y_bounds(make_y_dat())
  expect_true(res$in_bounds[res$variable == "Y_support"])
  expect_false(res$in_bounds[res$variable == "Y_oppose"])
})

# --- prefix ---

test_that("check_y_bounds: prefix overrides auto-selection", {
  dat <- data.frame(out_a = c(0, 0.5, 1), out_b = c(0, 0, 1), Y_x = c(0, 1, 2))
  res <- check_y_bounds(dat, prefix = "out_")
  expect_setequal(res$variable, c("out_a", "out_b"))
})

# --- outcomes: character vector ---

test_that("check_y_bounds: outcomes as character vector", {
  res <- check_y_bounds(make_y_dat(), outcomes = "Y_support")
  expect_equal(res$variable, "Y_support")
  expect_equal(nrow(res), 1)
})

# --- outcomes: tidyselect ---

test_that("check_y_bounds: outcomes as tidyselect expression", {
  res <- check_y_bounds(make_y_dat(), outcomes = dplyr::starts_with("Y_oppose"))
  expect_equal(res$variable, "Y_oppose")
})

# --- exclude: character vector ---

test_that("check_y_bounds: exclude as character vector drops named columns", {
  res <- check_y_bounds(make_y_dat(), exclude = "Y_oppose")
  expect_false("Y_oppose" %in% res$variable)
  expect_true("Y_support" %in% res$variable)
})

# --- exclude: tidyselect ---

test_that("check_y_bounds: exclude as tidyselect expression", {
  res <- check_y_bounds(make_y_dat(), exclude = dplyr::ends_with("_oppose"))
  expect_false("Y_oppose" %in% res$variable)
  expect_true("Y_support" %in% res$variable)
})

# --- suppressWarnings(as.numeric) ---

test_that("check_y_bounds: handles factor columns without error", {
  dat <- data.frame(Y_x = factor(c("0", "0.5", "1")))
  expect_no_warning(res <- check_y_bounds(dat))
  expect_equal(res$min[[1]], 0)
  expect_equal(res$max[[1]], 1)
})

# --- NULL / warning ---

test_that("check_y_bounds: warns and returns NULL when nothing selected", {
  dat <- data.frame(X_age = 1:3)
  expect_warning(res <- check_y_bounds(dat), "No outcome variables found")
  expect_null(res)
})


# ── check_missingness_nona ───────────────────────────────────────────────────

make_x_dat <- function() {
  data.frame(
    X_age         = c(25, NA, 30),
    X_age_nona    = c(25, 27, 30),
    X_income      = c(NA, 50, 60),
    X_race_missing = c(0, 1, 0),    # _missing indicator: excluded by default
    Y_outcome     = c(0.1, 0.5, 0.9)
  )
}

# --- Return structure ---

test_that("check_missingness_nona: returns a tibble with expected columns", {
  res <- check_missingness_nona(make_x_dat())
  expect_s3_class(res, "data.frame")
  expect_true(all(c("variable", "n_missing", "pct_missing",
                     "has_nona_version", "nona_has_na") %in% names(res)))
  expect_false("study_id" %in% names(res))
})

test_that("check_missingness_nona: study_id appended when provided", {
  res <- check_missingness_nona(make_x_dat(), study_id = "s1")
  expect_true("study_id" %in% names(res))
  expect_true(all(res$study_id == "s1"))
})

# --- Default auto-selection ---

test_that("check_missingness_nona: default selects X_ base columns only", {
  res <- check_missingness_nona(make_x_dat())
  expect_setequal(res$variable, c("X_age", "X_income"))
})

test_that("check_missingness_nona: default excludes _nona columns", {
  expect_false("X_age_nona" %in% check_missingness_nona(make_x_dat())$variable)
})

test_that("check_missingness_nona: default excludes _missing columns", {
  expect_false("X_race_missing" %in% check_missingness_nona(make_x_dat())$variable)
})

# --- Missingness counts ---

test_that("check_missingness_nona: n_missing and pct_missing are correct", {
  res <- check_missingness_nona(make_x_dat())
  age_row <- res[res$variable == "X_age", ]
  expect_equal(age_row$n_missing[[1]], 1L)
  expect_equal(age_row$pct_missing[[1]], 1/3, tolerance = 1e-8)

  income_row <- res[res$variable == "X_income", ]
  expect_equal(income_row$n_missing[[1]], 1L)
})

# --- has_nona_version and nona_has_na ---

test_that("check_missingness_nona: has_nona_version TRUE when companion exists", {
  res <- check_missingness_nona(make_x_dat())
  expect_true(res$has_nona_version[res$variable == "X_age"])
  expect_false(res$has_nona_version[res$variable == "X_income"])
})

test_that("check_missingness_nona: nona_has_na FALSE when companion is complete", {
  res <- check_missingness_nona(make_x_dat())
  expect_false(res$nona_has_na[res$variable == "X_age"])
})

test_that("check_missingness_nona: nona_has_na NA when no companion exists", {
  res <- check_missingness_nona(make_x_dat())
  expect_true(is.na(res$nona_has_na[res$variable == "X_income"]))
})

test_that("check_missingness_nona: nona_has_na TRUE when companion has NAs", {
  dat <- data.frame(X_a = c(1, NA, 3), X_a_nona = c(1, NA, 3))
  res <- check_missingness_nona(dat)
  expect_true(res$nona_has_na[res$variable == "X_a"])
})

# --- prefix ---

test_that("check_missingness_nona: prefix overrides auto-selection", {
  dat <- data.frame(cov_a = c(1, NA), cov_b = c(NA, 2), X_x = c(1, 2))
  res <- check_missingness_nona(dat, prefix = "cov_")
  expect_setequal(res$variable, c("cov_a", "cov_b"))
})

# --- nona_suffix ---

test_that("check_missingness_nona: nona_suffix controls companion lookup", {
  dat <- data.frame(
    X_age          = c(25, NA, 30),
    X_age_imputed  = c(25, 27, 30)
  )
  res <- check_missingness_nona(dat, nona_suffix = "_imputed")
  expect_true(res$has_nona_version[res$variable == "X_age"])
  expect_false(res$nona_has_na[res$variable == "X_age"])
})

test_that("check_missingness_nona: default _nona suffix ignored when overridden", {
  dat <- data.frame(
    X_age      = c(25, NA, 30),
    X_age_nona = c(25, 27, 30)   # this is now just a regular column
  )
  # with nona_suffix = "_imputed", X_age_nona is NOT the companion
  res <- check_missingness_nona(dat, nona_suffix = "_imputed")
  expect_false(res$has_nona_version[res$variable == "X_age"])
})

# --- covariates: character vector ---

test_that("check_missingness_nona: covariates as character vector", {
  res <- check_missingness_nona(make_x_dat(), covariates = "X_age")
  expect_equal(res$variable, "X_age")
  expect_equal(nrow(res), 1)
})

# --- covariates: tidyselect ---

test_that("check_missingness_nona: covariates as tidyselect expression", {
  res <- check_missingness_nona(make_x_dat(), covariates = dplyr::starts_with("X_income"))
  expect_equal(res$variable, "X_income")
})

# --- exclude: character vector ---

test_that("check_missingness_nona: exclude as character vector", {
  res <- check_missingness_nona(make_x_dat(), exclude = "X_income")
  expect_false("X_income" %in% res$variable)
  expect_true("X_age" %in% res$variable)
})

# --- exclude: tidyselect ---

test_that("check_missingness_nona: exclude as tidyselect expression", {
  res <- check_missingness_nona(make_x_dat(), exclude = dplyr::ends_with("_income"))
  expect_false("X_income" %in% res$variable)
  expect_true("X_age" %in% res$variable)
})

# --- NULL / warning ---

test_that("check_missingness_nona: warns and returns NULL when nothing selected", {
  dat <- data.frame(Y_outcome = 1:3)
  expect_warning(res <- check_missingness_nona(dat), "No base covariate variables found")
  expect_null(res)
})

# --- All-NA outcomes ----------------------------------------------------------

test_that("an all-NA outcome yields NA rather than a false out-of-bounds flag", {
  res <- check_y_bounds(data.frame(Y_x = c(NA_real_, NA_real_)))

  expect_true(is.na(res$min))
  expect_true(is.na(res$max))
  expect_true(is.na(res$in_bounds))
  expect_false(isTRUE(is.infinite(res$min)))
})

test_that("an all-NA outcome is not reported as out of bounds by report_checks", {
  res <- check_y_bounds(data.frame(Y_x = c(NA_real_, NA_real_)), study_id = "a")
  report <- report_checks(list(ybounds = res))
  expect_equal(nrow(report$out_of_bounds), 0)
})
