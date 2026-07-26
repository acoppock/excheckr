example_checks <- function() {
  list(
    ybounds = data.frame(
      study_id  = c("a", "b", "c"),
      variable  = c("Y_x", "Y_x", "Y_y"),
      in_bounds = c(TRUE, FALSE, NA)
    ),
    missingness = data.frame(
      study_id         = c("a", "b", "c", "d"),
      variable         = c("X_age", "X_educ", "X_party", "X_sex"),
      n_missing        = c(0L, 12L, 5L, 3L),
      has_nona_version = c(TRUE, FALSE, TRUE, TRUE),
      nona_has_na      = c(FALSE, NA, TRUE, FALSE)
    ),
    balance_joint = data.frame(
      study_id = c("a", "b"),
      p_value  = c(0.4, 0.01)
    ),
    balance_covariate = data.frame(
      study_id  = c("a", "a", "b"),
      covariate = c("X_age", "X_educ", "X_age"),
      p_value   = c(0.02, 0.6, NA)
    ),
    attrition = data.frame(
      study_id = c("a", "b"),
      outcome  = c("Y_x", "Y_x"),
      p_value  = c(0.9, 0.004)
    )
  )
}

test_that("report_checks applies the standard filters", {
  rep <- report_checks(example_checks())

  expect_s3_class(rep, "excheckr_report")
  expect_equal(rep$out_of_bounds$study_id, "b")
  expect_equal(rep$missing_no_nona$study_id, "b")
  expect_equal(rep$nona_still_missing$study_id, "c")
  expect_equal(rep$balance_joint$study_id, "b")
  expect_equal(rep$balance_covariate$covariate, "X_age")
  expect_equal(rep$attrition$study_id, "b")
})

test_that("report_checks respects alpha", {
  rep <- report_checks(example_checks(), alpha = 0.005)
  expect_equal(nrow(rep$balance_joint), 0)
  expect_equal(nrow(rep$attrition), 1)
})

test_that("report_checks skips absent elements", {
  rep <- report_checks(list(ybounds = example_checks()$ybounds))
  expect_named(rep, "out_of_bounds")
})

test_that("report_checks prefers the flag column when check_attrition_lasso ran", {
  checks <- list(
    attrition_lasso = data.frame(
      study_id = c("a", "b"),
      p_simple = c(0.9, 0.9),
      flag     = c(FALSE, TRUE)
    )
  )
  rep <- report_checks(checks)
  expect_equal(rep$attrition$study_id, "b")
})

test_that("report_checks falls back from attrition_lasso to attrition", {
  both <- example_checks()
  both$attrition_lasso <- data.frame(study_id = "z", flag = TRUE)
  rep <- report_checks(both)
  # `attrition` is present, so it wins
  expect_equal(rep$attrition$study_id, "b")
})

test_that("report_checks tolerates an empty check set", {
  rep <- report_checks(list())
  expect_length(rep, 0)
  expect_output(print(rep), "No recognized check elements")
})

test_that("report_checks reports a clean pipeline as clean", {
  clean <- list(
    ybounds = data.frame(study_id = "a", variable = "Y_x", in_bounds = TRUE),
    balance_joint = data.frame(study_id = "a", p_value = 0.4)
  )
  expect_output(print(report_checks(clean)), "All checks clean")
})

test_that("report_checks prints counts", {
  expect_output(print(report_checks(example_checks())), "outcomes outside")
})

test_that("report_checks errors on a data frame", {
  expect_error(report_checks(data.frame(x = 1)), "named list")
})
