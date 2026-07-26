make_check_dir <- function() {
  d <- tempfile("checks")
  dir.create(d)
  saveRDS(
    list(
      ybounds = data.frame(study_id = "a", variable = "Y_x", in_bounds = TRUE),
      balance_joint = data.frame(study_id = "a", p_value = 0.4)
    ),
    file.path(d, "a_checks.rds")
  )
  saveRDS(
    list(
      ybounds = data.frame(study_id = "b", variable = "Y_x", in_bounds = FALSE),
      balance_joint = data.frame(study_id = "b", p_value = 0.01)
    ),
    file.path(d, "b_checks.rds")
  )
  d
}

test_that("stack_checks binds elements across files", {
  d <- make_check_dir()
  out <- stack_checks(d)

  expect_named(out, c("ybounds", "balance_joint"))
  expect_equal(nrow(out$ybounds), 2)
  expect_equal(out$ybounds$study_id, c("a", "b"))
  expect_equal(out$balance_joint$p_value, c(0.4, 0.01))
})

test_that("stack_checks drops NULL and zero-row elements", {
  d <- make_check_dir()
  saveRDS(
    list(
      ybounds = data.frame(study_id = character(0), variable = character(0),
                           in_bounds = logical(0)),
      balance_joint = NULL
    ),
    file.path(d, "c_checks.rds")
  )

  out <- stack_checks(d)
  expect_equal(nrow(out$ybounds), 2)
  expect_equal(nrow(out$balance_joint), 2)
})

test_that("stack_checks keeps elements present in only some files", {
  d <- make_check_dir()
  saveRDS(
    list(attrition = data.frame(study_id = "c", outcome = "Y_x", p_value = 0.9)),
    file.path(d, "c_checks.rds")
  )

  out <- stack_checks(d)
  expect_true("attrition" %in% names(out))
  expect_equal(nrow(out$attrition), 1)
})

test_that("stack_checks honours the elements argument", {
  d <- make_check_dir()
  out <- stack_checks(d, elements = "ybounds")
  expect_named(out, "ybounds")
})

test_that("stack_checks returns an empty tibble for a wholly empty element", {
  d <- make_check_dir()
  out <- stack_checks(d, elements = "attrition")
  expect_equal(nrow(out$attrition), 0)
})

test_that("stack_checks skips a previously written all_checks.rds", {
  d <- make_check_dir()
  saveRDS(stack_checks(d), file.path(d, "all_checks.rds"))

  out <- stack_checks(d)
  expect_equal(nrow(out$ybounds), 2)
})

test_that("stack_checks errors informatively", {
  expect_error(stack_checks(file.path(tempdir(), "nope-does-not-exist")),
               "directory not found")

  d <- tempfile("empty")
  dir.create(d)
  expect_error(stack_checks(d), "no files matching")

  saveRDS(data.frame(x = 1), file.path(d, "bad_checks.rds"))
  expect_error(stack_checks(d), "named list of tibbles")
})
