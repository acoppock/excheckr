# tests/testthat/test-by-strata.R
#
# .by runs a check within strata. The contract is that it reproduces the
# nest/map/unnest idiom callers were writing by hand, with study_id working.

make_strat_dat <- function(seed = 1, n = 600) {
  set.seed(seed)
  dat <- data.frame(
    X_pid_3 = rep(c("Democrat", "Republican"), each = n / 2),
    topic   = rep(c("A", "B"), times = n / 2),
    X_age   = rnorm(n),
    X_educ  = rnorm(n)
  )
  dat$Z <- rbinom(n, 1, 0.5)
  dat$Y_a <- rnorm(n)
  dat$Y_a[rbinom(n, 1, 0.15) == 1] <- NA
  dat
}

hand_written <- function(dat, element) {
  raw <- dat |>
    tidyr::nest(.by = c("X_pid_3", "topic")) |>
    dplyr::mutate(r = purrr::map(data, ~ check_balance(.x, Z))) |>
    dplyr::select(-"data")
  raw |>
    dplyr::mutate(x = purrr::map(r, element)) |>
    dplyr::select(-"r") |>
    tidyr::unnest("x") |>
    dplyr::mutate(study_id = "s1")
}

norm <- function(d) {
  d <- as.data.frame(d)
  d <- d[do.call(order, d), sort(names(d))]
  rownames(d) <- NULL
  d
}

test_that(".by reproduces the hand-written nest/map/unnest idiom", {
  skip_if_not_installed("tidyr")
  dat <- make_strat_dat()

  by_arg <- check_balance(dat, Z, .by = c(X_pid_3, topic), study_id = "s1")

  expect_equal(norm(by_arg$joint_test), norm(hand_written(dat, "joint_test")))
  expect_equal(norm(by_arg$covariate_tests), norm(hand_written(dat, "covariate_tests")))
})

test_that("grouping columns are prepended and study_id is attached", {
  dat <- make_strat_dat()
  res <- check_balance(dat, Z, .by = c(X_pid_3, topic), study_id = "s1")

  expect_equal(names(res$joint_test)[1:2], c("X_pid_3", "topic"))
  expect_equal(nrow(res$joint_test), 4)          # 2 x 2 strata
  expect_equal(unique(res$joint_test$study_id), "s1")
})

test_that("a grouping column is not also tested as a covariate", {
  # X_pid_3 matches the "X_" prefix, so naive auto-selection would pick it up and
  # test a column that is constant within every stratum.
  dat <- make_strat_dat()
  res <- check_balance(dat, Z, .by = c(X_pid_3, topic))

  expect_false("X_pid_3" %in% res$covariate_tests$covariate)
  expect_setequal(unique(res$covariate_tests$covariate), c("X_age", "X_educ"))
})

test_that(".by keeps the return shape, so it composes with flatten", {
  dat <- make_strat_dat()

  as_list <- check_balance(dat, Z, .by = c(X_pid_3, topic))
  expect_named(as_list, c("covariate_tests", "joint_test"))

  flat <- check_balance(dat, Z, .by = c(X_pid_3, topic), flatten = TRUE)
  expect_s3_class(flat, "data.frame")
  expect_setequal(unique(flat$test), c("covariate", "joint"))
  expect_true(all(c("X_pid_3", "topic") %in% names(flat)))
})

test_that(".by works for check_attrition in both return shapes", {
  dat <- make_strat_dat()

  simple <- check_attrition(dat, Z, .by = c(X_pid_3, topic), study_id = "s1")
  expect_s3_class(simple, "data.frame")
  expect_equal(nrow(simple), 4)
  expect_equal(names(simple)[1:2], c("X_pid_3", "topic"))

  withcov <- check_attrition(dat, Z, covariates = c("X_age", "X_educ"),
                             .by = c(X_pid_3, topic), study_id = "s1")
  expect_named(withcov, c("simple", "coefficients", "f_test"))
  for (el in names(withcov)) {
    expect_true(all(c("X_pid_3", "topic", "study_id") %in% names(withcov[[el]])))
  }
  expect_equal(nrow(withcov$f_test), 4)
})

test_that(".by works for check_smd", {
  dat <- make_strat_dat()
  res <- check_smd(dat, Z, .by = c(X_pid_3, topic), study_id = "s1")

  expect_equal(names(res)[1:2], c("X_pid_3", "topic"))
  expect_equal(unique(res$study_id), "s1")
  expect_false("X_pid_3" %in% res$covariate)
})

test_that("strata come back in order of first appearance", {
  dat <- make_strat_dat()
  dat$topic <- factor(dat$topic, levels = c("B", "A"))   # factor order differs
  res <- check_attrition(dat, Z, .by = c(X_pid_3, topic))

  first_seen <- unique(paste(dat$X_pid_3, as.character(dat$topic)))
  got <- paste(res$X_pid_3, as.character(res$topic))
  expect_equal(got, first_seen)
})

test_that("NA forms its own stratum rather than being dropped", {
  dat <- make_strat_dat()
  dat$topic[1:50] <- NA
  res <- check_attrition(dat, Z, .by = c(X_pid_3, topic))

  expect_true(any(is.na(res$topic)))
  expect_equal(sum(res$nobs), nrow(dat[!is.na(dat$Z), ]))
})

test_that("a single-stratum .by gives the same numbers as no .by", {
  dat <- make_strat_dat()
  dat$only <- "one"

  with_by <- check_attrition(dat, Z, .by = only)
  without <- check_attrition(dat, Z)

  expect_equal(with_by$p_value, without$p_value)
  expect_equal(with_by$only, "one")
})

test_that("an empty .by selection errors rather than silently pooling", {
  dat <- make_strat_dat()
  expect_error(check_balance(dat, Z, .by = dplyr::starts_with("nope")),
               "no grouping columns")
})
