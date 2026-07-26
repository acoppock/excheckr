uniform_tests <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(
    study_id = rep(letters[1:10], each = n / 10),
    p_value  = runif(n)
  )
}

test_that("summarize_check_pvalues reports uniform behaviour for uniform input", {
  out <- summarize_check_pvalues(uniform_tests())

  expect_equal(nrow(out), 1)
  expect_equal(out$n_tests, 200)
  expect_equal(out$expected_below, 5)
  expect_gt(out$ks_p, 0.05)
  expect_lt(out$pct_below, 15)
})

test_that("summarize_check_pvalues detects a non-uniform pile-up", {
  skewed <- data.frame(p_value = c(runif(20, 0, 0.01), runif(20, 0, 1)))
  out <- summarize_check_pvalues(skewed)

  expect_lt(out$ks_p, 0.05)
  expect_gt(out$pct_below, 40)
})

test_that("FDR adjustment is never more liberal than the raw rate", {
  tests <- uniform_tests()
  across <- summarize_check_pvalues(tests)
  within <- summarize_check_pvalues(tests, group = "study_id")

  expect_lte(across$n_below_fdr, across$n_below)
  expect_lte(within$n_below_fdr, within$n_below)
  expect_equal(across$n_tests, within$n_tests)
})

test_that("summarize_check_pvalues drops missing p-values", {
  tests <- data.frame(p_value = c(0.1, NA, 0.9))
  expect_equal(summarize_check_pvalues(tests)$n_tests, 2)
})

test_that("summarize_check_pvalues errors informatively", {
  expect_error(summarize_check_pvalues(1:10), "data frame")
  expect_error(summarize_check_pvalues(data.frame(p = 0.5)), "not found")
  expect_error(summarize_check_pvalues(data.frame(p_value = NA_real_)),
               "no non-missing")
  expect_error(summarize_check_pvalues(uniform_tests(), group = "nope"),
               "not found")
})

test_that("summarize_check_pvalues honours a non-default p_col and alpha", {
  tests <- data.frame(p_simple = c(0.001, 0.2, 0.8, 0.9))
  out <- summarize_check_pvalues(tests, p_col = "p_simple", alpha = 0.25)
  expect_equal(out$n_below, 2)
  expect_equal(out$expected_below, 25)
})

test_that("plot_check_pvalues returns a ggplot", {
  g <- plot_check_pvalues(uniform_tests())
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$data), 200)
})

test_that("plot_check_pvalues facets when fdr = TRUE", {
  g_across <- plot_check_pvalues(uniform_tests(), fdr = TRUE)
  expect_equal(nrow(g_across$data), 400)
  expect_equal(levels(g_across$data$adjustment)[2],
               "FDR-adjusted (across all tests)")

  g_within <- plot_check_pvalues(uniform_tests(), group = "study_id", fdr = TRUE)
  expect_equal(levels(g_within$data$adjustment)[2],
               "FDR-adjusted (within study_id)")
})

test_that("plot_check_pvalues errors on a missing group column", {
  expect_error(
    plot_check_pvalues(uniform_tests(), group = "nope", fdr = TRUE),
    "not found"
  )
})
