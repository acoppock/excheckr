smd_data <- function(seed = 42) {
  set.seed(seed)
  n <- 400
  dat <- data.frame(
    Z        = rep(c(0L, 1L), n / 2),
    X_age    = rnorm(n, 50, 10),
    X_party  = factor(sample(c("D", "I", "R"), n, replace = TRUE))
  )
  # X_shifted is imbalanced by exactly half a control SD
  dat$X_shifted <- rnorm(n, 0, 1) + 0.5 * dat$Z
  dat
}

test_that("check_smd recovers a known standardized difference", {
  out <- check_smd(smd_data(), Z)
  shifted <- out[out$covariate == "X_shifted", ]

  expect_equal(nrow(shifted), 1)
  expect_equal(shifted$smd, 0.5, tolerance = 0.15)
  expect_true(shifted$flag)
})

test_that("balanced covariates are not flagged", {
  out <- check_smd(smd_data(), Z)
  age <- out[out$covariate == "X_age", ]
  expect_lt(abs(age$smd), 0.1)
  expect_false(age$flag)
})

test_that("factor covariates expand to one row per non-reference level", {
  out <- check_smd(smd_data(), Z)
  party <- out[out$covariate == "X_party", ]
  expect_equal(sort(party$level), c("I", "R"))
})

test_that("multi-arm treatments give one row per arm contrast", {
  set.seed(7)
  dat <- data.frame(
    Z     = factor(rep(c("C", "T1", "T2"), each = 100)),
    X_age = rnorm(300)
  )
  out <- check_smd(dat, Z)

  expect_equal(nrow(out), 2)
  expect_equal(out$arm, c("T1", "T2"))
  expect_true(all(out$reference == "C"))
})

test_that("reference can be overridden", {
  set.seed(7)
  dat <- data.frame(
    Z     = factor(rep(c("C", "T1", "T2"), each = 100)),
    X_age = rnorm(300)
  )
  out <- check_smd(dat, Z, reference = "T2")

  expect_true(all(out$reference == "T2"))
  expect_equal(sort(out$arm), c("C", "T1"))
  expect_error(check_smd(dat, Z, reference = "nope"), "not found")
})

test_that("study_id is appended when supplied", {
  out <- check_smd(smd_data(), Z, study_id = "smith_2024_study_1")
  expect_true(all(out$study_id == "smith_2024_study_1"))
})

test_that("threshold controls the flag", {
  out <- check_smd(smd_data(), Z, threshold = 1)
  expect_false(any(out$flag))
})

test_that("a constant covariate yields NA rather than an error", {
  dat <- data.frame(Z = rep(0:1, 50), X_const = 1)
  out <- check_smd(dat, Z)
  expect_true(is.na(out$smd))
  expect_false(out$flag)
})

test_that("check_smd accepts a character covariate vector and tidyselect", {
  dat <- smd_data()
  chr <- check_smd(dat, Z, covariates = c("X_age", "X_shifted"))
  expect_equal(sort(unique(chr$covariate)), c("X_age", "X_shifted"))

  sel <- check_smd(dat, Z, covariates = dplyr::starts_with("X_a"))
  expect_equal(unique(sel$covariate), "X_age")
})

test_that("check_smd warns with no covariates and errors with one arm", {
  expect_warning(check_smd(data.frame(Z = 0:1), Z), "No covariates")
  expect_error(check_smd(data.frame(Z = 1, X_age = 2), Z), "only one arm")
})

# --- Absent covariates and weights --------------------------------------------

test_that("a covariate not in the data is an error, not a silent drop", {
  dat <- data.frame(Z = rep(0:1, 50), X_age = rnorm(100))
  expect_error(check_smd(dat, Z, covariates = c("X_age", "X_typo")),
               "not found")
})

test_that("unit weights reproduce the unweighted result", {
  set.seed(21)
  dat <- data.frame(Z = rep(0:1, 100), X_age = rnorm(200),
                    X_party = factor(sample(c("D", "R"), 200, TRUE)))
  dat$w <- 1
  expect_equal(
    check_smd(dat, Z, covariates = c("X_age", "X_party")),
    check_smd(dat, Z, covariates = c("X_age", "X_party"), weights = "w")
  )
})

test_that("weights change the SMD and match a hand computation", {
  set.seed(22)
  n <- 400
  dat <- data.frame(Z = rep(0:1, n / 2), X_age = rnorm(n))
  dat$w <- runif(n, 0.5, 2)

  out <- check_smd(dat, Z, covariates = "X_age", weights = "w")

  ctrl <- dat$Z == 0
  trt <- dat$Z == 1
  mu_c <- sum(dat$X_age[ctrl] * dat$w[ctrl]) / sum(dat$w[ctrl])
  mu_t <- sum(dat$X_age[trt] * dat$w[trt]) / sum(dat$w[trt])
  sd_c <- sqrt(sum(dat$w[ctrl] * (dat$X_age[ctrl] - mu_c)^2) / (sum(dat$w[ctrl]) - 1))

  expect_equal(out$smd, (mu_t - mu_c) / sd_c)
  expect_false(isTRUE(all.equal(out$smd, check_smd(dat, Z, covariates = "X_age")$smd)))
})

test_that("a missing weights column errors", {
  dat <- data.frame(Z = rep(0:1, 50), X_age = rnorm(100))
  expect_error(check_smd(dat, Z, weights = "nope"), "weights column")
})
