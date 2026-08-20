# Negative controls for the claims audit.
#
# These were verified by hand on 2026-08-19, by corrupting a CSV and reverting,
# twice, in two repositories. Written here they run on every change instead.
# The property each one holds is that a claims file CANNOT report a clean run
# while something is wrong, which is the only thing the audit is for: a helper
# that reports every claim as fine is indistinguishable from one that works.

# Each test is a fresh claims file. start() clears the log the way running a new
# file in a new session does; the guard against starting a second log over an
# unsummarized one is exercised on its own at the foot of this file.
start <- function(...) {
  rm(list = ls(envir = .claim_env), envir = .claim_env)
  claim_start(...)
}

published_fixture <- function() {
  tibble::tibble(
    claim_id = c("n_mturk", "college_elite", "pooled_ate", "hedge", "bootstrap",
                 "power", "no_counterpart", "wrong"),
    value_paper = c("3,001", "96", "0.30", "20", "0.15", "10", NA_character_, "5.5"),
    claim_type = c("quantity", "quantity", "quantity", "descriptive", "quantity",
                   "descriptive", "quantity", "quantity"),
    needs_block = rep("TRUE", 8)
  )
}

errata_fixture <- function() {
  tibble::tibble(
    entry = c("1", "2"),
    class = c("quantity", "wording"),
    claim_ids = c("pooled_ate", "hedge"),
    corrected_values = c("0.24", NA_character_)
  )
}


# claim_number(): the union of five hand-rolled parsers -----------------------

test_that("claim_number reads every typography the five consumers needed", {
  expect_equal(claim_number("328,358"), 328358)
  expect_equal(claim_number("1{,}550"), 1550)
  expect_equal(claim_number("22.5\\%"), 22.5)
  expect_equal(claim_number("$-7.1$"), -7.1)
  expect_equal(claim_number(".06"), 0.06)
  expect_equal(claim_number("-.06"), -0.06)
  expect_equal(claim_number("+5%"), 5)
  expect_equal(claim_number("\u22120.3"), -0.3)  # the Unicode minus U+2212
  expect_equal(claim_number("$\\approx 7$"), 7)
  expect_true(is.na(claim_number(NA_character_)))
})

test_that("claim_number is vectorized and keeps NA distinct from a parse failure", {
  expect_equal(claim_number(c("190", NA, "0.5")), c(190, NA, 0.5))
  expect_true(is.na(suppressWarnings(claim_number("the four"))))
})

test_that("claim_digits reports the precision the text prints, not the value's", {
  expect_equal(claim_digits(c("0.30", "190", "-7.1", "1{,}550")), c(2L, 0L, 1L, 0L))
  expect_true(is.na(claim_digits(NA_character_)))
})


# The verdict ladder ---------------------------------------------------------

test_that("a value matching the article scores match", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("n_mturk", 3001, "MTurk respondents"), "match")
})

test_that("a corrupted value scores MISMATCH rather than passing quietly", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("n_mturk", 3002, "MTurk respondents"), "MISMATCH")
})

test_that("an undeclared NA scores MISSING and never inherits a soft verdict", {
  # coppock_green_2022's ladder had no MISSING rung, so an NA-valued DESCRIPTIVE
  # claim fell through to hedged and passed all four gates. The claim below is
  # exactly that shape, and it must not score hedged.
  start(published = published_fixture(), format = "id")
  expect_equal(claim("hedge", NA_real_, "a descriptive claim with no value"), "MISSING")
})

test_that("an exemption must be declared at the call site, never inferred", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("no_counterpart", NA_real_, "absent from the deposit",
                     digits = 0, expect = "absent"), "absent")
})

test_that("a claim declaring absence that produces a value is an error", {
  start(published = published_fixture(), format = "id")
  expect_error(
    claim("no_counterpart", 42, "absent from the deposit", digits = 0, expect = "absent"),
    "declares no counterpart"
  )
})

test_that("a declared exemption short-circuits the comparison and is counted", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("bootstrap", 0.19, "an unseeded bootstrap draw",
                     expect = "range"), "range")
  expect_equal(claim("power", 3.2, "pooled standard errors in the ratio",
                     digits = 1, expect = "derived"), "derived")
})

test_that("a claim the article states no number for scores shape", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("n_samples", 3, "samples the study was run in", digits = 0), "shape")
})

test_that("a descriptive claim that disagrees scores hedged, and one that agrees scores match", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("hedge", 23, "on the order of twenty"), "hedged")
  start(published = published_fixture(), format = "id")
  expect_equal(claim("hedge", 20, "on the order of twenty"), "match")
})


# The errata spine: corrected, STALE and DRIFT -------------------------------

test_that("a claim a quantity erratum corrects scores corrected at the published correction", {
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  expect_equal(claim("pooled_ate", 0.24, "the pooled effect"), "corrected")
})

test_that("reproducing a value an erratum says is wrong scores STALE", {
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  expect_equal(claim("pooled_ate", 0.30, "the pooled effect"), "STALE")
})

test_that("moving off the value the erratum publishes scores DRIFT", {
  # The check nothing in the corpus performed before 2026-08-19: it ties the
  # correction a reader is given to the correction the pipeline supports today.
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  expect_equal(claim("pooled_ate", 0.26, "the pooled effect"), "DRIFT")
})

test_that("an erratum of a class that corrects no quantity leaves its claim comparable", {
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  expect_equal(claim("hedge", 20, "a wording erratum corrects no quantity"), "match")
})


# Precision, which is the whole reason the expectation is a string ------------

test_that("comparison happens at the precision the article prints", {
  start(format = "id")
  expect_equal(claim("a", 0.3049, "rounds to the printed value", published = "0.30"), "match")
  start(format = "id")
  expect_equal(claim("a", 0.3051, "rounds past the printed value", published = "0.30"),
               "MISMATCH")
})

test_that("a value can be unchanged at pipeline precision and still cross a boundary", {
  # meta_conjoint: 70.5 -> 70.6 is nothing at pipeline precision, but it now
  # rounds to 71 where the text says 70.
  start(format = "id")
  expect_equal(claim("subgroup", 70.5, "the subgroup share", published = "70"), "match")
  start(format = "id")
  expect_equal(claim("subgroup", 70.6, "the subgroup share", published = "70"), "MISMATCH")
})

test_that("tol replaces the printed-precision test for a sentence that rounds harder", {
  start(format = "id")
  expect_equal(claim("about", 7.4, "about seven percentage points",
                     published = "7", tol = 0.5), "match")
  start(format = "id")
  expect_equal(claim("about", 7.6, "about seven percentage points",
                     published = "7", tol = 0.5), "MISMATCH")
})

test_that("digits is derived from the published value rather than typed twice", {
  start(format = "id")
  expect_output(claim("a", 0.3, "derived precision", published = "0.30"),
                "CLAIM a = 0.30 ")
})

test_that("digits is required where there is no published value to derive it from", {
  start(format = "id")
  expect_error(claim("a", 3, "no published value"), "digits must be given")
})


# One quantity stated in several places --------------------------------------

test_that("a named vector compares every statement against the one pipeline value", {
  start(format = "audit")
  expect_equal(
    claim("papers", 106, "papers in the corpus",
          published = c("main.tex:73" = "106", "main.tex:161" = "106")),
    "match"
  )
})

test_that("two documents that disagree with each other fail even when one is right", {
  # The failure auditing each document separately cannot reveal. meta_conjoint:
  # main text 26.7, appendix 26.8, pipeline 26.8.
  start(format = "audit")
  expect_equal(
    claim("share", 26.8, "the choice share",
          published = c("main.tex:210" = "26.7", "appendix.tex:44" = "26.8")),
    "MISMATCH"
  )
})


# The printed line, which downstream gates parse -----------------------------

test_that("the id format is byte stable, because build_ground_truth.R parses it", {
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  expect_output(claim("n_mturk", 3001, "MTurk respondents"),
                "^CLAIM n_mturk = 3001 \\|\\| \\[match\\] MTurk respondents$")
  expect_output(claim("wrong", 6.1, "a wrong value"),
                "\\[MISMATCH\\] a wrong value \\[article prints 5\\.5\\]$")
  expect_output(claim("pooled_ate", 0.24, "the pooled effect"),
                "\\[corrected\\] the pooled effect \\[article prints 0\\.30; erratum corrects to 0\\.24\\]$")
})

test_that("the audit format marks a mismatch visibly and states the source", {
  start(format = "audit", width = c(20L, 10L))
  expect_output(claim("papers", 107, "papers", published = c("main.tex:73" = "106")),
                "main\\.tex:73: 106.*<-- MISMATCH")
})


# The gates ------------------------------------------------------------------

test_that("a failing verdict stops the build", {
  start(published = published_fixture(), format = "id")
  claim("n_mturk", 3002, "MTurk respondents")
  expect_error(assert_claims(), "did not meet the expectation")
})

test_that("an erratum naming a claim the file does not print stops the build", {
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  claim("n_mturk", 3001, "MTurk respondents")
  expect_error(assert_claims(), "correct claims this file does not print")
})

test_that("a claim the extraction declares but the file does not print stops the build", {
  start(published = published_fixture()[1:2, ], format = "id")
  claim("n_mturk", 3001, "MTurk respondents")
  expect_error(assert_claims(), "differ")
})

test_that("a clean run passes every gate", {
  start(published = published_fixture(), errata = errata_fixture(), format = "id")
  claim("n_mturk", 3001, "MTurk respondents")
  claim("college_elite", 96, "per cent holding a college degree")
  claim("pooled_ate", 0.24, "the pooled effect")
  claim("hedge", 20, "on the order of twenty")
  claim("bootstrap", 0.19, "a bootstrap draw", expect = "range")
  claim("power", 3.2, "pooled standard errors", digits = 1, expect = "derived")
  claim("no_counterpart", NA_real_, "absent", digits = 0, expect = "absent")
  claim("wrong", 5.5, "a value that reproduces")
  expect_silent(assert_claims())
})

test_that("the gates that need a spine say so rather than passing vacuously", {
  start(format = "id")
  claim("a", 3, "no spine registered", digits = 0)
  expect_message(assert_claims(), "skipping gate 2")
})


# The summary and the manifest -----------------------------------------------

test_that("the summary counts exemptions rather than merely allowing them", {
  start(published = published_fixture(), format = "id")
  claim("n_mturk", 3001, "MTurk respondents")
  claim("bootstrap", 0.19, "a bootstrap draw", expect = "range")
  claim("no_counterpart", NA_real_, "absent", digits = 0, expect = "absent")
  expect_output(claim_summary(),
                "printed=3 matched=1 corrected=0 range=1 derived=0 absent=1")
  expect_output(claim_summary(), "asserted=1 unasserted=2")
})

test_that("the audit summary line keeps the format the check scripts parse", {
  start(format = "audit")
  claim("a", 106, "papers", published = "106")
  claim("b", 107, "countries", published = "48")
  claim_evidence("the estimates mostly cluster near zero")
  expect_output(claim_summary(),
                "CLAIM SUMMARY: asserted=2 matched=1 mismatched=1 unasserted=2")
})

test_that("the manifest comes from the calls rather than from parsing the source", {
  start(format = "audit")
  claim("papers", 106, "papers in the corpus",
        published = c("main.tex:73" = "106", "main.tex:161" = "106"))
  manifest <- claim_manifest()
  expect_equal(nrow(manifest), 2)
  expect_equal(manifest$source, c("main.tex:73", "main.tex:161"))
  expect_equal(manifest$stated, c("106", "106"))
})

test_that("evidence asserts nothing and is counted as unasserted", {
  start(format = "audit")
  expect_output(claim_evidence("the six estimates are -16.0, -5.3, -3.3, 0.2, 1.7, 2.0"),
                "evidence, not asserted")
  expect_equal(nrow(claim_summary()), 0)
})


# Guards on the inputs -------------------------------------------------------

test_that("a non-character extraction is refused with the reason", {
  bad <- published_fixture()
  bad$value_paper <- as.numeric(c(3001, 96, 0.30, 20, 0.15, 10, NA, 5.5))
  expect_error(start(published = bad), "not character")
})

test_that("a non-character errata spine is refused with the reason", {
  bad <- errata_fixture()
  bad$corrected_values <- c(0.24, NA)
  expect_error(start(errata = bad), "not character")
})

test_that("an erratum whose ids and values do not line up is refused", {
  bad <- errata_fixture()
  bad$claim_ids[1] <- "pooled_ate; college_elite"
  expect_error(start(errata = bad), "names 2 claims and 1 corrected values")
})

test_that("a duplicated claim id is refused rather than silently overwritten", {
  start(format = "id")
  claim("a", 3, "first", digits = 0)
  expect_error(claim("a", 4, "second", digits = 0), "already been claimed")
})

test_that("a published value that does not parse is an error, not a shape claim", {
  start(format = "id")
  expect_error(claim("a", 3, "the article says the four", published = "the four"),
               "does not parse as a number")
})

test_that("claims before claim_start are refused", {
  rm(list = ls(envir = .claim_env), envir = .claim_env)
  expect_error(claim("a", 3, "no log", digits = 0), "Call claim_start")
})

test_that("starting a second log over an unsummarized one warns", {
  start(format = "id")
  claim("a", 3, "first file", digits = 0)
  expect_warning(claim_start(format = "id"), "never summarized")
})
