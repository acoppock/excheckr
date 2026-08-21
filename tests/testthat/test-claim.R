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
  expect_error(claim("a", 3, "no published value", expect = "shape"),
               "digits must be given")
})

test_that("a claim with nothing to compare against is an error, not an unasserted pass", {
  # The manuscript direction types the expectation at the call site, so a
  # forgotten published = has nowhere to fall back to. Before this it printed,
  # scored shape, counted as unasserted and passed every gate.
  start(format = "audit")
  expect_error(claim("papers", 106, "papers in the corpus", digits = 0),
               "asserts nothing")
  expect_equal(claim("papers", 106, "papers in the corpus", digits = 0,
                     expect = "shape"), "shape")
})

test_that("with an extraction registered, a claim it holds no value for is still shape", {
  start(published = published_fixture(), format = "id")
  expect_equal(claim("n_samples", 3, "samples the study was run in", digits = 0),
               "shape")
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
  start(format = "audit")
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
  claim("a", 3, "no spine registered", published = "3")
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
                "CLAIM SUMMARY: asserted=2 matched=1 mismatched=1 unasserted=1")
})

test_that("the audit summary prints nothing above the mismatch block", {
  # meta_conjoint rules that line with sep("CLAIM SUMMARY"), meta_propaganda with
  # 78 dashes. The separator is the caller's, so neither has to change to adopt
  # this one.
  start(format = "audit")
  claim("a", 106, "papers", published = "106")
  expect_output(claim_summary(), "^CLAIM SUMMARY: asserted=1")
})

test_that("a failure is counted as mismatched and not also as unasserted", {
  # check_claim_values.R reports unasserted as "printed as evidence only", so a
  # count that grew with every wrong number would report judgement where there
  # was a disagreement.
  start(format = "audit")
  claim("a", 106, "papers", published = "106")
  claim("b", 107, "countries", published = "48")
  claim("c", 0.19, "a bootstrap draw", published = "0.15", expect = "range")
  claim_evidence("the estimates mostly cluster near zero")
  expect_output(claim_summary(),
                "CLAIM SUMMARY: asserted=3 matched=1 mismatched=1 unasserted=2")
})

test_that("an unnamed statement is sourced to the text under the audit format", {
  # check_pdf_values.R splits the document off the manifest's source column, and
  # an NA there is a document that does not exist rather than the body text.
  start(format = "audit")
  expect_output(claim("a", 8.8, "the share", published = "8.8"), "\\(text: 8\\.8\\)")
  expect_equal(claim_manifest()$source, "text")
})

test_that("the id format leaves an unnamed statement unsourced", {
  start(published = published_fixture(), format = "id")
  expect_output(claim("wrong", 6.1, "a wrong value"), "\\[article prints 5\\.5\\]$")
})

test_that("the manifest's value column is named for the table it seeds", {
  # claim_start(published = ) reads value_paper, so a manifest collapsed to one
  # row per claim is the extraction a published article's claims file registers.
  start(format = "audit")
  claim("papers", 106, "papers", published = c("main.tex:73" = "106",
                                               "main.tex:161" = "106"))
  frozen <- dplyr::distinct(claim_manifest(), claim_id, value_paper)
  expect_equal(names(frozen), c("claim_id", "value_paper"))
  expect_silent(start(published = as.data.frame(frozen)))
})

test_that("the manifest comes from the calls rather than from parsing the source", {
  start(format = "audit")
  claim("papers", 106, "papers in the corpus",
        published = c("main.tex:73" = "106", "main.tex:161" = "106"))
  manifest <- claim_manifest()
  expect_equal(nrow(manifest), 2)
  expect_equal(manifest$source, c("main.tex:73", "main.tex:161"))
  expect_equal(manifest$value_paper, c("106", "106"))
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
  claim("a", 3, "first", published = "3")
  expect_error(claim("a", 4, "second", published = "4"), "already been claimed")
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
  claim("a", 3, "first file", published = "3")
  expect_warning(claim_start(format = "id"), "never summarized")
})

test_that("the value column punctuates thousands the way the article does", {
  # meta_ir prints a fielding year and meta_propaganda prints both a year and a
  # respondent count, four lines apart. A magnitude rule cannot get both right,
  # and the one that shipped rendered every year as 2,012 beside a manuscript
  # reading 2012, on the same line.
  start(format = "audit")
  expect_output(claim("year", 2012, "first fielding year", published = "2012"),
                "^ +2012  ")
  expect_output(claim("n", 1776, "mean study size", published = "1{,}776"),
                "^ +1,776  ")
  expect_output(claim("tex", 1550, "search pool", published = "$1{,}550$"),
                "^ +1,550  ")
})

test_that("a mismatched year is punctuated the same in the trail and the block", {
  # The trail and the MISMATCHED CLAIMS block print the same quantity three
  # lines apart, so a rule applied in one and not the other puts two spellings
  # of one number on the same page.
  start(format = "audit")
  expect_output(claim("year", 2013, "first fielding year", published = "2012"),
                "^ +2013  ")
  expect_output(claim_summary(), "\n +2013  first fielding year")
})

test_that("the label is looked up in the extraction when a call names none", {
  # Three repos in the maintenance corpus fetch the audit line's second half out
  # of the extraction rather than typing it at 235 call sites. Without the
  # lookup a conversion prints the claim's id in place of its description, which
  # reads as a working audit trail and is the half a reader needs.
  labelled <- tibble::tibble(
    claim_id = c("sim_n_total", "sim_p1_min"),
    value_paper = c("800", "0.25"),
    quantity = c("800 units are split evenly between treatment and control",
                 "the proportion p1 that responds in the initial sample")
  )
  start(published = labelled, format = "id", label_column = "quantity")
  expect_output(claim("sim_n_total", 800),
                "\\|\\| \\[match\\] 800 units are split evenly")
  expect_output(claim("sim_p1_min", 0.25),
                "\\|\\| \\[match\\] the proportion p1 that responds")
})

test_that("a label given at the call site beats the extraction's", {
  labelled <- tibble::tibble(claim_id = "a", value_paper = "3", quantity = "from the csv")
  start(published = labelled, format = "id", label_column = "quantity")
  expect_output(claim("a", 3, "from the call site"), "\\|\\| \\[match\\] from the call site")
})

test_that("without a label column the line still prints the id", {
  start(published = published_fixture(), format = "id")
  expect_output(claim("college_elite", 96), "\\|\\| \\[match\\] college_elite")
})

test_that("a label column the extraction does not have is an error", {
  # Naming a column that is absent would fall back to the id for every claim in
  # the file, so the whole trail loses its descriptions and nothing says so.
  expect_error(start(published = published_fixture(), label_column = "quantity"),
               "no column 'quantity'")
})

test_that("a claim the label column has no value for falls back to its id", {
  labelled <- tibble::tibble(claim_id = c("a", "b"), value_paper = c("3", "4"),
                             quantity = c("described", NA_character_))
  start(published = labelled, format = "id", label_column = "quantity")
  expect_output(claim("a", 3), "\\|\\| \\[match\\] described")
  expect_output(claim("b", 4), "\\|\\| \\[match\\] b")
})

test_that("a value rounding to zero from below prints an unsigned zero", {
  # No article prints -0.00, so a line showing one shows a different number from
  # the page it is meant to be read beside, and from the value it just matched.
  start(format = "id")
  expect_output(claim("a", -0.0001, "a difference of two near-equal numbers",
                      published = "0.00"),
                "= 0\\.00 \\|\\| \\[match\\]")
  expect_output(claim("b", -0.4, "a count that came out negative zero",
                      published = "0"),
                "= 0 \\|\\| \\[match\\]")
  expect_output(claim("c", -1.004, "a genuinely negative value", published = "-1.00"),
                "= -1\\.00 \\|\\| \\[match\\]")
})

test_that("a correction printed finer than the article sets the printed precision", {
  # offer-westort_coppock_green_2021 states a simulation share as 37% and its
  # erratum corrects it to 36.5%. At the article's precision the line reads 36
  # beside a note saying the correction is 36.5, and the whole point of the line
  # is that the two can be laid together.
  published <- tibble::tibble(claim_id = "share", value_paper = "37")
  errata <- tibble::tibble(entry = "3", class = "quantity", claim_ids = "share",
                           corrected_values = "36.5")
  start(published = published, errata = errata, format = "id")
  expect_output(claim("share", 36.5, "share picking the best proposal"),
                "= 36\\.5 \\|\\| \\[corrected\\].*erratum corrects to 36\\.5")
})

test_that("the expectation is read from the extraction when a call declares none", {
  # offer-westort_coppock_green_2021's extraction had recorded 22 unseeded
  # bootstrap draws and two hedged approximations since before its claims file
  # existed, and the file compared all 24 at printed precision anyway, so 18 of
  # them read as disagreements with the article.
  published <- tibble::tibble(
    claim_id = c("boot", "hedged_count", "plain"),
    value_paper = c("0.061", "300", "5"),
    expect = c("range", "derived", NA_character_)
  )
  start(published = published, format = "id", expect_column = "expect")
  expect_output(claim("boot", 0.059, "an unseeded bootstrap draw"), "\\[range\\]")
  expect_output(claim("hedged_count", 322.5, "observations per day", digits = 1),
                "\\[derived\\]")
  expect_output(claim("plain", 5, "a plain quantity"), "\\[match\\]")
})

test_that("a call site's expectation beats the extraction's", {
  published <- tibble::tibble(claim_id = "a", value_paper = "3", expect = "range")
  start(published = published, format = "id", expect_column = "expect")
  expect_output(claim("a", 9, "declared derived here", expect = "derived"), "\\[derived\\]")
})

test_that("an expect column holding a rung the ladder does not have is an error", {
  published <- tibble::tibble(claim_id = "a", value_paper = "3", expect = "unseeded")
  expect_error(start(published = published, expect_column = "expect"),
               "holds unseeded")
})

test_that("absent may not be declared in the extraction", {
  # The one exemption that has to be visible where the value would have been.
  published <- tibble::tibble(claim_id = "a", value_paper = NA_character_, expect = "absent")
  expect_error(start(published = published, expect_column = "expect"),
               "absent is declared at the call site")
})

test_that("an expect column the extraction does not have is an error", {
  expect_error(start(published = published_fixture(), expect_column = "mode"),
               "no column 'mode'")
})
