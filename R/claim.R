# Claims audit --------------------------------------------------------------
#
# A claims file ties every number a published article or a manuscript states to
# the pipeline output that produces it. The functions here are the machinery
# five such files had each hand-rolled by 2026-08-19, in two directions that
# differ in exactly one respect: where the expectation comes from.
#
#   * A MANUSCRIPT file types the expectation at the call site, as the
#     manuscript's own typography. A failure resolves into a .tex edit.
#   * A MAINTENANCE file derives it from two tables, an extraction of what the
#     article prints and a spine of what a published erratum corrects. A failure
#     resolves into an erratum.
#
# The second is the first with two inputs added, so one verdict ladder serves
# both: register no errata spine and no claim_type column and the verdicts
# `corrected`, `STALE`, `DRIFT` and `hedged` become unreachable by construction
# rather than by a mode flag.
#
# These functions print as they run, which is unusual for a package and is the
# point rather than an oversight: the printed audit trail is the artifact a
# human reads before a submission, and `claim()` returns its verdict invisibly
# so that a test can assert on it without reading the output.


.claim_env <- new.env(parent = emptyenv())


#' Convert published typography to a number
#'
#' Reduces the way an article prints a number to the number itself. Handles
#' thousands separators written either as \code{,} or as LaTeX's \code{{,}},
#' percent signs, math delimiters, backslash escapes, the approximation macro,
#' the Unicode minus U+2212, a leading \code{+}, and a leading decimal point
#' with no zero before it.
#'
#' The union of these cases is what five hand-rolled claims files needed between
#' them, and no one of the five handled all of it: two could not parse
#' \code{"1{,}550"}, two could not parse \code{"$-7.1$"}, and three could not
#' parse a negative written with the Unicode minus U+2212 rather than a hyphen.
#' Each failure is invisible until an article happens to print that form.
#'
#' @param x A character vector of published values.
#'
#' @return A numeric vector, \code{NA} where \code{x} is \code{NA}.
#'
#' @examples
#' claim_number(c("328,358", "1{,}550", "22.5\\%", ".06", "+5%", "$-7.1$"))
#'
#' @family claims audit
#' @export
claim_number <- function(x) {
  suppressWarnings(as.numeric(claim_clean(x)))
}


#' Decimal places a published value prints
#'
#' The precision the article rounds to, which is the precision a comparison
#' against it must use. A value can be unchanged in the pipeline and still cross
#' a rounding boundary, so comparing at full precision is the wrong test.
#'
#' @param x A character vector of published values.
#'
#' @return An integer vector, 0 where the value prints no decimal point and
#'   \code{NA} where \code{x} is \code{NA}.
#'
#' @examples
#' claim_digits(c("0.30", "190", "-7.1"))
#'
#' @family claims audit
#' @export
claim_digits <- function(x) {
  cleaned <- claim_clean(x)
  frac <- regmatches(cleaned, regexpr("\\.[0-9]+", cleaned))
  out <- rep(NA_integer_, length(cleaned))
  has <- grepl("\\.[0-9]+", cleaned) & !is.na(cleaned)
  out[!is.na(cleaned)] <- 0L
  out[has] <- nchar(frac) - 1L
  out
}


#' Strip published typography down to a signed decimal string
#'
#' @param x A character vector.
#' @return A character vector.
#' @keywords internal
#' @noRd
claim_clean <- function(x) {
  x <- as.character(x)
  # Backslashes go first so that \% and \approx reduce to % and approx, then the
  # braces so that 1{,}550 reduces to 1,550 before separators are removed.
  x <- gsub("\\\\", "", x)
  x <- gsub("approx", "", x, fixed = TRUE)
  x <- gsub("[{}$~%,[:space:]]", "", x)
  x <- gsub("\u2212", "-", x, fixed = TRUE)
  x <- sub("^\\+", "", x)
  # A leading decimal point with no zero before it: ".06" and "-.06".
  x <- sub("^\\.", "0.", x)
  sub("^-\\.", "-0.", x)
}


#' Begin a claims audit
#'
#' Starts the log that \code{\link{claim}} writes to, and declares once, where a
#' reader sees it, the two things every call afterwards depends on: the tables
#' the expectation is derived from, and the shape of the printed line.
#'
#' The spine is passed as data frames rather than as file paths on purpose.
#' Reading these CSVs with every column as character is the decision that keeps
#' \code{"0.30"} from becoming \code{0.3} and destroying the precision the whole
#' comparison rests on, and a package that read the files itself would bury that
#' decision. Instead the frames are validated here and a non-character value
#' column is an error naming the reason.
#'
#' @param published Optional data frame, the extraction of what the article
#'   prints. Requires \code{claim_id} and \code{value_paper}; \code{claim_type}
#'   and \code{needs_block} are used when present. Every value column must be
#'   character.
#' @param errata Optional data frame, the errata spine one row per published
#'   entry. Requires \code{entry}, \code{class}, \code{claim_ids} and
#'   \code{corrected_values}, the last two semicolon-separated and parallel.
#' @param quantity_classes Errata classes that correct a quantity (default
#'   \code{"quantity"}). An entry of any other class corrects a word, a label or
#'   a cross-reference and leaves every quantity where the article put it, so a
#'   claim it names is still expected to reproduce.
#' @param format \code{"id"} prints \code{CLAIM <id> = <value> || [verdict]
#'   <label>}, which downstream coverage gates parse and which must stay byte
#'   stable. \code{"audit"} prints the columnar manuscript form, and gives an
#'   unnamed statement the source \code{"text"} so that the manifest a coverage
#'   gate reads always names a document. The choice changes the printed line
#'   only; no verdict depends on it.
#' @return Invisibly, \code{TRUE}.
#'
#' @examples
#' claim_start()
#' claim("n_studies", 190, "studies in the corpus", published = "190")
#' claim_summary()
#'
#' @family claims audit
#' @export
claim_start <- function(published = NULL, errata = NULL,
                        quantity_classes = "quantity",
                        format = c("id", "audit")) {
  format <- match.arg(format)

  if (!is.null(.claim_env$log) && length(.claim_env$log$records) > 0 &&
      !isTRUE(.claim_env$log$summarized)) {
    warning("claim_start: discarding ", length(.claim_env$log$records),
            " claims from a log that was never summarized.", call. = FALSE)
  }

  if (!is.null(published)) published <- validate_published(published)
  corrections <- if (is.null(errata)) NULL else build_corrections(errata, quantity_classes)

  .claim_env$log <- list(
    records = list(),
    unasserted = 0L,
    published = published,
    corrections = corrections,
    format = format,
    summarized = FALSE
  )
  invisible(TRUE)
}


#' Record and score one claim
#'
#' Prints one line of the audit trail and scores the claim against the
#' expectation the extraction and the errata spine imply for it, or against an
#' expectation given here.
#'
#' Eleven rungs, in order. An \code{expect = "absent"} claim that produced a
#' value is an error, because a claim declaring it has no counterpart in the
#' deposit and then printing one is a contradiction rather than a finding. A
#' value that is \code{NA} scores \code{absent} when that was declared and
#' \code{MISSING} when it was not: an exemption that can be inferred from the
#' value will one day be inferred from a bug, so it must be declared at the call
#' site. A declared \code{range} or \code{derived} short-circuits. Then no
#' published value gives \code{shape}; a correction gives \code{corrected},
#' \code{STALE} (the pipeline reproduces a value an erratum says it should
#' contradict) or \code{DRIFT} (it contradicts the article but no longer equals
#' what the erratum publishes as the correction); equality gives \code{match}; a
#' \code{descriptive} claim type gives \code{hedged}; and the fallthrough is
#' \code{MISMATCH}.
#'
#' A claim with no published value scores \code{shape} when an extraction is
#' registered, because the extraction is what says the sentence states no number.
#' With no extraction registered there is nowhere else the expectation could come
#' from, so the same call is an error rather than a claim that prints, counts as
#' unasserted and passes: a forgotten \code{published =} must not read as a
#' sentence that never had a number in it. Declare \code{expect = "shape"} for
#' the ones that genuinely do not.
#'
#' Comparison happens at the precision the published value prints, separately
#' for each published statement, so a quantity stated in two documents at two
#' precisions is compared correctly against both.
#'
#' @param id Claim identifier, unique within the log.
#' @param value The pipeline's value, a numeric scalar or \code{NA}.
#' @param label Human-readable description, printed beside the value.
#' @param published The article's own value, as a string carrying its own
#'   typography: \code{"0.30"}, not \code{0.3}. A NAMED character vector is one
#'   quantity stated in several places, compared against the single pipeline
#'   value on one line, which is the only way a disagreement BETWEEN two
#'   passages becomes visible at all. \code{NULL} looks the value up in the
#'   extraction registered by \code{\link{claim_start}}.
#' @param corrected The value a published erratum gives as the correction.
#'   \code{NULL} looks it up in the errata spine.
#' @param digits Decimal places to print. Defaults to the precision the
#'   published value prints, which 77 of 79 measured call sites had been typing
#'   by hand, and is required only where there is no published value to derive
#'   it from.
#' @param expect One of \code{"compare"}, \code{"range"} (an unseeded bootstrap
#'   draw, where agreement to printed digits is the wrong test), \code{"derived"}
#'   (the block computes something other than the article's own token in order to
#'   test what the article says about it), \code{"shape"} (the sentence states no
#'   number, so the value is printed for a reader and compared against nothing)
#'   or \code{"absent"} (the deposit has no counterpart for this claim).
#' @param tol Absolute tolerance, replacing the comparison at printed precision.
#'   For a sentence that deliberately rounds harder than the pipeline.
#'
#' @return Invisibly, the verdict as a string.
#'
#' @family claims audit
#' @export
claim <- function(id, value, label = id, published = NULL, corrected = NULL,
                  digits = NULL, expect = "compare", tol = NULL) {
  log <- current_log()
  if (!is.character(id) || length(id) != 1 || is.na(id)) {
    stop("claim: id must be a single string.")
  }
  if (id %in% names(log$records)) {
    stop("claim: '", id, "' has already been claimed in this log.")
  }
  expect <- match.arg(expect, c("compare", "range", "derived", "shape", "absent"))

  if (is.null(published)) published <- spine_published(log, id)
  if (is.null(corrected)) corrected <- spine_corrected(log, id)
  # Names carry the source of each statement and as.character() would drop them,
  # which is what makes a quantity stated in two documents comparable on one line.
  published <- stats::setNames(as.character(published), names(published))
  stated <- claim_number(published)
  if (any(is.na(stated) & !is.na(published))) {
    stop("claim: '", id, "' has a published value that does not parse as a number: ",
         paste(published[is.na(stated) & !is.na(published)], collapse = ", "))
  }
  # With an extraction registered, a claim it holds no value for is a shape claim
  # and the ladder scores it as one. With no extraction there is nowhere else for
  # the expectation to come from, so the same call asserts nothing at all, and a
  # forgotten published = would print, count as unasserted and pass.
  if (all(is.na(stated)) && is.null(log$published) && expect == "compare") {
    stop("claim: '", id, "' has no published value and no extraction is ",
         "registered, so it asserts nothing. Pass published =, or declare ",
         "expect = \"shape\".")
  }

  dec <- claim_digits(published)
  if (is.null(digits)) {
    if (all(is.na(dec))) {
      stop("claim: '", id, "' has no published value, so digits must be given.")
    }
    digits <- max(dec, na.rm = TRUE)
  }

  printed <- if (length(value) != 1 || is.na(value)) "NA" else sprintf("%.*f", digits, value)
  if (expect == "absent" && printed != "NA") {
    stop("claim: '", id, "' declares no counterpart in the deposit and printed ",
         printed, ".")
  }

  verdict <- if (printed == "NA") {
    if (expect == "absent") "absent" else "MISSING"
  } else if (expect != "compare") {
    expect
  } else if (all(is.na(stated))) {
    "shape"
  } else if (!is.na(corrected)) {
    if (agrees(value, published, tol)) {
      "STALE"
    } else if (!agrees(value, corrected, tol)) {
      "DRIFT"
    } else {
      "corrected"
    }
  } else if (agrees(value, published, tol)) {
    "match"
  } else if (identical(spine_type(log, id), "descriptive")) {
    "hedged"
  } else {
    "MISMATCH"
  }

  # An unnamed statement has a source under the audit format, because the manifest
  # a coverage gate reads splits the source off this string and a missing one
  # reads there as a document that does not exist rather than as the body text.
  stated_text <- published_text(published,
                                source_default = if (log$format == "audit") "text")
  separator <- published_separator(published)
  log$records[[id]] <- tibble::tibble(
    claim_id = id, label = label, printed = printed,
    stated = stated_text, corrected = corrected, verdict = verdict,
    separator = separator
  )
  .claim_env$log <- log
  print_claim(log, id, label, printed, stated_text, corrected, verdict, separator)
  invisible(verdict)
}


#' Print supporting evidence for a claim about shape
#'
#' A sentence saying "mostly", "clustered", "the exception is", or naming an
#' ordering does not reduce to a boolean, and a block printing \code{TRUE} for
#' "mostly" has invented a threshold the sentence never specified. This prints
#' the evidence and asserts nothing, counting itself as printed-but-unasserted
#' so that the summary reports how much of the file is judgement rather than
#' pretending the whole file is machine-checked.
#'
#' @param note The description being supported, as a string.
#' @param data Optional data frame of supporting rows, printed beneath the note.
#'   Print the distribution, not a summary statistic: an ordering claim can break
#'   while every value it cites stays put, so print the gaps as well.
#'
#' @return Invisibly, \code{NULL}.
#'
#' @family claims audit
#' @export
claim_evidence <- function(note, data = NULL) {
  log <- current_log()
  cat("  (evidence, not asserted) ", note, "\n", sep = "")
  if (!is.null(data)) print(as.data.frame(data))
  log$unasserted <- log$unasserted + 1L
  .claim_env$log <- log
  invisible(NULL)
}


#' Summarize a claims audit
#'
#' Prints the stable summary line the project's own check script parses, and
#' returns the scored claims. The exemptions are counted rather than merely
#' allowed: a file that let that set grow in silence would report the same clean
#' run whether it asserted every claim or none.
#'
#' \code{unasserted} counts what the file printed without comparing it: the
#' evidence notes, plus the claims an \code{expect} exempted. A failure is not
#' unasserted, it is asserted and disagreeing, and counting it in both places
#' would let a check script that reports \code{unasserted} as "printed as
#' evidence only" grow every time a number went wrong.
#'
#' Nothing decorative is printed above the summary under \code{format = "audit"}.
#' The separator is the project's own, cat it at the call site, because the two
#' manuscript projects rule that line differently and neither should have to
#' change to adopt this one.
#'
#' @return Invisibly, a tibble of one row per claim.
#'
#' @family claims audit
#' @export
claim_summary <- function() {
  log <- current_log()
  scored <- claim_scored(log)
  counts <- verdict_counts(scored)
  failing <- scored[scored$verdict %in% claim_failures(), , drop = FALSE]

  if (log$format == "audit") {
    if (nrow(failing) > 0) {
      cat("MISMATCHED CLAIMS:\n")
      for (i in seq_len(nrow(failing))) {
        cat(audit_line(failing$printed[i], failing$label[i], failing$stated[i],
                       failing$separator[i]))
      }
      cat("\n")
    }
    cat(sprintf("CLAIM SUMMARY: asserted=%d matched=%d mismatched=%d unasserted=%d\n",
                nrow(scored), counts[["matched"]] + counts[["corrected"]],
                nrow(failing), nrow(scored) - counts[["matched"]] -
                  counts[["corrected"]] - counts[["failed"]] + log$unasserted))
  } else {
    cat("\nCLAIM SUMMARY: ",
        paste(names(counts), counts, sep = "=", collapse = " "), "\n", sep = "")
    cat("CLAIM SUMMARY: asserted=", counts[["matched"]] + counts[["corrected"]],
        " unasserted=", nrow(scored) - counts[["matched"]] - counts[["corrected"]] -
          counts[["failed"]] + log$unasserted, "\n", sep = "")
  }

  log$summarized <- TRUE
  .claim_env$log <- log
  invisible(scored)
}


#' Assert the claims audit held
#'
#' The verdicts are assertions only if something reads them. Four gates do, and
#' together they are the property a claims file exists to hold: every quantity
#' the article prints is either reproduced or corrected by a published erratum,
#' and nothing is quietly in neither state.
#'
#' Gate 1 fails on any failing verdict. Gate 2 requires every claim a quantity
#' erratum names to have been printed and printed as \code{corrected}, since an
#' entry naming a claim the file does not print is a correction nothing checks.
#' Gate 3 requires the printed set to equal the set the extraction declares needs
#' a block. Gate 4 asserts the verdict counts partition the claims. Gates 2 and 3
#' need the spine and are skipped, with a message, when none was registered.
#'
#' @return Invisibly, the scored claims tibble.
#'
#' @family claims audit
#' @export
assert_claims <- function() {
  log <- current_log()
  scored <- claim_scored(log)

  failing <- scored[scored$verdict %in% claim_failures(), , drop = FALSE]
  if (nrow(failing) > 0) {
    print(as.data.frame(failing))
    stop(nrow(failing), " claims did not meet the expectation the article and the ",
         "errata spine set for them. A DRIFT has two causes: the pipeline has moved ",
         "off the value the note publishes, or the errata note has not been ",
         "re-rendered since it last moved. Re-render before investigating.",
         call. = FALSE)
  }

  if (is.null(log$corrections)) {
    message("assert_claims: no errata spine registered, skipping gate 2.")
  } else {
    uncovered <- setdiff(log$corrections$claim_id, scored$claim_id)
    if (length(uncovered) > 0) {
      print(as.data.frame(log$corrections[log$corrections$claim_id %in% uncovered, ]))
      stop("Errata entries correct claims this file does not print.", call. = FALSE)
    }
    named <- scored$verdict[scored$claim_id %in% log$corrections$claim_id]
    if (!all(named == "corrected")) {
      stop("A claim a quantity erratum names did not score corrected.", call. = FALSE)
    }
  }

  if (is.null(log$published) || !"needs_block" %in% names(log$published)) {
    message("assert_claims: no needs_block column registered, skipping gate 3.")
  } else {
    needs <- log$published$needs_block
    declared <- log$published$claim_id[!is.na(needs) & as.character(needs) == "TRUE"]
    if (!setequal(declared, scored$claim_id)) {
      print(list(declared_not_printed = setdiff(declared, scored$claim_id),
                 printed_not_declared = setdiff(scored$claim_id, declared)))
      stop("The printed claims and the claims the extraction declares differ.",
           call. = FALSE)
    }
  }

  counts <- verdict_counts(scored)
  stopifnot(sum(counts[-1]) - counts[["failed"]] == nrow(scored))
  invisible(scored)
}


#' The manifest of asserted values
#'
#' One row per published statement a claim asserted, which is what a coverage
#' check reads to know which of the manuscript's numbers are covered. It comes
#' from the \code{claim()} calls themselves rather than from parsing the file's
#' source, because a source parser is line-based and silently misses a call whose
#' arguments wrap, reporting an asserted value as uncovered.
#'
#' It is also where the claims table of a published article comes from. One row
#' per statement is what a coverage check needs, since each mention has to be
#' found in the document; one row per claim is what \code{\link{claim_start}}
#' registers, since a quantity has one value however many times the text states
#' it. \code{dplyr::distinct(claim_id, value_paper)} converts the first into the
#' second, and a paper whose two passages disagree survives that as two rows and
#' is refused, which is correct: there is no single value to freeze.
#'
#' The caller writes it. The path is a decision belonging at the call site rather
#' than inside the package.
#'
#' @return A tibble with \code{claim_id}, \code{label}, \code{source} and
#'   \code{value_paper}. The value column carries the name
#'   \code{\link{claim_start}} reads it back under, so collapsing the manifest
#'   to one row per claim gives the extraction a published article's claims file
#'   registers.
#'
#' @family claims audit
#' @export
claim_manifest <- function() {
  log <- current_log()
  scored <- claim_scored(log)
  rows <- lapply(seq_len(nrow(scored)), function(i) {
    parts <- strsplit(scored$stated[i], "; ", fixed = TRUE)[[1]]
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0) return(NULL)
    has_source <- grepl(": ", parts, fixed = TRUE)
    tibble::tibble(
      claim_id = scored$claim_id[i],
      label = scored$label[i],
      source = ifelse(has_source, sub(":\\s.*$", "", parts), NA_character_),
      value_paper = ifelse(has_source, sub("^.*:\\s", "", parts), parts)
    )
  })
  dplyr::bind_rows(rows)
}


# Internals -----------------------------------------------------------------

#' The verdicts that fail a gate
#' @keywords internal
#' @noRd
claim_failures <- function() c("MISMATCH", "STALE", "DRIFT", "MISSING")


#' The current log, or an error naming what is missing
#' @keywords internal
#' @noRd
current_log <- function() {
  if (is.null(.claim_env$log)) {
    stop("No claims log. Call claim_start() at the top of the file.", call. = FALSE)
  }
  .claim_env$log
}


#' Bind the recorded claims into one tibble
#' @keywords internal
#' @noRd
claim_scored <- function(log) {
  if (length(log$records) == 0) {
    return(tibble::tibble(claim_id = character(), label = character(),
                          printed = character(), stated = character(),
                          corrected = character(), verdict = character(),
                          separator = logical()))
  }
  dplyr::bind_rows(log$records)
}


#' Count the verdicts, in the order the summary prints them
#' @keywords internal
#' @noRd
verdict_counts <- function(scored) {
  exemptions <- c("match", "corrected", "range", "derived", "absent", "hedged", "shape")
  counts <- vapply(exemptions, function(v) sum(scored$verdict == v), integer(1))
  names(counts)[1] <- "matched"
  c(printed = nrow(scored), counts,
    failed = sum(scored$verdict %in% claim_failures()))
}


#' Does the pipeline value agree with a published statement?
#'
#' Compared at the precision each statement prints, which is what makes a value
#' that is unchanged in the pipeline but has crossed a rounding boundary a
#' disagreement. `tol` replaces that with an absolute tolerance, for a sentence
#' that deliberately rounds harder than the pipeline.
#'
#' @keywords internal
#' @noRd
agrees <- function(value, text, tol = NULL) {
  want <- claim_number(text)
  dec <- claim_digits(text)
  if (length(want) == 0 || all(is.na(want)) || length(value) != 1 || is.na(value)) {
    return(FALSE)
  }
  ok <- mapply(function(w, d) {
    if (is.na(w)) return(FALSE)
    if (!is.null(tol)) abs(value - w) <= tol else abs(round(value, d) - w) < 1e-9
  }, want, dec)
  all(ok)
}


#' Render published statements as one string
#' @keywords internal
#' @noRd
published_text <- function(published, source_default = NULL) {
  if (length(published) == 0 || all(is.na(published))) return(NA_character_)
  src <- names(published)
  if (is.null(src)) {
    if (is.null(source_default)) return(paste(published, collapse = "; "))
    src <- rep(source_default, length(published))
  }
  paste(paste0(src, ": ", published), collapse = "; ")
}


#' Does the article group this number's thousands?
#'
#' The audit trail is read against the article's own typography, so the value
#' column has to punctuate a number the way the page does. No rule from the
#' value alone gets that right: meta_propaganda states a year as 2012 and a
#' respondent count as 1,776 four lines apart, and a magnitude threshold picked
#' to spare the year silently strips the separator off every four-digit count.
#' The article's statement is the only thing that knows, so read it. With no
#' published value there is nothing to match, and the grouped form is the
#' readable one.
#'
#' @keywords internal
#' @noRd
published_separator <- function(published) {
  x <- as.character(published)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(TRUE)
  any(grepl("[0-9],[0-9]", gsub("[\\\\{}]", "", x)))
}


#' One line of the columnar audit trail
#'
#' The value is right-aligned and the label follows it, so that both columns
#' line up down a long run without anyone having to declare how wide they are.
#' A line is printed the moment its claim runs, before the labels below it
#' exist, so padding the label instead would mean either guessing a width or
#' holding the whole trail back until the end.
#'
#' \code{separator} carries whether the article groups this number's thousands,
#' because the line exists to be read against the article's own typography and
#' there is no rule from the value alone that gets both a year and a respondent
#' count right. See published_separator().
#'
#' @keywords internal
#' @noRd
audit_line <- function(printed, label, stated, separator = TRUE, marker = "") {
  shown <- if (printed == "NA" || grepl(".", printed, fixed = TRUE) || !separator) {
    printed
  } else {
    formatC(as.numeric(printed), format = "d", big.mark = ",")
  }
  # A claim on a sentence that states no number has nothing to show in the
  # parentheses, and an empty pair of them reads as a value that went missing.
  against <- if (is.na(stated)) "" else sprintf("   (%s)", stated)
  sprintf("  %10s  %s%s%s\n", shown, label, against, marker)
}


#' Print one line of the audit trail
#' @keywords internal
#' @noRd
print_claim <- function(log, id, label, printed, stated, corrected, verdict,
                        separator = TRUE) {
  if (log$format == "audit") {
    marker <- if (verdict %in% claim_failures()) "   <-- MISMATCH" else ""
    cat(audit_line(printed, label, stated, separator, marker))
    return(invisible(NULL))
  }

  # The article's own number is printed beside a value that is not being compared
  # to it, so a reader can still lay the two side by side where the audit has
  # declined to.
  against <- if (verdict == "derived") {
    paste0(" [the sentence names ", stated, "]")
  } else if (verdict %in% c("corrected", "STALE", "DRIFT")) {
    paste0(" [article prints ", stated, "; erratum corrects to ", corrected, "]")
  } else if (!is.na(stated) && verdict != "match") {
    paste0(" [article prints ", stated, "]")
  } else {
    ""
  }
  cat("CLAIM ", id, " = ", printed, " || [", verdict, "] ", label, against, "\n", sep = "")
  invisible(NULL)
}


#' The extraction's value for a claim, or NA
#' @keywords internal
#' @noRd
spine_published <- function(log, id) {
  if (is.null(log$published)) return(NA_character_)
  value <- log$published$value_paper[log$published$claim_id == id]
  if (length(value) != 1) NA_character_ else value
}


#' The extraction's claim type, or NA
#' @keywords internal
#' @noRd
spine_type <- function(log, id) {
  if (is.null(log$published) || !"claim_type" %in% names(log$published)) {
    return(NA_character_)
  }
  type <- log$published$claim_type[log$published$claim_id == id]
  if (length(type) != 1) NA_character_ else type
}


#' The correction a quantity erratum publishes for a claim, or NA
#' @keywords internal
#' @noRd
spine_corrected <- function(log, id) {
  if (is.null(log$corrections)) return(NA_character_)
  value <- log$corrections$corrected[log$corrections$claim_id == id]
  if (length(value) != 1) NA_character_ else value
}


#' Validate the extraction
#' @keywords internal
#' @noRd
validate_published <- function(published) {
  if (!is.data.frame(published)) {
    stop("claim_start: published must be a data frame, got ",
         class(published)[1], ".")
  }
  missing <- setdiff(c("claim_id", "value_paper"), names(published))
  if (length(missing) > 0) {
    stop("claim_start: published needs columns ", paste(missing, collapse = ", "), ".")
  }
  if (!is.character(published$value_paper)) {
    stop("claim_start: published$value_paper is ", class(published$value_paper)[1],
         ", not character. Read the extraction with every column as character: a ",
         "reader left to guess returns 0.3 for \"0.30\" and destroys the precision ",
         "the comparison depends on.")
  }
  if (anyDuplicated(published$claim_id) > 0) {
    stop("claim_start: published has duplicate claim_id values.")
  }
  published
}


#' Unnest the errata spine into one row per corrected claim
#' @keywords internal
#' @noRd
build_corrections <- function(errata, quantity_classes) {
  if (!is.data.frame(errata)) {
    stop("claim_start: errata must be a data frame, got ", class(errata)[1], ".")
  }
  missing <- setdiff(c("entry", "class", "claim_ids", "corrected_values"), names(errata))
  if (length(missing) > 0) {
    stop("claim_start: errata needs columns ", paste(missing, collapse = ", "), ".")
  }
  if (!is.character(errata$corrected_values)) {
    stop("claim_start: errata$corrected_values is ", class(errata$corrected_values)[1],
         ", not character. Read the spine with every column as character: it carries ",
         "\"0.30\", and a reader left to guess returns 0.3.")
  }

  quantity <- errata[errata$class %in% quantity_classes, , drop = FALSE]
  rows <- lapply(seq_len(nrow(quantity)), function(i) {
    ids <- trimws(strsplit(quantity$claim_ids[i], ";", fixed = TRUE)[[1]])
    values <- trimws(strsplit(quantity$corrected_values[i], ";", fixed = TRUE)[[1]])
    if (length(ids) != length(values)) {
      stop("claim_start: errata entry ", quantity$entry[i], " names ", length(ids),
           " claims and ", length(values), " corrected values.")
    }
    tibble::tibble(entry = quantity$entry[i], claim_id = ids, corrected = values)
  })
  corrections <- dplyr::bind_rows(rows)
  if (nrow(corrections) == 0) return(corrections)
  if (anyDuplicated(corrections$claim_id) > 0) {
    stop("claim_start: two quantity errata correct the same claim.")
  }
  if (anyNA(corrections$corrected)) {
    stop("claim_start: a quantity erratum names a claim with no corrected value.")
  }
  corrections
}
