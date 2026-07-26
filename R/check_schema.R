#' Check that a cleaned data frame conforms to its schema
#'
#' Structural counterpart to the statistical \code{check_*} family
#' (\code{check_balance}, \code{check_attrition}, \code{check_y_bounds}, ...).
#' Those check whether an experiment is sound; this checks whether the cleaned
#' data has the shape the pipeline expects: a unique row key, a treatment and an
#' outcome column, weights, no raw \code{haven_labelled} columns leaking, and no
#' columns outside the declared contract.
#'
#' Everything project-specific is an argument, so one tested implementation
#' serves every schema: the flat one-row-per-respondent schema
#' (\code{key = "resp_id"}), the within-subjects schema
#' (\code{key = c("resp_id", "topic")}), and the block-long schema
#' (\code{key = c("resp_id", "block", "wave")}). Conjoint data, whose role
#' prefixes and cardinality differ, keeps its own checker but can reuse
#' \code{assert_key_unique} and \code{assert_no_labelled}.
#'
#' @param data A data frame or tibble.
#' @param key Character vector of columns that jointly identify a row (default
#'   \code{"resp_id"}). The check confirms they are all present and that their
#'   combination is unique.
#' @param meta Character vector of allowed non-role columns (study metadata such
#'   as \code{"topic"}, \code{"study_id"}). Columns that are neither key,
#'   weights, a role column, nor listed here are flagged as possible leaks.
#' @param treatment,outcome,covariate Prefixes identifying the treatment,
#'   outcome, and covariate columns (defaults \code{"Z"}, \code{"Y"},
#'   \code{"X_"}). \code{treatment} and \code{outcome} match either a bare
#'   prefix (e.g. \code{"Z"}) or one followed by an underscore (\code{"Z_party"}).
#' @param require_weights Logical. If \code{TRUE} (the default), a non-NA
#'   \code{weights} column is required (use \code{weights = 1} when a study has
#'   no survey weights).
#' @param study_id Optional character scalar appended as a \code{study_id}
#'   column, so results stack across studies.
#'
#' @return A tibble with one row per check: \code{check} (name),
#'   \code{severity} (\code{"error"} or \code{"warn"}), \code{pass} (logical),
#'   \code{detail} (offending columns, or \code{NA}), and optionally
#'   \code{study_id}. Use \code{assert_schema} to turn failures into a stop at
#'   save time.
#'
#' @examples
#' dat <- data.frame(resp_id = 1:3, weights = 1,
#'                   X_age = c(20, 30, 40), Z_party = c(0, 1, 0), Y = c(0, 1, 1))
#' check_schema(dat)
#' check_schema(dat, meta = "topic", study_id = "my_study")
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows n_distinct
#' @family schema assertions
#' @export
check_schema <- function(data, key = "resp_id", meta = character(),
                         treatment = "Z", outcome = "Y", covariate = "X_",
                         require_weights = TRUE, study_id = NULL) {
  nm <- names(data)
  x_cols <- grep(paste0("^", covariate), nm, value = TRUE)
  z_cols <- grep(paste0("^", treatment, "(_|$)"), nm, value = TRUE)
  y_cols <- grep(paste0("^", outcome, "(_|$)"), nm, value = TRUE)
  role_cols <- c(x_cols, z_cols, y_cols)

  rows <- list()
  add <- function(check, severity, pass, detail = NA_character_) {
    rows[[length(rows) + 1]] <<- tibble::tibble(
      check = check, severity = severity, pass = pass, detail = detail
    )
  }

  key_present <- all(key %in% nm)
  add("key_present", "error", key_present,
      if (key_present) NA_character_ else paste("missing:", paste(setdiff(key, nm), collapse = ", ")))
  add("key_unique", "error",
      if (key_present) !any(duplicated(data[key])) else NA, NA_character_)

  if (require_weights) {
    w_present <- "weights" %in% nm
    add("weights_present", "error", w_present)
    add("weights_no_na", "error", if (w_present) !any(is.na(data[["weights"]])) else NA)
  }

  add("has_treatment", "error", length(z_cols) >= 1)
  add("has_outcome", "error", length(y_cols) >= 1)

  labelled <- nm[vapply(data, function(x) inherits(x, "haven_labelled"), logical(1))]
  add("no_labelled", "error", length(labelled) == 0,
      if (length(labelled)) paste(labelled, collapse = ", ") else NA_character_)

  extra <- setdiff(nm, c(key, if (require_weights) "weights", meta, role_cols))
  add("no_extra_columns", "warn", length(extra) == 0,
      if (length(extra)) paste(extra, collapse = ", ") else NA_character_)

  const_z <- z_cols[vapply(z_cols, function(z) dplyr::n_distinct(data[[z]], na.rm = TRUE) < 2, logical(1))]
  add("treatment_varies", "warn", length(const_z) == 0,
      if (length(const_z)) paste(const_z, collapse = ", ") else NA_character_)

  na_cols <- c(x_cols, y_cols)
  allna <- na_cols[vapply(na_cols, function(v) all(is.na(data[[v]])), logical(1))]
  add("no_all_na_columns", "warn", length(allna) == 0,
      if (length(allna)) paste(allna, collapse = ", ") else NA_character_)

  result <- dplyr::bind_rows(rows)
  if (!is.null(study_id)) result$study_id <- study_id
  result
}


#' Assert a cleaned data frame conforms to its schema
#'
#' Runs \code{check_schema} and turns the result into a hard stop, for use at
#' the tail of a cleaning script (before \code{write_rds}) so a nonconforming
#' study fails loudly where it is built. Any failing \code{"error"} check stops;
#' any failing \code{"warn"} check emits a warning. Returns the report invisibly.
#'
#' @inheritParams check_schema
#' @param ... Passed to \code{check_schema}.
#'
#' @return The \code{check_schema} report tibble, invisibly.
#'
#' @examples
#' dat <- data.frame(resp_id = 1:3, weights = 1, Z = c(0, 1, 0), Y = c(0, 1, 1))
#' assert_schema(dat)
#'
#' @family schema assertions
#' @export
assert_schema <- function(data, ..., study_id = NULL) {
  res <- check_schema(data, ..., study_id = study_id)
  lab <- if (is.null(study_id)) "" else paste0(study_id, ": ")
  failed <- !vapply(res$pass, isTRUE, logical(1))

  errs <- res[res$severity == "error" & failed, ]
  if (nrow(errs) > 0) {
    msg <- paste(ifelse(is.na(errs$detail), errs$check,
                        paste0(errs$check, " (", errs$detail, ")")), collapse = "; ")
    stop(lab, "schema check failed: ", msg, call. = FALSE)
  }
  warns <- res[res$severity == "warn" & failed, ]
  for (i in seq_len(nrow(warns))) {
    warning(lab, warns$check[i],
            if (!is.na(warns$detail[i])) paste0(": ", warns$detail[i]) else "",
            call. = FALSE)
  }
  invisible(res)
}


#' Assert that a data frame's row key is unique
#'
#' Hard-errors if the combination of \code{key} columns is duplicated. A small
#' composable primitive used by \code{check_schema} and reusable by bespoke
#' checkers (e.g. a conjoint \code{check_conjoint}).
#'
#' @param data A data frame or tibble.
#' @param key Character vector of columns that jointly identify a row.
#'
#' @return \code{TRUE}, invisibly.
#'
#' @examples
#' assert_key_unique(data.frame(resp_id = 1:3), key = "resp_id")
#'
#' @family schema assertions
#' @export
assert_key_unique <- function(data, key) {
  missing <- setdiff(key, names(data))
  if (length(missing)) stop("key column(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  dup <- duplicated(data[key])
  if (any(dup))
    stop("row key not unique: ", sum(dup), " duplicated rows on ", paste(key, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}


#' Assert that no column is a leaked haven_labelled
#'
#' Hard-errors if any column still carries the \code{haven_labelled} class, i.e.
#' a raw labelled column read from a \code{.dta}/\code{.sav} that was never
#' recoded or stripped. A composable primitive used by \code{check_schema} and
#' reusable by bespoke checkers.
#'
#' @param data A data frame or tibble.
#'
#' @return \code{TRUE}, invisibly.
#'
#' @examples
#' assert_no_labelled(data.frame(x = 1:3))
#'
#' @family schema assertions
#' @export
assert_no_labelled <- function(data) {
  labelled <- names(data)[vapply(data, function(x) inherits(x, "haven_labelled"), logical(1))]
  if (length(labelled))
    stop("haven_labelled columns present: ", paste(labelled, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
