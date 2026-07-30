#' Stack per-study check results into one list of tibbles
#'
#' Reads every per-study check file in \code{dir} and binds them element-wise
#' into a single named list of tibbles. Each file is expected to hold a named
#' list of tibbles written by a per-study checking script, typically the output
#' of \code{\link{check_y_bounds}}, \code{\link{check_missingness_nona}},
#' \code{\link{check_balance}}, and \code{\link{check_attrition}} with a
#' \code{study_id} supplied.
#'
#' Elements that are \code{NULL} or have zero rows in a given file are dropped
#' before binding, so a study that contributes nothing to one check does not
#' break the stack for the others. An element present in some files but not
#' others is still returned, built from whichever files have it.
#'
#' @param dir Path to the directory holding the per-study check files
#'   (default \code{"checks"}).
#' @param pattern Regular expression identifying the per-study files
#'   (default \code{"_checks\\\\.rds$"}).
#' @param exclude Regular expression for files to skip, matched against the
#'   base name. Defaults to \code{"^all_checks\\\\.rds$"} so that re-running
#'   over a directory that already holds a stacked file is safe.
#' @param elements Character vector naming the list elements to stack, or
#'   \code{NULL} (default) to stack the union of element names found across
#'   all files.
#' @param warn_schema Logical. Warn when the files contributing one element
#'   disagree about their columns (default \code{TRUE}). Set \code{FALSE} in a
#'   project where that is expected: a corpus whose studies are stratified on
#'   different variables produces different grouping columns by design, and the
#'   warning cannot tell that apart from staleness.
#'
#' @return A named list of tibbles, one per element name.
#'
#' @section Stale files hide in a successful stack:
#' \code{dplyr::bind_rows} unions column names and fills the gaps with \code{NA}.
#' That tolerance is what lets a study which skipped one check stack alongside
#' studies that ran it, and it is also how a stale file hides: when one study's
#' results predate a change in what a check returns, its rows arrive missing the new
#' columns while still carrying whatever the old version put in the shared ones. The
#' stack succeeds, the corpus looks fully re-run, and it is not. A real instance:
#' two per-study files a day older than the rest left \code{estimable} as \code{NA}
#' on eight rows and shifted the count of informative attrition tests. Hence the
#' warning, and hence \code{warn_schema}.
#'
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' saveRDS(
#'   list(ybounds = data.frame(study_id = "a", variable = "Y_x", in_bounds = TRUE)),
#'   file.path(d, "a_checks.rds")
#' )
#' saveRDS(
#'   list(ybounds = data.frame(study_id = "b", variable = "Y_x", in_bounds = FALSE)),
#'   file.path(d, "b_checks.rds")
#' )
#' stack_checks(d)
#'
#' @importFrom dplyr bind_rows
#' @family across-study summaries
#' @export
stack_checks <- function(dir = "checks",
                         pattern = "_checks\\.rds$",
                         exclude = "^all_checks\\.rds$",
                         elements = NULL,
                         warn_schema = TRUE) {
  if (!dir.exists(dir)) {
    stop("stack_checks: directory not found: ", dir)
  }

  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!is.null(exclude)) {
    files <- files[!grepl(exclude, basename(files))]
  }

  if (length(files) == 0) {
    stop("stack_checks: no files matching '", pattern, "' found in ", dir,
         ". Run the per-study checking scripts first.")
  }

  contents <- lapply(files, readRDS)

  not_list <- !vapply(contents, function(x) is.list(x) && !is.data.frame(x), logical(1))
  if (any(not_list)) {
    stop("stack_checks: expected each file to hold a named list of tibbles, but ",
         paste(basename(files[not_list]), collapse = ", "), " did not.")
  }

  if (is.null(elements)) {
    elements <- unique(unlist(lapply(contents, names)))
  }

  if (length(elements) == 0) {
    stop("stack_checks: the check files contain no named elements to stack.")
  }

  out <- lapply(elements, function(key) {
    keep <- vapply(contents, function(x) {
      tbl <- x[[key]]
      !is.null(tbl) && NROW(tbl) > 0
    }, logical(1))
    chunks <- lapply(contents[keep], function(x) x[[key]])
    if (length(chunks) == 0) return(tibble::tibble())
    if (warn_schema) warn_schema_mismatch(chunks, basename(files[keep]), key)
    dplyr::bind_rows(chunks)
  })
  names(out) <- elements

  out
}


#' Warn when the files contributing one element disagree about their columns
#'
#' \code{bind_rows} unions column names and fills the gaps with \code{NA}, which is
#' what makes stacking tolerant of a study that skipped a check. The same tolerance
#' hides a stale file: when one study's results predate a change in what a check
#' returns, its rows arrive missing the new columns and carrying whatever the old
#' version put in the shared ones, and the stacked table silently mixes two
#' schemas. That is exactly the case where a corpus looks fully re-run and is not.
#'
#' @param chunks List of per-file tibbles for one element.
#' @param filenames Base names of the files those chunks came from.
#' @param key The element name, for the message.
#' @return Nothing; called for the warning.
#' @keywords internal
#' @noRd
warn_schema_mismatch <- function(chunks, filenames, key) {
  if (length(chunks) < 2) return(invisible(NULL))
  col_sets <- lapply(chunks, names)
  all_cols <- unique(unlist(col_sets))
  shared <- Reduce(intersect, col_sets)
  missing_any <- setdiff(all_cols, shared)
  if (length(missing_any) == 0) return(invisible(NULL))

  # Name the files that are short of columns the others have, since those are the
  # ones to re-run.
  short <- vapply(col_sets, function(cs) length(setdiff(all_cols, cs)) > 0, logical(1))
  offenders <- filenames[short]
  shown <- utils::head(offenders, 5)

  warning("stack_checks: the files contributing '", key,
          "' do not agree on their columns. Missing from at least one file: ",
          paste(utils::head(missing_any, 8), collapse = ", "),
          if (length(missing_any) > 8) paste0(" (and ", length(missing_any) - 8, " more)") else "",
          ". bind_rows fills those with NA, so the stacked table mixes schemas. ",
          "Usually a stale per-study file written before a check changed what it ",
          "returns: re-run ", paste(shown, collapse = ", "),
          if (length(offenders) > 5) paste0(" (and ", length(offenders) - 5, " more)") else "",
          ".", call. = FALSE)
  invisible(NULL)
}
