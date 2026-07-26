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
#'
#' @return A named list of tibbles, one per element name.
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
                         elements = NULL) {
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
    chunks <- lapply(contents, function(x) x[[key]])
    chunks <- Filter(function(x) !is.null(x) && NROW(x) > 0, chunks)
    if (length(chunks) == 0) return(tibble::tibble())
    dplyr::bind_rows(chunks)
  })
  names(out) <- elements

  out
}
