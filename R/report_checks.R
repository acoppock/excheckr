#' Triage a stacked set of design checks
#'
#' Applies the standard "what needs a human look" filters to the output of
#' \code{\link{stack_checks}} and returns only the rows that failed. Elements
#' absent from \code{checks} are skipped, so the same call works whether the
#' pipeline ran \code{\link{check_attrition}}, \code{\link{check_attrition_lasso}},
#' or neither.
#'
#' The filters are:
#' \describe{
#'   \item{out_of_bounds}{\code{ybounds} rows where \code{in_bounds} is
#'     \code{FALSE}: an outcome outside the expected [0, 1] range.}
#'   \item{missing_no_nona}{\code{missingness} rows with missing values and no
#'     imputed companion column: covariates that will silently drop rows from
#'     every adjusted model.}
#'   \item{nona_still_missing}{\code{missingness} rows whose imputed companion
#'     column still contains \code{NA}: the imputation did not take.}
#'   \item{balance_joint}{\code{balance_joint} rows with \code{p_value} at or
#'     below \code{alpha}.}
#'   \item{balance_covariate}{\code{balance_covariate} rows with \code{p_value}
#'     at or below \code{alpha}. Expect roughly \code{alpha} of these to fail by
#'     chance; use \code{\link{summarize_check_pvalues}} to judge whether there
#'     are more than chance would give.}
#'   \item{attrition}{\code{attrition} or \code{attrition_lasso} rows flagged at
#'     \code{alpha}.}
#' }
#'
#' @param checks A named list of stacked check tibbles, as returned by
#'   \code{\link{stack_checks}}.
#' @param alpha Rejection threshold for the balance and attrition filters
#'   (default \code{0.05}).
#'
#' @return An object of class \code{"excheckr_report"}: a named list of
#'   tibbles holding only the failing rows, with an \code{alpha} attribute.
#'   Elements with nothing to report are present but empty. Has a
#'   \code{print} method that shows the counts.
#'
#' @examples
#' checks <- list(
#'   ybounds = data.frame(study_id = c("a", "b"), variable = "Y_x",
#'                        min = c(0, -1), max = c(1, 3),
#'                        in_bounds = c(TRUE, FALSE)),
#'   balance_joint = data.frame(study_id = c("a", "b"), p_value = c(0.4, 0.01))
#' )
#' report_checks(checks)
#'
#' @importFrom dplyr filter arrange desc
#' @export
report_checks <- function(checks, alpha = 0.05) {
  if (!is.list(checks) || is.data.frame(checks)) {
    stop("report_checks: expected a named list of check tibbles, got ",
         class(checks)[1], ".")
  }

  out <- list()

  ybounds <- pluck_check(checks, "ybounds")
  if (!is.null(ybounds) && "in_bounds" %in% names(ybounds)) {
    out$out_of_bounds <- ybounds[!is.na(ybounds$in_bounds) & !ybounds$in_bounds, , drop = FALSE]
  }

  missingness <- pluck_check(checks, "missingness")
  if (!is.null(missingness) && all(c("n_missing", "has_nona_version") %in% names(missingness))) {
    out$missing_no_nona <- missingness[
      missingness$n_missing > 0 & !missingness$has_nona_version, , drop = FALSE
    ]
    if ("nona_has_na" %in% names(missingness)) {
      out$nona_still_missing <- missingness[
        missingness$has_nona_version & !is.na(missingness$nona_has_na) &
          missingness$nona_has_na, , drop = FALSE
      ]
    }
  }

  for (nm in c("balance_joint", "balance_covariate")) {
    tbl <- pluck_check(checks, nm)
    if (!is.null(tbl) && "p_value" %in% names(tbl)) {
      out[[nm]] <- tbl[!is.na(tbl$p_value) & tbl$p_value <= alpha, , drop = FALSE]
    }
  }

  attrition <- pluck_check(checks, "attrition")
  if (is.null(attrition)) attrition <- pluck_check(checks, "attrition_lasso")
  if (!is.null(attrition)) {
    # Only treat `flag` as the check result when it is logical. The name is not
    # reserved: a project may carry an unrelated character column called `flag`
    # (a party or actor label, say), and reading that as a failure indicator
    # would either error or silently report the wrong rows.
    if ("flag" %in% names(attrition) && is.logical(attrition$flag)) {
      # check_attrition_lasso already computed the flag at its own threshold
      out$attrition <- attrition[!is.na(attrition$flag) & attrition$flag, , drop = FALSE]
    } else if ("p_value" %in% names(attrition)) {
      out$attrition <- attrition[
        !is.na(attrition$p_value) & attrition$p_value <= alpha, , drop = FALSE
      ]
    }
  }

  structure(out, class = "excheckr_report", alpha = alpha)
}


#' @export
print.excheckr_report <- function(x, ...) {
  alpha <- attr(x, "alpha")
  cat("excheckr triage report (alpha = ", alpha, ")\n", sep = "")
  if (length(x) == 0) {
    cat("  No recognized check elements found.\n")
    return(invisible(x))
  }
  labels <- c(
    out_of_bounds      = "outcomes outside [0, 1]",
    missing_no_nona    = "covariates missing with no imputed companion",
    nona_still_missing = "imputed companions that still contain NA",
    balance_joint      = "joint balance tests flagged",
    balance_covariate  = "covariate balance tests flagged",
    attrition          = "attrition tests flagged"
  )
  for (nm in names(x)) {
    n <- NROW(x[[nm]])
    label <- if (nm %in% names(labels)) labels[[nm]] else nm
    cat("  ", format(n, width = 5), "  ", label, "\n", sep = "")
  }
  clean <- all(vapply(x, function(e) NROW(e) == 0L, logical(1)))
  if (clean) cat("  All checks clean.\n")
  cat("Inspect an element with report$<name>.\n")
  invisible(x)
}


#' Pull a check element by name, tolerating absence and empty tibbles
#'
#' @param checks A named list.
#' @param nm Element name.
#' @return The element, or NULL when absent or empty.
#' @keywords internal
#' @noRd
pluck_check <- function(checks, nm) {
  if (!nm %in% names(checks)) return(NULL)
  tbl <- checks[[nm]]
  if (is.null(tbl) || NROW(tbl) == 0) return(NULL)
  tbl
}
