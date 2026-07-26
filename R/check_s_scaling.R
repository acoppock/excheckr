#' Check that \code{_s} (Glass's delta) variables are correctly standardized
#'
#' For each \code{_s}-suffixed outcome variable, reports the control-group SD
#' (which should equal 1.0 by construction), the treatment-group SD, and the
#' ratio of treatment SD to control SD. A treatment-to-control SD ratio far from
#' 1 indicates that the treatment changed the outcome variance, which is exactly
#' the situation where Glass's delta is preferable to Cohen's d.
#'
#' Auto-selects columns whose names start with \code{D_} or \code{Y_} and end
#' with \code{_s}. Supply \code{outcomes} to override this selection.
#'
#' @param data A data frame, after \code{scale_by_control()} has been applied.
#' @param treatment Character scalar. Name of the treatment column.
#' @param control_value Scalar. Value of \code{treatment} identifying the
#'   control group (default: \code{0}).
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column is appended to the returned tibble.
#' @param outcomes Character vector of \code{_s} column names to check, or
#'   \code{NULL} (default) to auto-select by \code{prefixes} and \code{"_s"}
#'   suffix.
#' @param prefixes Character vector of column-name prefixes used for
#'   auto-selection when \code{outcomes} is \code{NULL}
#'   (default: \code{c("D_", "Y_")}).
#'
#' @return A tibble with columns \code{variable}, \code{control_sd},
#'   \code{treatment_sd}, \code{sd_ratio}, \code{control_sd_ok}, and
#'   optionally \code{study_id}. \code{control_sd_ok} is \code{TRUE} when
#'   \code{control_sd} rounds to 1.000 (within floating-point tolerance),
#'   confirming the standardization is correct.
#'
#' @seealso \code{\link{scale_by_control}}
#'
#' @examples
#' dat <- data.frame(
#'   Z = c(0L, 0L, 0L, 1L, 1L, 1L),
#'   D_belief_01 = c(0.2, 0.4, 0.3, 0.6, 0.8, 0.7),
#'   Y_attitude_01 = c(0.3, 0.5, 0.4, 0.4, 0.6, 0.5)
#' )
#' dat <- scale_by_control(dat, treatment = "Z")
#' check_s_scaling(dat, treatment = "Z")
#'
#' @importFrom stats sd
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @family outcome scaling
#' @export
check_s_scaling <- function(data, treatment, control_value = 0,
                             study_id = NULL, outcomes = NULL,
                             prefixes = c("D_", "Y_")) {
  if (is.null(outcomes)) {
    varnames <- names(data)
    outcomes <- varnames[
      vapply(prefixes, function(p) startsWith(varnames, p), logical(length(varnames))) |>
        apply(1, any) &
        endsWith(varnames, "_s")
    ]
  }

  if (length(outcomes) == 0) {
    warning("No '_s' outcome variables found. Run scale_by_control() first.")
    return(invisible(NULL))
  }

  control_rows   <- data[[treatment]] == control_value
  treatment_rows <- data[[treatment]] != control_value

  result <- tibble::tibble(
    variable     = outcomes,
    control_sd   = vapply(outcomes, function(v)
      stats::sd(data[[v]][control_rows],   na.rm = TRUE), numeric(1)),
    treatment_sd = vapply(outcomes, function(v)
      stats::sd(data[[v]][treatment_rows], na.rm = TRUE), numeric(1))
  ) |>
    dplyr::mutate(
      sd_ratio     = treatment_sd / control_sd,
      control_sd_ok = abs(control_sd - 1) < 1e-10
    )

  if (!is.null(study_id)) result$study_id <- study_id

  result
}
