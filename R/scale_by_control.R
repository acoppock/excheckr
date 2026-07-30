#' Scale outcome variables by control-group standard deviation (Glass's delta)
#'
#' For each outcome variable, divides by its standard deviation in the control
#' group to produce a standardized version. The standardized variable is added
#' to the data frame with a \code{_s} suffix.
#'
#' Auto-selects columns whose names start with any prefix in \code{prefixes}
#' (default: \code{c("D_", "Y_")}), excluding columns ending with
#' \code{"_missing"} or \code{"_s"}. Supply \code{outcomes} to override this
#' selection.
#'
#' The resulting \code{_s} variables are on Glass's delta scale: a one-unit
#' difference equals one control-group SD. Using the control-group SD as the
#' standardizer is Glass's delta (Glass, 1976), which is preferred when the
#' treatment may change the variance of the outcome. Dividing by the pooled SD
#' (Cohen's d) would conflate effect-size estimation with variance changes
#' induced by the treatment. The \code{_s} variables are intentionally excluded
#' from \code{check_y_bounds()} (which skips \code{"_s"} columns), since
#' standardized values are not bounded to [0, 1]. Use \code{check_s_scaling()}
#' to verify the standardization and inspect treatment-arm variance.
#'
#' @references
#' Glass, G. V. (1976). Primary, secondary, and meta-analysis of research.
#' \emph{Educational Researcher}, \emph{5}(10), 11--17.
#' \doi{10.3102/0013189X005010003}
#'
#' @param data A data frame.
#' @param treatment Character scalar. Name of the treatment column.
#' @param control_value Scalar. Value of \code{treatment} identifying the
#'   control group (default: \code{0}).
#' @param outcomes Character vector of column names to standardize, or
#'   \code{NULL} (default) to auto-select by \code{prefixes}.
#' @param prefixes Character vector of column-name prefixes used for
#'   auto-selection when \code{outcomes} is \code{NULL}
#'   (default: \code{c("D_", "Y_")}).
#' @param strip_suffix Suffix removed from a column's name before \code{"_s"} is
#'   appended, or \code{NULL} to append to the name unchanged. The default
#'   \code{"_01"} suits a convention where a rescaled-to-unit-interval outcome
#'   carries that marker, so \code{D_belief_01} becomes \code{D_belief_s} rather
#'   than \code{D_belief_01_s}: the value is no longer on the unit interval once
#'   divided by an SD, so keeping the marker would be a lie. Pass \code{NULL} if
#'   your names carry no such marker.
#'
#' @return The input data frame with the standardized columns appended. Each is
#'   named for its source with \code{strip_suffix} removed and \code{"_s"} added,
#'   so \code{Y_turnout} yields \code{Y_turnout_s} and \code{Y_turnout_01} yields
#'   \code{Y_turnout_s}.
#'
#' @examples
#' dat <- data.frame(
#'   Z = c(0L, 0L, 0L, 1L, 1L, 1L),
#'   D_belief_01 = c(0.2, 0.4, 0.3, 0.6, 0.8, 0.7),
#'   Y_attitude_01 = c(0.3, 0.5, 0.4, 0.4, 0.6, 0.5)
#' )
#'
#' # _01 is stripped, so the new columns are D_belief_s and Y_attitude_s
#' names(scale_by_control(dat, treatment = "Z"))
#'
#' # Keep the full source name instead
#' names(scale_by_control(dat, treatment = "Z", strip_suffix = NULL))
#'
#' @importFrom stats sd
#' @family outcome scaling
#' @export
scale_by_control <- function(data, treatment, control_value = 0,
                              outcomes = NULL, prefixes = c("D_", "Y_"),
                              strip_suffix = "_01") {
  if (is.null(outcomes)) {
    outcomes <- unlist(lapply(prefixes, function(p) {
      auto_select_vars(data, prefix = p, exclude_suffixes = c("_missing", "_s"))
    }))
  }

  control_rows <- data[[treatment]] == control_value
  if (!any(control_rows, na.rm = TRUE)) {
    stop("scale_by_control: no rows have ", treatment, " == ", control_value,
         ". Pass control_value to name the control arm; every '_s' column would ",
         "otherwise be silently NA.", call. = FALSE)
  }

  for (v in outcomes) {
    ctrl_sd <- stats::sd(data[[v]][control_rows], na.rm = TRUE)
    if (is.na(ctrl_sd) || ctrl_sd == 0) {
      warning(paste0(
        "scale_by_control: control SD is ",
        if (is.na(ctrl_sd)) "NA" else "zero",
        " for '", v, "'. Skipping '_s' version."
      ))
      next
    }
    stem <- if (is.null(strip_suffix)) v else sub(paste0(strip_suffix, "$"), "", v)
    data[[paste0(stem, "_s")]] <- data[[v]] / ctrl_sd
  }

  data
}
