#' Summarize and visualize missingness of covariates
#'
#' Computes missingness summaries for a set of covariates and displays a joint
#' missingness heatmap (upper-triangle including the diagonal).
#'
#' If no variables are specified, it defaults to all variables starting with `"X_"`,
#' excluding columns ending with `_nona` or `_missing`.
#'
#' This function supports both unquoted column names and tidyselect helpers
#' such as [dplyr::all_of()], [tidyselect::starts_with()], etc.
#'
#' @param data A data frame or tibble containing the covariates.
#' @param ... Columns to include in the analysis. You can specify them
#'   unquoted (e.g., `age`, `income`) or using selection helpers such as
#'   [dplyr::all_of()] or [tidyselect::starts_with()].
#'   If left empty, all `"X_"` columns are used.
#'
#' @return Invisibly returns a list with two elements:
#' \describe{
#'   \item{summary}{A tibble summarizing total cases, total missing, and fraction missing per variable.}
#'   \item{heatmap}{A [ggplot2::ggplot()] object showing the upper-triangle joint missingness (including diagonal).}
#' }
#'
#' @examples
#' dat <- data.frame(
#'   X_pid_3 = c("A", NA, "B", "A", NA, "B", "A", NA, "B", "A"),
#'   X_income = c(100, 200, NA, 400, NA, 300, 500, NA, 600, 700),
#'   X_age = c(25, NA, 30, 40, NA, 35, 45, NA, 50, 55)
#' )
#'
#' # X_pid_3 and X_income have similar missingness
#' check_covariate_missingness(dat, X_pid_3, X_income, X_age)
#'
#' # Or default to all "X_" columns
#' check_covariate_missingness(dat)
#'
#' # Or use tidyselect helpers
#' vars <- c("X_pid_3", "X_income", "X_age")
#' check_covariate_missingness(dat, dplyr::all_of(vars))
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @importFrom dplyr mutate rowwise ungroup filter
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_viridis_c coord_fixed labs theme_minimal theme element_text
#' @family per-study checks
#' @export
check_covariate_missingness <- function(data, ...) {
  # Evaluate column selection; supports unquoted names, all_of(), or nothing
  sel <- eval_select(expr(c(...)), data)
  varnames <- names(sel)

  # If no columns explicitly provided, fall back to X_* variables
  if (length(varnames) == 0) {
    varnames <- auto_select_vars(data, prefix = "X_")
  }

  if (length(varnames) == 0) {
    warning("No variables selected for missingness analysis.")
    return(invisible(NULL))
  }

  n <- nrow(data)

  # Step 1: Missingness summary
  total_missing <- vapply(varnames, function(v) sum(is.na(data[[v]])), integer(1))
  fraction_missing <- vapply(varnames, function(v) mean(is.na(data[[v]])), numeric(1))

  missing_summary <- tibble(
    variable = varnames,
    total_cases = n,
    total_missing_cases = total_missing,
    fraction_missing_cases = fraction_missing
  )

  print(missing_summary)

  # Step 2: Joint missingness heatmap
  joint_missing <- expand.grid(var1 = varnames, var2 = varnames) |>
    mutate(
      var1_f = factor(var1, levels = varnames),
      var2_f = factor(var2, levels = varnames)
    ) |>
    rowwise() |>
    mutate(
      fraction_missing = if (var1 == var2) {
        mean(is.na(data[[var1]]))
      } else {
        mean(is.na(data[[var1]]) & is.na(data[[var2]]))
      }
    ) |>
    ungroup() |>
    filter(as.integer(var1_f) <= as.integer(var2_f))

  heatmap_plot <- ggplot(
    joint_missing,
    aes(x = var1_f, y = var2_f, fill = fraction_missing)
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = sprintf("%.2f", fraction_missing)),
      size = 4
    ) +
    scale_fill_viridis_c(
      option = "viridis",
      direction = -1,
      limits = c(0, max(joint_missing$fraction_missing))
    ) +
    coord_fixed() +
    labs(
      x = NULL,
      y = NULL,
      fill = "Fraction missing"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(heatmap_plot)

  invisible(list(summary = missing_summary, heatmap = heatmap_plot))
}