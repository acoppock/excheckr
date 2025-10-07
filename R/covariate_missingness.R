#' Summarize and visualize missingness of covariates
#'
#' Computes missingness summaries for a set of covariates and displays a joint
#' missingness heatmap (upper-triangle including diagonal). If no variables are specified,
#' it defaults to all variables starting with `"X_"`, excluding columns ending with `_nona` or `_missing`.
#'
#' @param data A data.frame or tibble containing the covariates.
#' @param ... Covariates to include. If left empty, all variables starting with `"X_"` are used.
#'
#' @return Invisibly returns a list with two elements:
#'   \item{summary}{A tibble with total cases, total missing, and fraction missing per variable.}
#'   \item{heatmap}{A ggplot2 heatmap of upper-triangle joint missingness including diagonal.}
#'
#' @examples
#' dat <- data.frame(
#'   X_pid_3 = c("A", NA, "B", "A", NA, "B", "A", NA, "B", "A"),
#'   X_income = c(100, 200, NA, 400, NA, 300, 500, NA, 600, 700),
#'   X_age = c(25, NA, 30, 40, NA, 35, 45, NA, 50, 55)
#' )
#'
#' # X_pid_3 and X_income have very similar missingness
#' covariate_missingness(dat, X_pid_3, X_income, X_age)
#'
#' # Or default to all "X_" columns
#' covariate_missingness(dat)
#'
#' @export
covariate_missingness <- function(data, ...) {
  # Capture the columns, if any
  vars <- rlang::enquos(...)

  if (length(vars) == 0) {
    varnames <- names(data)
    varnames <- varnames[
      startsWith(varnames, "X_") &
        !grepl("(_nona|_missing)$", varnames)
    ]
  } else {
    varnames <- purrr::map_chr(vars, ~rlang::as_name(.x))
  }

  if (length(varnames) == 0) {
    warning("No variables selected for missingness analysis.")
    return(invisible(NULL))
  }

  n <- nrow(data)

  # Step 1: Missingness summary
  total_missing <- integer(length(varnames))
  fraction_missing <- numeric(length(varnames))

  for (i in seq_along(varnames)) {
    total_missing[i] <- sum(is.na(data[[varnames[i]]]))
    fraction_missing[i] <- mean(is.na(data[[varnames[i]]]))
  }

  missing_summary <- tibble::tibble(
    variable = varnames,
    total_cases = rep(n, length(varnames)),
    total_missing_cases = total_missing,
    fraction_missing_cases = fraction_missing
  )

  print(missing_summary)

  # Step 2: Joint missingness heatmap
  joint_missing <- expand.grid(var1 = varnames, var2 = varnames) |>
    dplyr::mutate(
      var1_f = factor(var1, levels = varnames),
      var2_f = factor(var2, levels = varnames)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      fraction_missing = if (var1 == var2) {
        mean(is.na(data[[var1]]))
      } else {
        mean(is.na(data[[var1]]) & is.na(data[[var2]]))
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(as.integer(var1_f) <= as.integer(var2_f))

  heatmap_plot <- ggplot2::ggplot(joint_missing,
                                  ggplot2::aes(x = var1_f, y = var2_f, fill = fraction_missing)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", fraction_missing)), size = 4) +
    ggplot2::scale_fill_viridis_c(option = "viridis", direction = -1,
                                  limits = c(0, max(joint_missing$fraction_missing))) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL, fill = "Fraction missing") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  print(heatmap_plot)

  invisible(list(summary = missing_summary, heatmap = heatmap_plot))
}
