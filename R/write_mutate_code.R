#' Write covariate imputation code
#'
#' Generates tidyverse-style `mutate()` code to impute missing values
#' for selected variables. Numeric variables are imputed with the median,
#' while factor or character variables are imputed with the mode (user-defined).
#' In addition to the imputed variable (with suffix `_nona`), a missingness
#' dummy variable (with suffix `_missing`) is created for each input.
#'
#' @param data A data frame or tibble.
#' @param ... Columns to generate imputation code for. You can specify them
#'   unquoted (e.g., `age`, `income`) or using selection helpers such as
#'   [dplyr::all_of()] or [tidyselect::starts_with()].
#'   If left empty, all `"X_"` columns are used.
#' @param include_missingness_dummies Logical. Should missingness dummy variables
#'   be included in the generated code? Defaults to TRUE.
#'
#' @details
#' This function prints imputation code to the console that you can copy-paste
#' into your analysis script. It does not perform the imputation itself.
#'
#' @return Invisibly returns the generated code as a single string.
#'
#' @examples
#' # Example data with missingness
#' dat <- data.frame(
#'   X_factor_variable = factor(rep(c("A", "B", NA), c(4, 5, 1))),
#'   X_numeric_variable = c(100, 200, NA, 400, 500, NA, 700, 800, 900, NA)
#' )
#'
#' # Generate imputation code
#' write_covariate_imputation_code(dat, X_factor_variable, X_numeric_variable)
#'
#' # Or default to all "X_" columns
#' write_covariate_imputation_code(dat)
#'
#' # Or use tidyselect helpers
#' vars <- c("X_factor_variable", "X_numeric_variable")
#' write_covariate_imputation_code(dat, dplyr::all_of(vars))
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr as_name ensym
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @family code generators
#' @export
write_covariate_imputation_code <- function(data, ..., include_missingness_dummies = TRUE) {
  # Capture the dataset name as a string
  data_name <- rlang::as_name(rlang::ensym(data))

  # Evaluate column selection; supports unquoted names, all_of(), or nothing
  sel <- eval_select(expr(c(...)), data)
  varnames <- names(sel)

  # If no columns explicitly provided, fall back to X_* variables
  if (length(varnames) == 0) {
    varnames <- auto_select_vars(data, prefix = "X_")
  }

  if (length(varnames) == 0) {
    warning("No variables selected for imputation.")
    return(invisible(NULL))
  }

  code_lines <- purrr::map_chr(varnames, function(v) {
    col <- data[[v]]
    if (include_missingness_dummies) {
      if (is.numeric(col)) {
        glue::glue(
          "    {v}_nona = replace_na({v}, median({v}, na.rm = TRUE)),\n",
          "    {v}_missing = if_else(is.na({v}), 1, 0)"
        )
      } else {
        glue::glue(
          "    {v}_nona = replace_na({v}, stat_mode({v})),\n",
          "    {v}_missing = if_else(is.na({v}), 1, 0)"
        )
      }
    } else {
      if (is.numeric(col)) {
        glue::glue("    {v}_nona = replace_na({v}, median({v}, na.rm = TRUE))")
      } else {
        glue::glue("    {v}_nona = replace_na({v}, stat_mode({v}))")
      }
    }
  })

  code <- paste(
    sprintf("%s <-", data_name),
    sprintf("  %s |>", data_name),
    "  mutate(",
    paste(code_lines, collapse = ",\n"),
    "  )",
    sep = "\n"
  )

  cat(code, "\n")
  invisible(code)
}


#' Write outcome missingness code
#'
#' Generates tidyverse-style `mutate()` code to generate missingness
#' dummy variables (with suffix `_missing`) for selected variables.
#'
#' @param data A data frame or tibble.
#' @param ... Columns to generate missingness dummy variables for. You can specify them
#'   unquoted (e.g., `age`, `income`) or using selection helpers such as
#'   [dplyr::all_of()] or [tidyselect::starts_with()].
#'   If left empty, all `"Y_"` columns are used.
#'
#' @details
#' This function prints mutate code to the console that you can copy-paste
#' into your cleaning script. It does not create the variables itself.
#'
#' @return Invisibly returns the generated code as a single string.
#'
#' @examples
#' # Example data with missingness
#' dat <- data.frame(
#'    Y_attitude = rep(c(1, 2, 3, 4, 5, NA), c(10, 20, 30, 40, 50, 50)),
#'    Y_behavior = rep(c(0, 1, NA), c(100, 50, 50))
#' )
#'
#' # Generate missingness dummy code
#' write_outcome_missingness_dummies_code(dat, Y_attitude, Y_behavior)
#'
#' # Or default to all "Y_" columns
#' write_outcome_missingness_dummies_code(dat)
#'
#' # Or use tidyselect helpers
#' vars <- c("Y_attitude", "Y_behavior")
#' write_outcome_missingness_dummies_code(dat, dplyr::all_of(vars))
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr as_name ensym
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @family code generators
#' @export
write_outcome_missingness_dummies_code <- function(data, ...) {
  # Capture the dataset name as a string
  data_name <- rlang::as_name(rlang::ensym(data))

  # Evaluate column selection; supports unquoted names, all_of(), or nothing
  sel <- eval_select(expr(c(...)), data)
  varnames <- names(sel)

  # If no columns explicitly provided, fall back to Y_* variables
  if (length(varnames) == 0) {
    varnames <- auto_select_vars(data, prefix = "Y_")
  }

  if (length(varnames) == 0) {
    warning("No variables selected for missingness analysis.")
    return(invisible(NULL))
  }

  code_lines <- purrr::map_chr(varnames, function(v) {
    glue::glue("    {v}_missing = if_else(is.na({v}), 1, 0)")
  })

  code <- paste(
    sprintf("%s <-", data_name),
    sprintf("  %s |>", data_name),
    "  mutate(",
    paste(code_lines, collapse = ",\n"),
    "  )",
    sep = "\n"
  )

  cat(code, "\n")
  invisible(code)
}