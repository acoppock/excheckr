#' Write covariate imputation code
#'
#' Generates tidyverse-style `mutate()` code to impute missing values
#' for selected variables. Numeric variables are imputed with the median,
#' while factor or character variables are imputed with the mode (user-defined).
#' In addition to the imputed variable (with suffix `_nona`), a missingness
#' dummy variable (with suffix `_missing`) is created for each input.
#'
#' @param data A data frame or tibble.
#' @param ... Unquoted variable names to generate imputation code for.
#'
#' @details
#' This function prints imputation code to the console that you can copy–paste
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
#' @export
write_covariate_imputation_code <- function(data, ...) {
  # Capture the dataset name as a string
  data_name <- rlang::as_name(rlang::ensym(data))

  # Capture variable names
  vars <- rlang::enquos(...)

  # If no variables explicitly specified, select all X_ columns excluding _nona/_missing
  if (length(vars) == 0) {
    varnames <- names(data)
    varnames <- varnames[startsWith(varnames, "X_") &
                           !grepl("(_nona|_missing)$", varnames)]
  } else {
    varnames <- purrr::map_chr(vars, rlang::as_name)
  }

  if (length(varnames) == 0) {
    warning("No variables selected for imputation.")
    return(invisible(NULL))
  }

  code_lines <- purrr::map_chr(varnames, function(v) {
    col <- data[[v]]
    if (is.numeric(col)) {
      glue::glue(
        "    {v}_nona = replace_na({v}, median({v}, na.rm = TRUE)),\n",
        "    {v}_missing = if_else(is.na({v}), 1, 0)"
      )
    } else {
      glue::glue(
        "    {v}_nona = replace_na({v}, mode({v})),\n",
        "    {v}_missing = if_else(is.na({v}), 1, 0)"
      )
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
#' Generates tidyverse-style `mutate()` code generate missingness
#' dummy variables (with suffix `_missing`) is created for selected variables.
#'
#' @param data A data frame or tibble.
#' @param ... Unquoted variable names to generate missingness dummy variables for
#'
#' @details
#' This function prints mutate code to the console that you can copy–paste
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
#' # Generate imputation code
#' write_covariate_imputation_code(dat, X_factor_variable, X_numeric_variable)
#'
#' @export
write_outcome_missingness_dummies_code <- function(data, ...) {
  # Capture the dataset name as a string
  data_name <- rlang::as_name(rlang::ensym(data))

  # Capture variable names
  vars <- rlang::enquos(...)

  # If no variables explicitly specified, select all X_ columns excluding _nona/_missing
  if (length(vars) == 0) {
    varnames <- names(data)
    varnames <- varnames[startsWith(varnames, "Y_") &
                           !grepl("(_missing)$", varnames)]
  } else {
    varnames <- purrr::map_chr(vars, rlang::as_name)
  }

  if (length(varnames) == 0) {
    warning("No variables selected.")
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


