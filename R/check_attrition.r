#' Check differential attrition across treatment conditions
#'
#' Performs attrition checks by regressing outcome missingness indicators
#' on treatment assignment to test for differential attrition.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param outcomes Character vector of outcome variable names, or unquoted column names
#'   using tidyselect helpers. If left empty, all `"Y_"` columns are used.
#' @param .method Regression function to use (default: `estimatr::lm_robust`).
#'   Must accept formula and data arguments.
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return A tibble with one row per outcome, containing the regression results
#'   from regressing each outcome's missingness indicator on treatment.
#'
#' @details
#' For each outcome variable, creates a missingness indicator (1 if missing, 0 otherwise)
#' and regresses it on treatment assignment. Significant coefficients indicate
#' differential attrition across treatment conditions.
#'
#' If missingness indicator variables already exist (with `_missing` suffix),
#' those are used. Otherwise, they are created on the fly.
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#'
#' dat <- data.frame(
#'   Z = rbinom(200, 1, 0.5),
#'   Y_attitude = c(rnorm(150), rep(NA, 50)),
#'   Y_behavior = c(rnorm(180), rep(NA, 20))
#' )
#'
#' # Check differential attrition
#' check_attrition(dat, Z)
#'
#' # Specific outcomes
#' check_attrition(dat, Z, c("Y_attitude", "Y_behavior"))
#'
#' # With clustered standard errors
#' dat$cluster_id <- sample(1:10, 200, replace = TRUE)
#' check_attrition(dat, Z, clusters = cluster_id)
#' }
#'
#' @importFrom dplyr bind_rows select
#' @importFrom broom tidy
#' @importFrom rlang ensym as_name expr
#' @importFrom tidyselect eval_select
#' @export
check_attrition <- function(data, treatment, outcomes = NULL, .method = estimatr::lm_robust, ...) {
  # Capture treatment variable name
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle outcome selection
  if (is.null(substitute(outcomes))) {
    # No outcomes specified - use Y_* columns
    varnames <- auto_select_vars(data, prefix = "Y_")
  } else if (is.character(outcomes)) {
    # Character vector provided
    varnames <- outcomes
  } else {
    # Tidyselect expression
    sel <- eval_select(rlang::expr(outcomes), data)
    varnames <- names(sel)
  }

  if (length(varnames) == 0) {
    warning("No outcomes selected for attrition check.")
    return(invisible(NULL))
  }

  # Run regression for each outcome's missingness indicator
  results <- lapply(varnames, function(v) {
    # Check if missingness indicator already exists
    missing_var <- paste0(v, "_missing")

    if (missing_var %in% names(data)) {
      # Use existing missingness indicator
      miss_col <- missing_var
    } else {
      # Create missingness indicator on the fly
      miss_col <- paste0(v, "_missing_temp")
      data[[miss_col]] <- as.integer(is.na(data[[v]]))
    }

    # Regress missingness ~ treatment
    form <- as.formula(paste(miss_col, "~", treatment_name))
    fit <- .method(form, data = data, ...)
    tidy_fit <- broom::tidy(fit)
    tidy_fit <- tidy_fit[tidy_fit$term == treatment_name, ]
    tidy_fit$outcome <- v
    return(tidy_fit)
  })

  result_df <- dplyr::bind_rows(results)

  # Reorder columns for clarity
  result_df <- dplyr::select(
    result_df,
    outcome,
    dplyr::everything()
  )

  print(result_df)
  invisible(result_df)
}


#' Write attrition check code
#'
#' Generates code to perform attrition checks by regressing outcome missingness
#' indicators on treatment assignment.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param outcomes Character vector of outcome variable names, or unquoted column names
#'   using tidyselect helpers. If left empty, all `"Y_"` columns are used.
#' @param .method Regression function to use (default: `estimatr::lm_robust`).
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return Invisibly returns the generated code as a single string.
#'
#' @details
#' This function prints R code to the console that you can copy-paste
#' into your analysis script. It does not perform the attrition check itself.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   Z = rbinom(200, 1, 0.5),
#'   Y_attitude = c(rnorm(150), rep(NA, 50)),
#'   Y_behavior = c(rnorm(180), rep(NA, 20))
#' )
#'
#' write_attrition_check_code(dat, Z)
#' }
#'
#' @importFrom rlang ensym as_name
#' @importFrom tidyselect eval_select
#' @importFrom glue glue
#' @export
write_attrition_check_code <- function(data, treatment, outcomes = NULL, .method = estimatr::lm_robust, ...) {
  # Capture dataset and treatment names
  data_name <- rlang::as_name(rlang::ensym(data))
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle outcome selection
  if (is.null(substitute(outcomes))) {
    varnames <- auto_select_vars(data, prefix = "Y_")
  } else if (is.character(outcomes)) {
    varnames <- outcomes
  } else {
    sel <- eval_select(rlang::expr(outcomes), data)
    varnames <- names(sel)
  }

  if (length(varnames) == 0) {
    warning("No outcomes selected for attrition check.")
    return(invisible(NULL))
  }

  # Get additional arguments
  dots <- list(...)
  extra_args <- if (length(dots) > 0) {
    paste0(", ", paste(names(dots), "=", sapply(dots, deparse), collapse = ", "))
  } else {
    ""
  }

  method_name <- deparse(substitute(.method))

  # Generate code for each outcome
  code_lines <- vapply(varnames, function(v) {
    missing_var <- paste0(v, "_missing")

    if (missing_var %in% names(data)) {
      # Use existing missingness indicator
      glue::glue(
        "# Attrition check for {v}\n",
        "{method_name}({missing_var} ~ {treatment_name}, data = {data_name}{extra_args})"
      )
    } else {
      # Need to create missingness indicator first
      glue::glue(
        "# Attrition check for {v}\n",
        "{data_name}${missing_var} <- as.integer(is.na({data_name}${v}))\n",
        "{method_name}({missing_var} ~ {treatment_name}, data = {data_name}{extra_args})"
      )
    }
  }, character(1))

  code <- paste(code_lines, collapse = "\n\n")

  cat(code, "\n")
  invisible(code)
}