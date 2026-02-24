#' Check covariate balance across treatment conditions
#'
#' Performs balance checks by regressing each covariate on treatment assignment.
#' Handles categorical covariates by creating dummy variables for each level.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param covariates Character vector of covariate names, or unquoted column names
#'   using tidyselect helpers. If left empty, all `"X_"` columns are used.
#' @param .method Regression function to use (default: `estimatr::lm_robust`).
#'   Must accept formula and data arguments.
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return A tibble with one row per covariate (or covariate level for factors),
#'   containing the regression results from regressing each covariate on treatment.
#'
#' @details
#' For numeric covariates, regresses the covariate on treatment directly.
#' For factor/character covariates, creates dummy variables for each level
#' (excluding the first level as reference) and regresses each dummy on treatment.
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#'
#' dat <- data.frame(
#'   Z = rbinom(100, 1, 0.5),
#'   X_age = rnorm(100, 50, 10),
#'   X_gender = sample(c("M", "F"), 100, replace = TRUE),
#'   X_party = factor(sample(c("D", "R", "I"), 100, replace = TRUE))
#' )
#'
#' # Default: all X_ covariates with lm_robust
#' check_balance(dat, Z)
#'
#' # Specific covariates
#' check_balance(dat, Z, c("X_age", "X_gender"))
#'
#' # With clustered standard errors
#' dat$cluster_id <- sample(1:10, 100, replace = TRUE)
#' check_balance(dat, Z, clusters = cluster_id)
#' }
#'
#' @importFrom dplyr bind_rows mutate select
#' @importFrom broom tidy
#' @importFrom rlang ensym as_name expr
#' @importFrom tidyselect eval_select
#' @export
check_balance <- function(data, treatment, covariates = NULL, .method = estimatr::lm_robust, ...) {
  # Capture treatment variable name
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle covariate selection
  if (is.null(substitute(covariates))) {
    # No covariates specified - use X_* columns
    varnames <- auto_select_vars(data, prefix = "X_")
  } else if (is.character(covariates)) {
    # Character vector provided
    varnames <- covariates
  } else {
    # Tidyselect expression
    sel <- eval_select(rlang::expr(covariates), data)
    varnames <- names(sel)
  }

  if (length(varnames) == 0) {
    warning("No covariates selected for balance check.")
    return(invisible(NULL))
  }

  # Run regression for each covariate
  results <- lapply(varnames, function(v) {
    col <- data[[v]]

    if (is.numeric(col)) {
      # Numeric covariate: regress covariate ~ treatment
      form <- as.formula(paste(v, "~", treatment_name))
      fit <- .method(form, data = data, ...)
      tidy_fit <- broom::tidy(fit)
      mm <- model.matrix(form, data = data)
      treatment_idx <- which(attr(terms(form), "term.labels") == treatment_name)
      treatment_terms <- colnames(mm)[attr(mm, "assign") == treatment_idx]
      tidy_fit <- tidy_fit[tidy_fit$term %in% treatment_terms, ]
      tidy_fit$covariate <- v
      tidy_fit$level <- NA_character_
      return(tidy_fit)

    } else {
      # Factor/character covariate: create dummies for each level
      if (!is.factor(col)) {
        col <- as.factor(col)
      }

      levels_vec <- levels(col)

      # Create dummy for each level (excluding reference)
      dummy_results <- lapply(levels_vec[-1], function(lev) {
        # Clean level name: replace spaces and special chars with underscores
        clean_lev <- gsub("[^[:alnum:]_]", "_", lev)
        dummy_name <- paste0(v, "_", clean_lev)
        data[[dummy_name]] <- as.integer(col == lev)

        form <- as.formula(paste(dummy_name, "~", treatment_name))
        fit <- .method(form, data = data, ...)
        tidy_fit <- broom::tidy(fit)
        mm <- model.matrix(form, data = data)
        treatment_idx <- which(attr(terms(form), "term.labels") == treatment_name)
        treatment_terms <- colnames(mm)[attr(mm, "assign") == treatment_idx]
        tidy_fit <- tidy_fit[tidy_fit$term %in% treatment_terms, ]
        tidy_fit$covariate <- v
        tidy_fit$level <- lev
        return(tidy_fit)
      })

      dplyr::bind_rows(dummy_results)
    }
  })

  result_df <- dplyr::bind_rows(results)

  # Reorder columns for clarity
  result_df <- dplyr::select(
    result_df,
    covariate,
    level,
    dplyr::everything()
  )

  print(result_df)
  invisible(result_df)
}


#' Write balance check code
#'
#' Generates code to perform balance checks by regressing each covariate
#' on treatment assignment.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param covariates Character vector of covariate names, or unquoted column names
#'   using tidyselect helpers. If left empty, all `"X_"` columns are used.
#' @param .method Regression function to use (default: `estimatr::lm_robust`).
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return Invisibly returns the generated code as a single string.
#'
#' @details
#' This function prints R code to the console that you can copy-paste
#' into your analysis script. It does not perform the balance check itself.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   Z = rbinom(100, 1, 0.5),
#'   X_age = rnorm(100, 50, 10),
#'   X_gender = sample(c("M", "F"), 100, replace = TRUE)
#' )
#'
#' write_balance_check_code(dat, Z)
#' }
#'
#' @importFrom rlang ensym as_name
#' @importFrom tidyselect eval_select
#' @importFrom glue glue
#' @export
write_balance_check_code <- function(data, treatment, covariates = NULL, .method = estimatr::lm_robust, ...) {
  # Capture dataset and treatment names
  data_name <- rlang::as_name(rlang::ensym(data))
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle covariate selection
  if (is.null(substitute(covariates))) {
    varnames <- auto_select_vars(data, prefix = "X_")
  } else if (is.character(covariates)) {
    varnames <- covariates
  } else {
    sel <- eval_select(rlang::expr(covariates), data)
    varnames <- names(sel)
  }

  if (length(varnames) == 0) {
    warning("No covariates selected for balance check.")
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

  # Generate code for each covariate
  code_lines <- vapply(varnames, function(v) {
    col <- data[[v]]

    if (is.numeric(col)) {
      glue::glue(
        "# Balance check for {v}\n",
        "{method_name}({v} ~ {treatment_name}, data = {data_name}{extra_args})"
      )
    } else {
      if (!is.factor(col)) col <- as.factor(col)
      levels_vec <- levels(col)

      dummy_lines <- vapply(levels_vec[-1], function(lev) {
        # Clean level name: replace spaces and special chars with underscores
        clean_lev <- gsub("[^[:alnum:]_]", "_", lev)
        dummy_name <- paste0(v, "_", clean_lev)
        glue::glue(
          "# Balance check for {v} (level: {lev})\n",
          "{data_name}${dummy_name} <- as.integer({data_name}${v} == '{lev}')\n",
          "{method_name}({dummy_name} ~ {treatment_name}, data = {data_name}{extra_args})"
        )
      }, character(1))

      paste(dummy_lines, collapse = "\n\n")
    }
  }, character(1))

  code <- paste(code_lines, collapse = "\n\n")

  cat(code, "\n")
  invisible(code)
}