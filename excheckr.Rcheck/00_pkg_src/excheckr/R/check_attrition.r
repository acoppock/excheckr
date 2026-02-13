#' Check differential attrition across treatment conditions
#'
#' Performs attrition checks by regressing outcome missingness indicators
#' on treatment assignment to test for differential attrition.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param outcomes Character vector of outcome variable names, or unquoted column names
#'   using tidyselect helpers. If left empty, all `"Y_"` columns are used.
#' @param covariates Character vector of covariate names, or unquoted column names
#'   using tidyselect helpers. If provided, fits a Lin (2013) model interacting
#'   treatment with demeaned covariates and performs an F-test comparing the full
#'   model (missingness ~ treatment * covariates) to the restricted model
#'   (missingness ~ covariates).
#' @param .method Regression function to use (default: `estimatr::lm_robust`).
#'   Must accept formula and data arguments.
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return When \code{covariates} is \code{NULL}, a tibble with one row per outcome
#'   containing the treatment coefficient from regressing missingness on treatment.
#'   When \code{covariates} is provided, a list with two elements:
#'   \describe{
#'     \item{coefficients}{A tibble of all coefficient estimates from the Lin model
#'       (treatment, demeaned covariates, and their interactions).}
#'     \item{f_test}{A tibble with one row per outcome containing the Wald F-test
#'       of joint significance of treatment and treatment-by-covariate interactions.}
#'   }
#'
#' @details
#' For each outcome variable, creates a missingness indicator (1 if missing, 0 otherwise)
#' and regresses it on treatment assignment. Significant coefficients indicate
#' differential attrition across treatment conditions.
#'
#' If missingness indicator variables already exist (with `_missing` suffix),
#' those are used. Otherwise, they are created on the fly.
#'
#' When covariates are provided, the function follows the Lin (2013) estimator
#' approach used in \code{estimatr::lm_lin}: covariates are demeaned by subtracting
#' the full-sample mean, then the full model
#' \code{missingness ~ treatment * (demeaned covariates)} is fit. A Wald F-test
#' compares this to the restricted model \code{missingness ~ demeaned covariates},
#' testing whether treatment and its interactions with covariates jointly predict
#' attrition.
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#'
#' dat <- data.frame(
#'   Z = rbinom(200, 1, 0.5),
#'   X_age = rnorm(200, 50, 10),
#'   X_income = rnorm(200, 50000, 10000),
#'   Y_attitude = c(rnorm(150), rep(NA, 50)),
#'   Y_behavior = c(rnorm(180), rep(NA, 20))
#' )
#'
#' # Simple attrition check (no covariates)
#' check_attrition(dat, Z)
#'
#' # With covariates: Lin model + F-test for differential attrition
#' check_attrition(dat, Z, covariates = c("X_age", "X_income"))
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
#' @importFrom tibble tibble
#' @importFrom stats coef vcov df.residual pf
#' @export
check_attrition <- function(data, treatment, outcomes = NULL, covariates = NULL,
                            .method = estimatr::lm_robust, ...) {
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

  # Handle covariate selection
  covar_expr <- substitute(covariates)
  has_covariates <- !is.null(covar_expr)

  if (has_covariates) {
    if (is.character(covariates)) {
      covar_names <- covariates
    } else {
      sel <- eval_select(rlang::expr(covariates), data)
      covar_names <- names(sel)
    }

    if (length(covar_names) == 0) {
      warning("No covariates selected. Falling back to simple attrition check.")
      has_covariates <- FALSE
    }
  }

  if (has_covariates) {
    # --- Lin (2013) model with F-test ---

    # Demean covariates (replicating lm_lin)
    demeaned <- demean_covariates(data, covar_names)
    demeaned_names <- colnames(demeaned)
    analysis_data <- cbind(data, demeaned)

    results_coef <- list()
    results_ftest <- list()

    for (v in varnames) {
      # Get or create missingness indicator
      missing_var <- paste0(v, "_missing")
      if (missing_var %in% names(analysis_data)) {
        miss_col <- missing_var
      } else {
        miss_col <- paste0(v, "_missing_temp")
        analysis_data[[miss_col]] <- as.integer(is.na(analysis_data[[v]]))
      }

      # Full model: missingness ~ treatment * (demeaned covariates)
      covar_rhs <- paste(paste0("`", demeaned_names, "`"), collapse = " + ")
      full_formula <- stats::as.formula(
        paste(miss_col, "~", treatment_name, "* (", covar_rhs, ")")
      )
      fit_full <- .method(full_formula, data = analysis_data, ...)

      # Coefficient table
      coef_table <- broom::tidy(fit_full)
      coef_table$outcome <- v
      results_coef[[v]] <- coef_table

      # Wald F-test: jointly test treatment and all treatment:covariate interactions
      all_coef_names <- names(stats::coef(fit_full))
      test_terms <- all_coef_names[
        all_coef_names == treatment_name |
        grepl(paste0("^", treatment_name, ":"), all_coef_names) |
        grepl(paste0(":", treatment_name, "$"), all_coef_names)
      ]

      b <- stats::coef(fit_full)[test_terms]
      V <- stats::vcov(fit_full)[test_terms, test_terms]
      q <- length(test_terms)

      W <- as.numeric(t(b) %*% solve(V) %*% b)
      F_stat <- W / q
      df2 <- stats::df.residual(fit_full)
      p_value <- stats::pf(F_stat, q, df2, lower.tail = FALSE)

      results_ftest[[v]] <- tibble::tibble(
        outcome = v,
        F_stat = F_stat,
        df1 = as.integer(q),
        df2 = as.integer(df2),
        p_value = p_value
      )
    }

    coef_df <- dplyr::bind_rows(results_coef)
    coef_df <- dplyr::select(coef_df, outcome, dplyr::everything())

    ftest_df <- dplyr::bind_rows(results_ftest)

    cat("Coefficient estimates (Lin, 2013):\n")
    print(coef_df)
    cat("\nF-test of joint significance (treatment + treatment x covariate interactions):\n")
    print(ftest_df)

    result <- list(coefficients = coef_df, f_test = ftest_df)
    invisible(result)

  } else {
    # --- Original behavior: simple missingness ~ treatment ---

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
      form <- stats::as.formula(paste(miss_col, "~", treatment_name))
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

  # Get additional arguments (use match.call to avoid evaluating dots)
  mc <- match.call(expand.dots = FALSE)
  dots_exprs <- mc$...
  extra_args <- if (length(dots_exprs) > 0) {
    paste0(", ", paste(names(dots_exprs), "=", sapply(dots_exprs, deparse), collapse = ", "))
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