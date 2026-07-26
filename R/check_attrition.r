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
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column holding this value is appended to every returned tibble.
#' @param quiet Logical. If \code{TRUE}, suppresses all console output (default
#'   \code{FALSE}). Set to \code{TRUE} when calling programmatically inside
#'   \code{map()} or similar.
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
#' set.seed(42)
#' n <- 200
#' dat <- data.frame(
#'   Z = rep(c(0L, 1L), n / 2),
#'   X_age = rnorm(n, 50, 10),
#'   X_income = rnorm(n, 50000, 10000)
#' )
#' dat$Y_attitude <- rnorm(n)
#' dat$Y_attitude[which(rbinom(n, 1, ifelse(dat$Z == 1, 0.35, 0.15)) == 1)] <- NA
#' dat$Y_behavior <- rnorm(n)
#' dat$Y_behavior[which(rbinom(n, 1, 0.15) == 1)] <- NA
#'
#' # Simple attrition check (no covariates)
#' check_attrition(dat, Z)
#'
#' # With covariates: Lin model + F-test for differential attrition
#' check_attrition(dat, Z, covariates = c("X_age", "X_income"))
#'
#' \donttest{
#' # Cluster-randomized experiment (requires randomizr)
#' if (requireNamespace("randomizr", quietly = TRUE)) {
#'   dat_cl <- data.frame(cluster_id = rep(1:20, each = 10))
#'   dat_cl$Z <- randomizr::cluster_ra(clusters = dat_cl$cluster_id)
#'   dat_cl$Y_outcome <- 0.5 * dat_cl$Z + rnorm(200)
#'   dat_cl$Y_outcome[which(rbinom(200, 1, ifelse(dat_cl$Z == 1, 0.30, 0.10)) == 1)] <- NA
#'   check_attrition(dat_cl, Z, clusters = cluster_id)
#' }
#' }
#'
#' @importFrom dplyr bind_rows select
#' @importFrom broom tidy glance
#' @importFrom rlang ensym as_name expr enquo quo_is_null eval_tidy
#' @importFrom tidyselect eval_select
#' @importFrom tibble tibble
#' @importFrom stats coef vcov df.residual pf as.formula
#' @export
check_attrition <- function(data, treatment, outcomes = NULL, covariates = NULL,
                            .method = estimatr::lm_robust, study_id = NULL,
                            quiet = TRUE, ...) {
  # Capture treatment variable name
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle outcome selection
  outcomes_quo <- rlang::enquo(outcomes)
  if (rlang::quo_is_null(outcomes_quo)) {
    varnames <- auto_select_vars(data, prefix = "Y")
  } else {
    char_val <- tryCatch(rlang::eval_tidy(outcomes_quo), error = function(e) NULL)
    if (is.character(char_val)) {
      varnames <- char_val
    } else {
      varnames <- names(eval_select(outcomes_quo, data))
    }
  }

  if (length(varnames) == 0) {
    warning("No outcomes selected for attrition check.")
    return(invisible(NULL))
  }

  # Handle covariate selection
  covar_expr <- substitute(covariates)
  has_covariates <- !is.null(covar_expr)

  if (has_covariates) {
    covariates_quo <- rlang::enquo(covariates)
    char_cov <- tryCatch(rlang::eval_tidy(covariates_quo), error = function(e) NULL)
    if (is.character(char_cov)) {
      covar_names <- char_cov
    } else {
      covar_names <- names(eval_select(covariates_quo, data))
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

      # Short-circuit: no attrition at all → trivial result
      miss_vals <- analysis_data[[miss_col]]
      if (all(miss_vals == 0, na.rm = TRUE)) {
        results_coef[[v]] <- tibble::tibble(
          outcome = v, term = treatment_name, estimate = 0,
          std.error = NA_real_, statistic = NA_real_, p.value = 1
        )
        results_ftest[[v]] <- tibble::tibble(
          outcome = v, F_stat = 0, df1 = NA_integer_, df2 = NA_integer_, p_value = 1
        )
        next
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

      # Wald F-test: jointly test treatment and all treatment:covariate interactions.
      # Matches exact name (binary Z), factor-level names (Zlevel), and all
      # interaction terms involving treatment regardless of position.
      all_coef_names <- names(stats::coef(fit_full))
      test_terms <- all_coef_names[
        startsWith(all_coef_names, treatment_name) |
        grepl(paste0(":", treatment_name), all_coef_names)
      ]

      b <- stats::coef(fit_full)[test_terms]
      V <- stats::vcov(fit_full)[test_terms, test_terms]
      q <- length(test_terms)

      df2 <- stats::df.residual(fit_full)
      W <- tryCatch(
        as.numeric(t(b) %*% solve(V) %*% b),
        error = function(e) NULL
      )

      if (is.null(W)) {
        warning(paste("F-test not estimable for outcome:", v,
                      "(singular covariance matrix: too many covariates or near-collinearity)"))
        results_ftest[[v]] <- tibble::tibble(
          outcome = v,
          F_stat = NA_real_,
          df1 = as.integer(q),
          df2 = as.integer(df2),
          p_value = NA_real_
        )
      } else {
        F_stat <- W / q
        p_value <- stats::pf(F_stat, q, df2, lower.tail = FALSE)
        results_ftest[[v]] <- tibble::tibble(
          outcome = v,
          F_stat = F_stat,
          df1 = as.integer(q),
          df2 = as.integer(df2),
          p_value = p_value
        )
      }
    }

    coef_df <- dplyr::bind_rows(results_coef)
    coef_df <- dplyr::select(coef_df, "outcome", dplyr::everything())

    ftest_df <- dplyr::bind_rows(results_ftest)

    if (!is.null(study_id)) {
      coef_df$study_id <- study_id
      ftest_df$study_id <- study_id
    }

    if (!quiet) {
      cat("Coefficient estimates (Lin, 2013):\n")
      print(coef_df)
      cat("\nF-test of joint significance (treatment + treatment x covariate interactions):\n")
      print(ftest_df)
    }

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

      # If no attrition at all, return trivial result immediately
      miss_vals <- data[[miss_col]]
      if (all(miss_vals == 0, na.rm = TRUE)) {
        return(tibble::tibble(
          outcome = v,
          F_stat  = 0,
          df1     = NA_integer_,
          df2     = NA_integer_,
          p_value = 1,
          nobs    = as.integer(sum(!is.na(miss_vals)))
        ))
      }

      # Regress missingness ~ treatment; use omnibus F-test so multi-arm
      # factor treatments (3+ levels) are handled correctly
      form <- stats::as.formula(paste(miss_col, "~", treatment_name))
      fit <- .method(form, data = data, ...)
      gl <- broom::glance(fit)
      # Use fit$k - 1 for df1: glance() returns a data.frame whose $df
      # partial-matches $df.residual, so gl$df is unreliable.
      tibble::tibble(
        outcome = v,
        F_stat  = gl$statistic,
        df1     = as.integer(fit$k - 1L),
        df2     = as.integer(gl$df.residual),
        p_value = gl$p.value,
        nobs    = as.integer(gl$nobs)
      )
    })

    result_df <- dplyr::bind_rows(results)
    if (!is.null(study_id)) result_df$study_id <- study_id

    if (!quiet) print(result_df)
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
#' set.seed(42)
#' n <- 200
#' dat <- data.frame(Z = rep(c(0L, 1L), n / 2))
#' dat$Y_attitude <- rnorm(n)
#' dat$Y_attitude[which(rbinom(n, 1, ifelse(dat$Z == 1, 0.35, 0.15)) == 1)] <- NA
#' dat$Y_behavior <- rnorm(n)
#' dat$Y_behavior[which(rbinom(n, 1, 0.15) == 1)] <- NA
#'
#' write_attrition_check_code(dat, Z)
#'
#' \donttest{
#' # Cluster-randomized experiment (requires randomizr)
#' if (requireNamespace("randomizr", quietly = TRUE)) {
#'   dat_cl <- data.frame(cluster_id = rep(1:20, each = 10))
#'   dat_cl$Z <- randomizr::cluster_ra(clusters = dat_cl$cluster_id)
#'   dat_cl$Y_outcome <- 0.5 * dat_cl$Z + rnorm(200)
#'   write_attrition_check_code(dat_cl, Z, clusters = cluster_id)
#' }
#' }
#'
#' @importFrom rlang ensym as_name enquo quo_is_null eval_tidy
#' @importFrom tidyselect eval_select
#' @importFrom glue glue
#' @export
write_attrition_check_code <- function(data, treatment, outcomes = NULL, .method = estimatr::lm_robust, ...) {
  # Capture dataset and treatment names
  data_name <- rlang::as_name(rlang::ensym(data))
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle outcome selection
  outcomes_quo <- rlang::enquo(outcomes)
  if (rlang::quo_is_null(outcomes_quo)) {
    varnames <- auto_select_vars(data, prefix = "Y_")
  } else {
    char_val <- tryCatch(rlang::eval_tidy(outcomes_quo), error = function(e) NULL)
    if (is.character(char_val)) {
      varnames <- char_val
    } else {
      varnames <- names(eval_select(outcomes_quo, data))
    }
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

  method_name <- sub("^.*::", "", deparse(substitute(.method)))

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