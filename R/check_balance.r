#' Check covariate balance across treatment conditions
#'
#' Performs balance checks by regressing each covariate on treatment assignment
#' (covariate-by-covariate F-tests) and a joint test of all covariates together.
#' Supports both binary and multi-armed treatments.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param covariates Character vector of covariate names, or unquoted column names
#'   using tidyselect helpers. If left empty, all `"X_"` columns are used.
#' @param .method Regression function to use (default: `estimatr::lm_robust`).
#'   Must accept formula and data arguments.
#' @param declaration Optional. A \code{randomizr} declaration object (e.g.,
#'   from \code{randomizr::declare_ra}). When provided, the joint test uses
#'   randomization inference via \code{ri2::conduct_ri} instead of the
#'   parametric test. This is recommended for clustered designs or any design
#'   where exact inference is desired.
#' @param sims Integer. Number of simulations for randomization inference
#'   (default: 1000). Only used when \code{declaration} is provided.
#' @param quiet Logical. If \code{TRUE}, suppresses all console output (default
#'   \code{FALSE}). Set to \code{TRUE} when calling programmatically inside
#'   \code{map()} or similar.
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{covariate_tests}{A tibble with one row per covariate (or covariate
#'       level for factors), containing the F-test from regressing each covariate
#'       on treatment. Columns: covariate, level, F_stat, df1, df2, p_value, nobs.}
#'     \item{joint_test}{A tibble with a single row containing the joint test
#'       of all covariates predicting treatment. Columns: F_stat, df1, df2, p_value, nobs.}
#'   }
#'
#' @details
#' For numeric covariates, regresses the covariate on treatment directly and
#' extracts the overall model F-test (which jointly tests all treatment dummies
#' for multi-armed designs).
#' For factor/character covariates, creates dummy variables for each level
#' (excluding the first level as reference) and regresses each dummy on treatment.
#'
#' The joint test strategy depends on the number of treatment arms and whether
#' a \code{declaration} is provided:
#' \itemize{
#'   \item Binary treatment, no declaration: F-test from regressing numeric
#'     treatment on all covariates via \code{.method}.
#'   \item Multi-armed treatment, no declaration: multinomial likelihood-ratio
#'     test via \code{nnet::multinom}.
#'   \item Any treatment with declaration: randomization inference using the
#'     multinomial LR statistic as the test function, via \code{ri2::conduct_ri}.
#' }
#'
#' @examples
#' set.seed(42)
#' dat <- data.frame(
#'   Z = rep(c(0L, 1L), 100),
#'   X_age = rnorm(200, 50, 10),
#'   X_gender = sample(c("M", "F"), 200, replace = TRUE),
#'   X_party = factor(sample(c("D", "R", "I"), 200, replace = TRUE))
#' )
#' dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)
#'
#' # Default: all X_ covariates with lm_robust
#' check_balance(dat, Z)
#'
#' # Specific covariates
#' check_balance(dat, Z, c("X_age", "X_income"))
#'
#' # Multi-armed treatment (uses multinomial LR test)
#' set.seed(1)
#' dat2 <- data.frame(
#'   Z = factor(rep(c("C", "T1", "T2"), length.out = 201)),
#'   X_age = rnorm(201, 50, 10)
#' )
#' check_balance(dat2, Z)
#'
#' \donttest{
#' # Randomization inference with a declaration (requires randomizr and ri2)
#' if (requireNamespace("randomizr", quietly = TRUE) &&
#'     requireNamespace("ri2", quietly = TRUE)) {
#'   decl <- randomizr::declare_ra(N = 201, conditions = c("C", "T1", "T2"))
#'   check_balance(dat2, Z, declaration = decl, sims = 200)
#' }
#' }
#'
#' @importFrom dplyr bind_rows select
#' @importFrom broom tidy glance
#' @importFrom rlang ensym as_name expr enquo quo_is_null eval_tidy
#' @importFrom tidyselect eval_select
#' @importFrom tibble tibble
#' @importFrom stats as.formula model.matrix model.frame complete.cases pf coef fitted.values
#' @export
check_balance <- function(data, treatment, covariates = NULL, .method = estimatr::lm_robust,
                          declaration = NULL, sims = 1000, quiet = TRUE, ...) {
  # Capture treatment variable name
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle covariate selection
  covariates_quo <- rlang::enquo(covariates)
  if (rlang::quo_is_null(covariates_quo)) {
    varnames <- auto_select_vars(data, prefix = "X_")
  } else {
    char_val <- tryCatch(rlang::eval_tidy(covariates_quo), error = function(e) NULL)
    if (is.character(char_val)) {
      # Empty character vector falls back to auto-selection
      varnames <- if (length(char_val) > 0) char_val else auto_select_vars(data, prefix = "X_")
    } else {
      varnames <- names(eval_select(covariates_quo, data))
    }
  }

  if (length(varnames) == 0) {
    warning("No covariates selected for balance check.")
    return(invisible(NULL))
  }

  # Detect treatment type
  z_col <- data[[treatment_name]]
  if (is.factor(z_col)) {
    z_levels <- levels(z_col)
  } else if (is.character(z_col)) {
    z_col <- as.factor(z_col)
    z_levels <- levels(z_col)
  } else {
    # numeric
    z_levels <- sort(unique(z_col))
  }
  is_multiarm <- length(z_levels) > 2

  # Ensure treatment is a factor in data for covariate-by-covariate X ~ Z regressions
  # (so lm_robust creates K-1 dummies and the glance F-test is the overall model F)
  if (!is.factor(data[[treatment_name]])) {
    data[[treatment_name]] <- as.factor(data[[treatment_name]])
  }

  # Detect cluster column from dots
  dots_call <- match.call(expand.dots = FALSE)$...
  cluster_col <- NULL
  if ("clusters" %in% names(dots_call)) {
    cluster_col <- as.character(dots_call[["clusters"]])
  }

  # --- Covariate-by-covariate tests ---
  results <- lapply(varnames, function(v) {
    col <- data[[v]]

    if (is.numeric(col)) {
      # Numeric covariate: regress X ~ Z, extract overall F-test
      form <- stats::as.formula(paste(paste0("`", v, "`"), "~", treatment_name))
      fit <- .method(form, data = data, ...)
      gl <- broom::glance(fit)
      # Use fit$k - 1 for df1: glance() returns a data.frame whose $df
      # partial-matches $df.residual, so gl$df is unreliable.
      tibble::tibble(
        covariate = v,
        level = NA_character_,
        F_stat = gl$statistic,
        df1 = as.integer(fit$k - 1L),
        df2 = as.integer(gl$df.residual),
        p_value = gl$p.value,
        nobs = as.integer(gl$nobs)
      )
    } else {
      # Factor/character covariate: create dummies for each level
      if (!is.factor(col)) col <- as.factor(col)
      levels_vec <- levels(col)

      dummy_results <- lapply(levels_vec[-1], function(lev) {
        clean_lev <- gsub("[^[:alnum:]_]", "_", lev)
        dummy_name <- paste0(v, "_", clean_lev)
        data[[dummy_name]] <- as.integer(col == lev)

        form <- stats::as.formula(paste(paste0("`", dummy_name, "`"), "~", treatment_name))
        fit <- .method(form, data = data, ...)
        gl <- broom::glance(fit)
        tibble::tibble(
          covariate = v,
          level = lev,
          F_stat = gl$statistic,
          df1 = as.integer(fit$k - 1L),
          df2 = as.integer(gl$df.residual),
          p_value = gl$p.value,
          nobs = as.integer(gl$nobs)
        )
      })
      dplyr::bind_rows(dummy_results)
    }
  })
  covariate_tests <- dplyr::bind_rows(results)

  # --- Joint test ---
  # Expand all covariates to numeric matrix
  covar_formula <- stats::as.formula(paste("~", paste(paste0("`", varnames, "`"), collapse = " + ")))
  covar_mat <- stats::model.matrix(
    covar_formula,
    data = stats::model.frame(covar_formula, data, na.action = stats::na.pass)
  )
  # Drop intercept
  covar_mat <- covar_mat[, -1, drop = FALSE]
  expanded_names <- colnames(covar_mat)

  # Add expanded covariates to data
  analysis_data <- data
  for (cname in expanded_names) {
    analysis_data[[cname]] <- covar_mat[, cname]
  }

  if (!is.null(declaration)) {
    # Randomization inference path: any K
    joint_test <- ri_joint_test(
      data = analysis_data,
      treatment_name = treatment_name,
      covariate_cols = expanded_names,
      declaration = declaration,
      sims = sims
    )
  } else if (!is_multiarm) {
    # Binary treatment: single regression Z ~ X1 + X2 + ...
    # Use numeric 0/1 response for the joint test
    analysis_data[[".Z_numeric"]] <- as.numeric(analysis_data[[treatment_name]]) - 1
    bt_names <- paste0("`", expanded_names, "`")
    joint_formula <- stats::as.formula(
      paste(".Z_numeric", "~", paste(bt_names, collapse = " + "))
    )
    fit_joint <- .method(joint_formula, data = analysis_data, ...)
    gl <- broom::glance(fit_joint)
    joint_test <- tibble::tibble(
      F_stat = gl$statistic,
      df1 = as.integer(fit_joint$k - 1L),
      df2 = as.integer(gl$df.residual),
      p_value = gl$p.value,
      nobs = as.integer(gl$nobs)
    )
  } else {
    # Multi-armed treatment: multinomial likelihood-ratio test
    joint_test <- multinomial_lr_joint_test(
      data = analysis_data,
      treatment_name = treatment_name,
      covariate_cols = expanded_names
    )
  }

  result <- list(covariate_tests = covariate_tests, joint_test = joint_test)

  if (!quiet) {
    cat("Covariate-by-covariate balance tests:\n")
    print(covariate_tests)
    cat("\nJoint balance test:\n")
    print(joint_test)
  }

  invisible(result)
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
#' The joint balance test is generated as a call to \code{check_balance()},
#' since the cross-equation Wald test is too complex to emit as copy-paste code.
#'
#' @examples
#' set.seed(42)
#' dat <- data.frame(
#'   Z = rep(c(0L, 1L), 100),
#'   X_age = rnorm(200, 50, 10),
#'   X_gender = sample(c("M", "F"), 200, replace = TRUE)
#' )
#' dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)
#'
#' write_balance_check_code(dat, Z)
#'
#' \donttest{
#' # Cluster-randomized experiment (requires randomizr)
#' if (requireNamespace("randomizr", quietly = TRUE)) {
#'   dat_cl <- data.frame(cluster_id = rep(1:20, each = 10))
#'   dat_cl$Z <- randomizr::cluster_ra(clusters = dat_cl$cluster_id)
#'   dat_cl$X_age <- rnorm(200, 50, 10)
#'   dat_cl$X_income <- 50000 + rnorm(200, 0, 10000)
#'   write_balance_check_code(dat_cl, Z, clusters = cluster_id)
#' }
#' }
#'
#' @importFrom rlang ensym as_name enquo quo_is_null eval_tidy
#' @importFrom tidyselect eval_select
#' @importFrom glue glue
#' @export
write_balance_check_code <- function(data, treatment, covariates = NULL, .method = estimatr::lm_robust, ...) {
  # Capture dataset and treatment names
  data_name <- rlang::as_name(rlang::ensym(data))
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Handle covariate selection
  covariates_quo <- rlang::enquo(covariates)
  if (rlang::quo_is_null(covariates_quo)) {
    varnames <- auto_select_vars(data, prefix = "X_")
  } else {
    char_val <- tryCatch(rlang::eval_tidy(covariates_quo), error = function(e) NULL)
    if (is.character(char_val)) {
      varnames <- char_val
    } else {
      varnames <- names(eval_select(covariates_quo, data))
    }
  }

  if (length(varnames) == 0) {
    warning("No covariates selected for balance check.")
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

  # Ensure treatment is factor for multi-arm detection
  z_col <- data[[treatment_name]]
  z_levels <- if (is.factor(z_col)) levels(z_col) else unique(z_col)
  is_multiarm <- length(z_levels) > 2

  # Generate covariate-by-covariate code
  code_lines <- vapply(varnames, function(v) {
    col <- data[[v]]

    if (is.numeric(col)) {
      glue::glue(
        "# Balance check for {v}\n",
        "glance({method_name}({v} ~ {treatment_name}, data = {data_name}{extra_args}))"
      )
    } else {
      if (!is.factor(col)) col <- as.factor(col)
      levels_vec <- levels(col)

      dummy_lines <- vapply(levels_vec[-1], function(lev) {
        clean_lev <- gsub("[^[:alnum:]_]", "_", lev)
        dummy_name <- paste0(v, "_", clean_lev)
        glue::glue(
          "# Balance check for {v} (level: {lev})\n",
          "{data_name}${dummy_name} <- as.integer({data_name}${v} == '{lev}')\n",
          "glance({method_name}({dummy_name} ~ {treatment_name}, data = {data_name}{extra_args}))"
        )
      }, character(1))

      paste(dummy_lines, collapse = "\n\n")
    }
  }, character(1))

  # Generate joint test code
  if (!is_multiarm) {
    # Binary treatment: explicit regression of numeric Z on all covariates
    # Expand factor covariates to dummies for the joint formula
    all_rhs <- character(0)
    for (v in varnames) {
      col <- data[[v]]
      if (is.numeric(col)) {
        all_rhs <- c(all_rhs, v)
      } else {
        if (!is.factor(col)) col <- as.factor(col)
        levels_vec <- levels(col)
        for (lev in levels_vec[-1]) {
          clean_lev <- gsub("[^[:alnum:]_]", "_", lev)
          all_rhs <- c(all_rhs, paste0(v, "_", clean_lev))
        }
      }
    }
    rhs_str <- paste(all_rhs, collapse = " + ")
    joint_line <- glue::glue(
      "# Joint balance test (all covariates)\n",
      "{data_name}$.Z_numeric <- as.numeric(as.factor({data_name}${treatment_name})) - 1\n",
      "glance({method_name}(.Z_numeric ~ {rhs_str}, data = {data_name}{extra_args}))"
    )
  } else {
    # Multi-armed treatment: cross-equation Wald test is too complex for inline code
    covar_arg <- paste0("c(", paste0('"', varnames, '"', collapse = ", "), ")")
    joint_line <- glue::glue(
      "# Joint balance test (cross-equation Wald test for multi-armed treatment)\n",
      "# This test requires a stacked sandwich estimator across K-1 equations;\n",
      "# use check_balance() which implements it internally\n",
      "check_balance({data_name}, {treatment_name}, {covar_arg}{extra_args})"
    )
  }

  code <- paste(c(code_lines, joint_line), collapse = "\n\n")

  cat(code, "\n")
  invisible(code)
}
