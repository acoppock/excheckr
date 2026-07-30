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
#' @param quiet Logical. The default \code{TRUE} returns the result, which
#'   auto-prints at the console. \code{FALSE} prints a labelled report instead
#'   and returns the same object invisibly, so nothing is printed twice.
#' @param .by Optional tidyselect expression naming columns to run the check
#'   separately within, e.g. \code{.by = c(X_pid_3, topic)}. Treatment assigned
#'   within strata makes a whole-sample check answer the wrong question, so this
#'   splits the data, runs the check on each stratum, and stacks the results with the
#'   grouping columns prepended. The return shape is unchanged, so it composes with
#'   every other argument. Strata are returned in order of first appearance and
#'   \code{NA} forms its own stratum, matching \code{tidyr::nest(.by = )}.
#'
#'   Without it the caller has to write the \code{nest} / \code{map} / \code{unnest}
#'   plumbing by hand and then attach \code{study_id} afterwards, because the
#'   argument cannot survive the \code{map}. With it, \code{study_id} works.
#' @param ... Additional arguments passed to `.method` (e.g., `clusters`, `se_type`).
#'
#' @return When \code{covariates} is \code{NULL}, a tibble with one row per
#'   outcome and columns \code{outcome}, \code{F_stat}, \code{df1}, \code{df2},
#'   \code{p_value}, \code{nobs}, and \code{estimable}. The test is the omnibus
#'   F-test from regressing the missingness indicator on treatment, so it covers
#'   multi-armed treatments as well as binary ones; it reports no coefficient,
#'   because with three or more arms there is no single number to report. Use
#'   \code{covariates} (below) if you want the coefficients themselves.
#'
#'   When \code{covariates} is provided, a list with three elements:
#'   \describe{
#'     \item{simple}{The covariate-free test above, computed on the same data, so
#'       that asking for the interacted test does not cost you the simpler one.}
#'     \item{coefficients}{A tibble of all coefficient estimates from the Lin model
#'       (treatment, demeaned covariates, and their interactions).}
#'     \item{f_test}{A tibble with one row per outcome containing the Wald F-test
#'       of joint significance of treatment and treatment-by-covariate
#'       interactions, plus an \code{estimable} column.}
#'   }
#'
#' @section Read the two tests together, not one instead of the other:
#' The covariate-free test asks whether dropout rates differ across arms. The
#' interacted test asks the more demanding question of whether they differ
#' \emph{anywhere} in covariate space, and it spends a degree of freedom per
#' covariate per arm to ask it. Neither subsumes the other, and the covariate-free
#' one is usually the criterion to act on:
#' \itemize{
#'   \item It is sharper for the arm-level question. On a simulated study with
#'     planted differential attrition and two covariates, the covariate-free test
#'     returns \code{p = 0.0006} on 1 degree of freedom where the interacted test
#'     returns \code{0.0066} on 3.
#'   \item It is well calibrated under cluster randomization, at 4.3 percent
#'     against a nominal 5, where the interacted test rejects about 12 percent of
#'     the time and no correction repairs it.
#'   \item It cannot run out of degrees of freedom. The interacted test can, which
#'     is what \code{estimatrTools::check_attrition_lasso} exists to address.
#' }
#' Earlier versions returned only the interacted test when \code{covariates} was
#' supplied, so projects that wanted both called the function twice. Both now come
#' back from one call, and the second fit is no longer wasted.
#'
#' @section When there is no p-value, and what that means:
#' \code{status} records why a row has no p-value, because the reasons mean opposite
#' things and collapsing them distorts any rate computed from the result. It takes
#' four values, and \code{estimable} is simply \code{status == "tested"}:
#' \describe{
#'   \item{\code{"tested"}}{A p-value was computed. Only these belong in a
#'     uniform-reference diagnostic.}
#'   \item{\code{"no_attrition"}}{Nobody was missing this outcome. \strong{This is a
#'     pass, not a missing test}: with no attrition there can be no differential
#'     attrition, so the design question is answered in the affirmative.}
#'   \item{\code{"all_missing"}}{Everybody was missing it, so nothing can be
#'     learned. Uninformative, and usually a sign the outcome was not asked of this
#'     subgroup.}
#'   \item{\code{"not_estimable"}}{The indicator varies but no statistic came back,
#'     from rank deficiency or a degenerate robust covariance matrix at very low
#'     missingness. Uninformative.}
#' }
#'
#' @section Two different rates, and which one you want:
#' Because \code{"no_attrition"} is a pass rather than a gap, there are two defensible
#' rates and they answer different questions. On one real corpus of 1320 study-arm
#' rows, 31 flagged at \code{alpha = 0.05}, 849 had no attrition, 51 were
#' uninformative, and 389 were tested and passed:
#' \itemize{
#'   \item \strong{How much differential attrition is in this corpus?} Count the
#'     no-attrition rows as passes: \code{31 / 1269 = 2.4\%}. Exclude only the
#'     genuinely uninformative rows. This is the number for a sentence about how much
#'     attrition trouble a corpus has.
#'   \item \strong{Are the computed p-values uniform, as a valid design implies?}
#'     Use only tested rows: \code{31 / 420 = 7.4\%}. A row with no attrition
#'     produces no draw from Uniform(0, 1), so it cannot enter this comparison at
#'     all.
#' }
#' Quoting the second while describing the first overstates the failure rate roughly
#' threefold, since it silently narrows the denominator from every row where the
#' question applies to only those rows with some attrition. Quoting the first while
#' testing uniformity is the mirror error, and is what reporting
#' \code{p_value = 1} for the no-attrition rows used to produce: it put a spike of
#' 849 ones at the top of the distribution, which made
#' \code{\link{summarize_check_pvalues}} report a badly non-uniform collection when
#' nothing was wrong, while moving \code{pct_below} in the reassuring direction.
#'
#' \code{n_missing} is returned so either rate can be computed without going back to
#' the data.
#'
#' @section Clustered designs:
#' Passing \code{clusters} and \code{se_type = "CR2"} is enough for the
#' covariate-free test, which is well calibrated (4.3 percent rejection at a
#' nominal 5 percent in a 30-cluster simulation). It is not enough for the
#' covariate-adjusted Wald test, which rejects about 12 percent of the time in the
#' same design. The cause is not the denominator degrees of freedom, so no
#' correction fixes it: substituting the number of clusters for the residual
#' degrees of freedom moves the rejection rate from 11.4 to 11.2 percent. The
#' cluster-robust variance estimator is itself biased downward when treatment is
#' constant within cluster. A warning is emitted when \code{clusters} is supplied
#' with \code{covariates}; treat that p-value as descriptive.
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
#' @family per-study checks
#' @export
check_attrition <- function(data, treatment, outcomes = NULL, covariates = NULL,
                            .method = estimatr::lm_robust, study_id = NULL,
                            quiet = TRUE, .by = NULL, ...) {
  # Capture treatment variable name
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Detect a clusters argument in the dots, so the multi-df Wald test can warn
  # about its own reference distribution. See the "Clustered designs" section.
  dots_call <- match.call(expand.dots = FALSE)$...
  has_clusters <- "clusters" %in% names(dots_call)

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

  # Stratified run. Both the outcome and covariate lists are already resolved to
  # names, so nothing needs re-selecting per stratum, and printing happens once at
  # the end rather than once per stratum.
  by_quo <- rlang::enquo(.by)
  if (!rlang::quo_is_null(by_quo)) {
    by_cols <- by_column_names(data, by_quo)
    varnames <- setdiff(varnames, by_cols)
    if (length(varnames) == 0) {
      warning("No outcomes left for attrition check after removing .by columns.")
      return(invisible(NULL))
    }
    covar_arg <- if (has_covariates) setdiff(covar_names, by_cols) else NULL
    result <- run_by_strata(data, by_quo, function(d) {
      # Omit covariates entirely rather than passing NULL. Supply is detected with
      # substitute(), so an explicit NULL reads as "an argument was given" and then
      # warns about selecting nothing from it, once per stratum.
      if (is.null(covar_arg)) {
        check_attrition(d, !!treatment_name, outcomes = varnames,
                        .method = .method, study_id = study_id, quiet = TRUE, ...)
      } else {
        check_attrition(d, !!treatment_name, outcomes = varnames,
                        covariates = covar_arg, .method = .method,
                        study_id = study_id, quiet = TRUE, ...)
      }
    })
    if (!quiet) {
      print(result)
      return(invisible(result))
    }
    return(result)
  }

  if (has_covariates) {
    # --- Lin (2013) model with F-test ---

    if (has_clusters) {
      warning("check_attrition: the covariate-adjusted joint Wald test ",
              "over-rejects under cluster randomization (about 12 percent at a ",
              "nominal 5 percent with 30 clusters), and no degrees-of-freedom ",
              "correction repairs it. Read its p-value as descriptive. The ",
              "covariate-free test from the same function is well calibrated.")
    }

    warn_aliased_covariates(data, covar_names, "check_attrition")

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

      # No variation in the missingness indicator means there is no test to run,
      # in either direction: nobody dropped out, or everybody did. Report it as
      # unestimable rather than inventing p = 1, which would otherwise pile up
      # as a spike at 1 in the uniform-reference diagnostics downstream.
      miss_vals <- analysis_data[[miss_col]]
      if (!has_variation(miss_vals)) {
        results_coef[[v]] <- tibble::tibble(
          outcome = v, term = treatment_name, estimate = NA_real_,
          std.error = NA_real_, statistic = NA_real_, p.value = NA_real_
        )
        results_ftest[[v]] <- tibble::tibble(
          outcome = v, F_stat = NA_real_, df1 = NA_integer_, df2 = NA_integer_,
          p_value = NA_real_,
          status = if (sum(miss_vals == 1, na.rm = TRUE) == 0L) "no_attrition" else "all_missing",
          estimable = FALSE
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

      # Wald F-test: jointly test treatment and all treatment:covariate
      # interactions. In a fully interacted model every coefficient that is
      # neither the intercept nor a covariate main effect is a treatment term,
      # so take that complement rather than matching on the treatment name: a
      # covariate whose name merely begins with the treatment name (Zeal_c
      # against a treatment called Z) would otherwise be swept into the test.
      b_all <- stats::coef(fit_full)
      bare <- gsub("`", "", names(b_all), fixed = TRUE)
      test_terms <- names(b_all)[bare != "(Intercept)" & !bare %in% demeaned_names]
      q <- length(test_terms)
      df2 <- stats::df.residual(fit_full)

      # Rank deficiency shows up either as an NA coefficient (the term was
      # dropped, so vcov has no row for it) or as a singular V.
      W <- tryCatch({
        b <- b_all[test_terms]
        if (anyNA(b)) stop("rank deficient", call. = FALSE)
        V <- stats::vcov(fit_full)[test_terms, test_terms, drop = FALSE]
        as.numeric(t(b) %*% solve(V) %*% b)
      }, error = function(e) NULL)

      if (is.null(W)) {
        warning(paste("F-test not estimable for outcome:", v,
                      "(rank-deficient or singular covariance matrix:",
                      "too many covariates or near-collinearity)"))
        results_ftest[[v]] <- tibble::tibble(
          outcome = v,
          F_stat = NA_real_,
          df1 = as.integer(q),
          df2 = as.integer(df2),
          p_value = NA_real_,
          status = "not_estimable",
          estimable = FALSE
        )
      } else {
        F_stat <- W / q
        p_value <- stats::pf(F_stat, q, df2, lower.tail = FALSE)
        results_ftest[[v]] <- tibble::tibble(
          outcome = v,
          F_stat = F_stat,
          df1 = as.integer(q),
          df2 = as.integer(df2),
          p_value = p_value,
          status = if (is.na(p_value)) "not_estimable" else "tested",
          estimable = !is.na(p_value)
        )
      }
    }

    coef_df <- dplyr::bind_rows(results_coef)
    coef_df <- dplyr::select(coef_df, "outcome", dplyr::everything())

    ftest_df <- dplyr::bind_rows(results_ftest)

    # The covariate-free test is computed here too. It answers a different and
    # less demanding question than the interacted one, it is the better calibrated
    # of the pair under clustering, and it is the primary criterion for arm-level
    # differential dropout, so a caller asking for the interacted test should not
    # have to run the function twice to keep it.
    simple_df <- simple_attrition_tests(data, varnames, treatment_name, .method, ...)

    if (!is.null(study_id)) {
      simple_df$study_id <- study_id
      coef_df$study_id <- study_id
      ftest_df$study_id <- study_id
    }

    result <- list(simple = simple_df, coefficients = coef_df, f_test = ftest_df)

    if (!quiet) {
      cat("Covariate-free omnibus test:\n")
      print(simple_df)
      cat("\nCoefficient estimates (Lin, 2013):\n")
      print(coef_df)
      cat("\nF-test of joint significance (treatment + treatment x covariate interactions):\n")
      print(ftest_df)
      return(invisible(result))
    }
    result

  } else {
    result_df <- simple_attrition_tests(data, varnames, treatment_name, .method, ...)
    if (!is.null(study_id)) result_df$study_id <- study_id

    if (!quiet) {
      print(result_df)
      return(invisible(result_df))
    }
    result_df
  }
}


#' Covariate-free omnibus attrition test, one row per outcome
#'
#' The covariate-free half of \code{check_attrition}, factored out because both
#' branches need it: on its own when \code{covariates} is \code{NULL}, and
#' alongside the Lin model as the \code{simple} element when it is not. Returning
#' both from one call matters because the two answer different questions and the
#' covariate-free one is the better-behaved of the pair, so a caller should not
#' have to give one up to get the other.
#'
#' @param data A data frame.
#' @param varnames Character vector of outcome column names.
#' @param treatment_name Name of the treatment column.
#' @param .method Regression function.
#' @param ... Passed to \code{.method}.
#' @return A tibble with one row per outcome.
#' @keywords internal
#' @noRd
simple_attrition_tests <- function(data, varnames, treatment_name, .method, ...) {
  results <- lapply(varnames, function(v) {
    # Use a pre-built missingness indicator when the cleaning made one.
    missing_var <- paste0(v, "_missing")
    if (missing_var %in% names(data)) {
      miss_col <- missing_var
    } else {
      miss_col <- paste0(v, "_missing_temp")
      data[[miss_col]] <- as.integer(is.na(data[[v]]))
    }

    # No variation in the missingness indicator means there is no test to run,
    # in either direction: nobody dropped out, or everybody did. Report it as
    # unestimable rather than inventing p = 1, which would otherwise pile up as a
    # spike at 1 in the uniform-reference diagnostics downstream.
    miss_vals <- data[[miss_col]]
    n_obs <- sum(!is.na(miss_vals))
    n_miss <- sum(miss_vals == 1, na.rm = TRUE)

    if (!has_variation(miss_vals)) {
      # Distinguish the two ways an indicator can fail to vary, because they mean
      # opposite things. Nobody missing is a pass: with no attrition there can be
      # no differential attrition, and lumping it in with genuine failures inflates
      # any rate computed over "tests that ran". Everybody missing is uninformative.
      return(tibble::tibble(
        outcome   = v,
        F_stat    = NA_real_,
        df1       = NA_integer_,
        df2       = NA_integer_,
        p_value   = NA_real_,
        nobs      = as.integer(n_obs),
        n_missing = as.integer(n_miss),
        status    = if (n_miss == 0L) "no_attrition" else "all_missing",
        estimable = FALSE
      ))
    }

    # Omnibus F-test, so a multi-arm factor treatment is handled correctly.
    form <- stats::as.formula(paste(miss_col, "~", treatment_name))
    fit <- .method(form, data = data, ...)
    gl <- broom::glance(fit)
    tibble::tibble(
      outcome   = v,
      F_stat    = gl$statistic,
      df1       = model_df1(fit),
      df2       = as.integer(gl$df.residual),
      p_value   = gl$p.value,
      nobs      = as.integer(gl$nobs),
      n_missing = as.integer(n_miss),
      # The model can fit and still yield no statistic, when the robust covariance
      # matrix is degenerate at very low missingness. Key both columns off the
      # p-value rather than off reaching this line.
      status    = if (is.na(gl$p.value)) "not_estimable" else "tested",
      estimable = !is.na(gl$p.value)
    )
  })
  dplyr::bind_rows(results)
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
#' @family code generators
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