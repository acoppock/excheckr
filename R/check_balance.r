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
#'   from \code{randomizr::declare_ra}), or the string \code{"complete"}. When
#'   provided, the joint test uses randomization inference via
#'   \code{ri2::conduct_ri} instead of the parametric test. This is recommended
#'   for clustered designs or any design where exact inference is desired, and
#'   for multi-arm designs with many covariates, where the multinomial
#'   likelihood-ratio test's asymptotic reference distribution is unreliable.
#'   \code{"complete"} is shorthand for complete random assignment holding the
#'   observed arm sizes fixed; supply a real declaration whenever the design was
#'   blocked or clustered.
#' @param sims Integer. Number of simulations for randomization inference
#'   (default: 1000). Only used when \code{declaration} is provided.
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column holding this value is appended to both returned tibbles.
#' @param flatten Logical. If \code{TRUE}, returns a single tibble with the
#'   covariate tests and the joint test stacked and distinguished by a
#'   \code{test} column, rather than a two-element list (default \code{FALSE}).
#' @param quiet Logical. The default \code{TRUE} returns the result, which
#'   auto-prints at the console. \code{FALSE} prints a labelled report instead
#'   and returns the same object invisibly, so nothing is printed twice.
#' @param max_orders_apart How far the joint Wald p-value may fall below the
#'   classical F p-value on the same fit, in orders of magnitude, before the joint
#'   test is suppressed with a warning instead of reported (default \code{6}). The
#'   two forms test the same hypothesis and differ only in that the Wald form
#'   inverts the coefficient covariance matrix, so a large gap indicates a failed
#'   inversion rather than real imbalance. See the section below. Only the binary,
#'   no-\code{declaration} path uses this; the randomization-inference path inverts
#'   nothing and needs no such guard.
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
#' @return When \code{flatten = FALSE} (the default), a list with two elements:
#'   \describe{
#'     \item{covariate_tests}{A tibble with one row per covariate (or covariate
#'       level for factors), containing the test from regressing each covariate
#'       on treatment. Columns: covariate, level, F_stat, statistic, df1, df2,
#'       p_value, nobs.}
#'     \item{joint_test}{A tibble with a single row containing the joint test
#'       of all covariates predicting treatment. Same columns, without
#'       covariate and level.}
#'   }
#'   When \code{flatten = TRUE}, a single tibble stacking both, with a
#'   \code{test} column taking values \code{"covariate"} and \code{"joint"}.
#'
#' @section What \code{F_stat} contains:
#' The \code{F_stat} column does not always hold an F statistic, because the
#' joint test is not always an F test. The \code{statistic} column records which
#' quantity it is, so that results stacked across studies stay interpretable:
#' \itemize{
#'   \item \code{"F"}: a genuine F statistic. All covariate-by-covariate tests,
#'     and the joint test for a binary treatment with no \code{declaration}.
#'   \item \code{"LR/df"}: a multinomial likelihood-ratio statistic divided by
#'     its degrees of freedom, which is F-like but is not an F. The joint test
#'     for a multi-armed treatment with no \code{declaration}.
#'   \item \code{"LR"}: the raw multinomial likelihood-ratio statistic,
#'     undivided. The joint test on the randomization inference path, where the
#'     reference distribution is the permutation distribution rather than a
#'     parametric one, so there is nothing to divide by.
#' }
#' Stacking a mix of binary and multi-armed studies therefore puts different
#' quantities in one \code{F_stat} column. The \code{p_value} column is
#' comparable across all three; \code{F_stat} is not. Group by \code{statistic}
#' before comparing or plotting the statistics themselves.
#'
#' @section Clustered designs:
#' The parametric joint test over-rejects badly under cluster randomization, at
#' about 13 percent against a nominal 5 percent in a 30-cluster simulation, even
#' when \code{clusters} and \code{se_type = "CR2"} are supplied. The denominator
#' degrees of freedom are not the cause and no correction repairs it: substituting
#' the cluster count for the residual degrees of freedom moves the rate from 11.4
#' to 11.2 percent. The cluster-robust variance estimator is itself biased
#' downward because treatment is constant within cluster, so the effective sample
#' size is the number of clusters. Supplying \code{declaration} brings the same
#' design to 4.5 percent, and a warning is emitted when \code{clusters} is passed
#' without one.
#'
#' @section Aliased covariates: warned about, never dropped for you:
#' A covariate that is a linear function of the others carries no separate
#' information, and the joint test cannot estimate a coefficient for it. When that
#' happens \code{check_balance} warns, names each redundant covariate and the
#' covariates that determine it where the relationship is exact, and prints the
#' \code{covariates =} line that would fix the call. It does not prune. Choosing
#' which of two redundant covariates to keep is an analysis decision that belongs in
#' the script where a reader can see it, not inside a check.
#'
#' The warning is the only signal you get, which is why it exists.
#' \code{lm_robust} does not refuse a rank-deficient design: it drops the aliased
#' column and reports an F on the remainder, so without the warning the caller
#' receives a p-value for a covariate set they did not specify and nothing in the
#' returned object records the substitution. (An unobserved factor level is the one
#' case that can instead return \code{NA} outright.) That is how balance p-values
#' have been computed on redundant batteries without anyone noticing.
#'
#' The redundancies seen in practice are mundane and worth recognising: the same
#' variable under two names, such as a female indicator beside a woman indicator; a
#' coarsening beside the thing it coarsens, such as a Republican indicator beside a
#' three-category party factor, or a college indicator beside a three-level
#' education factor; and a continuous measure beside a binned version of itself.
#' Resolving them also shrinks the covariate count, which the section below explains
#' is worth doing for a second, independent reason.
#'
#' @section When the joint test cannot be trusted:
#' The joint test is a Wald test, so it inverts the covariance matrix of the
#' coefficients rather than reading its diagonal. When that matrix is
#' near-singular, its inverse is dominated by numerical noise and the statistic can
#' come out arbitrarily large while every marginal standard error still looks
#' reasonable. The result is a p-value that is not merely anti-conservative but
#' impossible.
#'
#' Real instances, all found in working meta-analysis corpora:
#' \itemize{
#'   \item 1578 respondents, 37 covariates, 1511 household clusters: joint
#'     \code{p = 5.8e-108} while no covariate-by-covariate p-value fell below 0.09.
#'   \item 2937 respondents, 40 covariates, no clustering: joint
#'     \code{p = 1.5e-248} against a classical \code{p = 0.23}.
#'   \item 134 respondents, 5 covariates, no clustering: joint \code{p = 3.6e-12}
#'     against a classical \code{p = 0.07}.
#' }
#' In the first case the marginal robust standard errors were within one percent of
#' the classical ones, so nothing on the diagonal gave the problem away. The
#' reciprocal condition number of the covariance matrix was \code{1.1e-07} against
#' 0.24 to 0.86 in well-behaved fits, and the classical F on the same fit was 0.94
#' with \code{p = 0.58}.
#'
#' @section Why it happens, and what to do about the covariate set:
#' The cause is the \emph{robust} variance estimator, not collinearity. On the third
#' case above, the same fit gives:
#' \tabular{lrr}{
#'   \strong{statistic} \tab \strong{value} \tab \strong{p} \cr
#'   Wald with robust (HC2) V \tab 15.92 \tab 3.6e-12 \cr
#'   Wald with classical V \tab 2.06 \tab 0.074 \cr
#'   Classical F from residual sums of squares \tab 2.06 \tab 0.074
#' }
#' The coefficients are identical, both being OLS. The Wald \emph{form} is fine: fed
#' the classical variance matrix it reproduces the classical F exactly. What differs
#' is which variance matrix is inverted.
#'
#' The classical F needs one variance parameter, \eqn{\sigma^2}, estimated from
#' \eqn{n-k} residuals. A robust Wald test needs the whole \eqn{q \times q} matrix,
#' which is \eqn{q(q+1)/2} free parameters estimated from squared residuals and
#' leverage, and then inverted. Inverting a noisy estimate is not the same as
#' estimating the inverse, and the error does not average out: it inflates the
#' statistic. Counting observations per variance parameter puts every pathological
#' case in the same place:
#' \tabular{lrrr}{
#'   \strong{case} \tab \strong{n} \tab \strong{q} \tab \strong{n / (q(q+1)/2)} \cr
#'   1578 respondents, 40 covariates \tab 1578 \tab 40 \tab 1.9 \cr
#'   2937 respondents, 40 covariates \tab 2937 \tab 40 \tab 3.6 \cr
#'   134 respondents, 5 covariates \tab 134 \tab 5 \tab 8.9
#' }
#'
#' Two consequences follow, and the second is the practical one.
#'
#' First, the reciprocal condition number is the wrong diagnostic, which is why the
#' guard does not use one. \eqn{b'V^{-1}b} is invariant to rescaling a covariate
#' while \code{rcond(V)} is not: multiplying an age covariate by 100 leaves the
#' statistic at 15.92 and moves the condition number from \code{1.4e-05} to
#' \code{2.5e-03}. A condition number partly measures the spread of covariate
#' scales. Simulated designs with condition numbers as bad as \code{4e-06} return
#' perfectly sane p-values.
#'
#' Second, **this is a reason to pass fewer covariates rather than to patch the
#' test.** The joint test's battery should be the covariates you would actually
#' adjust for, each measured once, not everything the study recorded. A battery of 40
#' asks for 820 variance parameters. Concretely: resolve exact redundancies, which
#' \code{check_balance} now warns about by name; drop one of any pair of
#' parameterizations of the same measurement, such as a continuous age beside binned
#' age; and prefer \code{declaration} for a wide battery, since randomization
#' inference compares a statistic to its permutation distribution and estimates no
#' variance matrix at all. The same \eqn{n} relative to parameter count governs the
#' calibration results in the section below, so a smaller battery buys accuracy in
#' two ways at once.
#'
#' When the Wald p-value falls more than \code{max_orders_apart} orders of magnitude
#' below the classical one, the joint test reports \code{NA} with
#' \code{estimable = FALSE} and warns, naming both p-values and the condition
#' number. The default of 6 orders is far outside anything heteroskedasticity or
#' clustering produces legitimately, and it deliberately leaves alone the separate
#' case of a fit with genuinely few clusters, where the two p-values disagree by a
#' factor rather than by orders of magnitude and the remedy is
#' \code{declaration} rather than a smaller covariate set.
#'
#' Remedies, in order: look for redundant parameterizations and drop one; reduce the
#' covariate set; or pass \code{declaration}, since randomization inference compares
#' a statistic to its permutation distribution and inverts no matrix.
#'
#' @section How many observations per coefficient:
#' Calibration is governed by \eqn{N/q}, where \eqn{q} is the number of
#' coefficients the test estimates. Above roughly 30 observations per coefficient
#' the tests sit at nominal; below about 10 they are anti-conservative enough to
#' mislead. For the joint test \eqn{q = (K-1)p} with \eqn{p} the number of
#' model-matrix columns, so arms and covariates both inflate it. The
#' covariate-by-covariate tests escape this only when treatment is binary, which
#' makes each a single-degree-of-freedom test; with \eqn{K} arms each becomes a
#' \eqn{K-1} degree-of-freedom test and is subject to the same problem. See
#' \code{vignette("balance_testing")}.
#'
#' @seealso \code{\link{check_smd}} for the magnitude of each imbalance, which
#'   these p-values do not convey.
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
#' # Label the results and return one tibble, ready to stack across studies
#' check_balance(dat, Z, study_id = "smith_2024_study_1", flatten = TRUE)
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
#' @family per-study checks
#' @export
check_balance <- function(data, treatment, covariates = NULL, .method = estimatr::lm_robust,
                          declaration = NULL, sims = 1000, study_id = NULL,
                          flatten = FALSE, quiet = TRUE, max_orders_apart = 6,
                          .by = NULL, ...) {
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

  # Stratified run. Recurse with the covariate list already resolved to names, so
  # nothing needs re-selecting per stratum, and print once at the end rather than
  # once per stratum.
  by_quo <- rlang::enquo(.by)
  if (!rlang::quo_is_null(by_quo)) {
    # A grouping column is hidden from the stratum data, so it must not remain in
    # the covariate list either. It is constant within a stratum regardless.
    varnames <- setdiff(varnames, by_column_names(data, by_quo))
    if (length(varnames) == 0) {
      warning("No covariates left for balance check after removing .by columns.")
      return(invisible(NULL))
    }
    result <- run_by_strata(data, by_quo, function(d) {
      check_balance(d, !!treatment_name, covariates = varnames, .method = .method,
                    declaration = declaration, sims = sims, study_id = study_id,
                    flatten = flatten, quiet = TRUE,
                    max_orders_apart = max_orders_apart, ...)
    })
    if (!quiet) {
      print(result)
      return(invisible(result))
    }
    return(result)
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

  # Detect a clusters argument in the dots. The covariate-by-covariate tests are
  # well calibrated with cluster-robust standard errors, but the joint test is
  # not, and the only repair is randomization inference rather than a different
  # reference distribution. See the "Clustered designs" section.
  dots_call <- match.call(expand.dots = FALSE)$...
  has_clusters <- "clusters" %in% names(dots_call)
  if (has_clusters && is.null(declaration)) {
    warning("check_balance: the parametric joint test over-rejects under cluster ",
            "randomization (about 13 percent at a nominal 5 percent with 30 ",
            "clusters), and no degrees-of-freedom correction repairs it. Pass ",
            "declaration = for an exact randomization-inference p-value. The ",
            "covariate-by-covariate tests are unaffected.")
  }

  # --- Covariate-by-covariate tests ---
  results <- lapply(varnames, function(v) {
    col <- data[[v]]

    if (is.numeric(col)) {
      # Numeric covariate: regress X ~ Z, extract overall F-test
      form <- stats::as.formula(paste(paste0("`", v, "`"), "~", treatment_name))
      fit <- .method(form, data = data, ...)
      gl <- broom::glance(fit)
      tibble::tibble(
        covariate = v,
        level = NA_character_,
        F_stat = gl$statistic,
        statistic = "F",
        df1 = model_df1(fit),
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
          statistic = "F",
          df1 = model_df1(fit),
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

  # Aliasing is a property of the covariate set, not of the fit, so check it before
  # fitting and say which covariate to drop. Deliberately does not prune: choosing
  # between two redundant covariates is an analysis decision for the call site.
  warn_aliased_covariates(analysis_data, varnames, "check_balance")

  if (identical(declaration, "complete")) {
    # Shorthand for the most common case: complete random assignment with the
    # observed arm sizes taken as fixed. Equivalent to permuting the observed
    # treatment vector. Stated as an explicit declaration so the assumption is
    # visible rather than buried in a permutation loop.
    if (!requireNamespace("randomizr", quietly = TRUE)) {
      stop("declaration = 'complete' requires the randomizr package. ",
           "Install it with install.packages('randomizr').")
    }
    z_obs <- analysis_data[[treatment_name]]
    arm_counts <- table(z_obs)
    declaration <- randomizr::declare_ra(
      N          = length(z_obs),
      m_each     = as.vector(arm_counts),
      conditions = names(arm_counts)
    )
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

    # The joint test is a Wald test, so it inverts the whole covariance matrix
    # rather than reading its diagonal, and a near-singular matrix makes the
    # statistic meaningless rather than merely noisy. Cross-check against the
    # classical F on the same fit, which is inversion-free: the two answer the same
    # question and cannot legitimately differ by many orders of magnitude, so a
    # large gap means the inversion failed rather than that imbalance is real. See
    # the "When the joint test cannot be trusted" section.
    p_classical <- classical_f_pvalue(fit_joint, analysis_data[[".Z_numeric"]])
    orders_apart <- if (is.na(gl$p.value) || is.na(p_classical)) {
      NA_real_
    } else {
      log10(max(p_classical, .Machine$double.xmin)) -
        log10(max(gl$p.value, .Machine$double.xmin))
    }
    if (!is.na(orders_apart) && orders_apart > max_orders_apart) {
      warning("check_balance: joint test suppressed. Its Wald p-value (",
              format(gl$p.value, digits = 3), ") is ", round(orders_apart),
              " orders of magnitude smaller than the classical F p-value on the ",
              "same fit (", format(p_classical, digits = 3), "), which is more ",
              "than max_orders_apart = ", max_orders_apart, ". The Wald form ",
              "inverts the coefficient covariance matrix and the classical form ",
              "does not, so a gap this large means the inversion is dominated by ",
              "numerical noise (reciprocal condition number ",
              format(covariance_rcond(fit_joint), digits = 3),
              "), not that the covariates predict treatment. Usually ",
              "near-collinearity in a wide covariate battery: reduce the covariate ",
              "set, or pass declaration = for a randomization-inference p-value, ",
              "which inverts nothing.")
      joint_test <- tibble::tibble(
        F_stat = NA_real_,
        statistic = "F",
        df1 = model_df1(fit_joint),
        df2 = as.integer(gl$df.residual),
        p_value = NA_real_,
        nobs = as.integer(gl$nobs),
        estimable = FALSE
      )
    } else {
      joint_test <- tibble::tibble(
        F_stat = gl$statistic,
        statistic = "F",
        df1 = model_df1(fit_joint),
        df2 = as.integer(gl$df.residual),
        p_value = gl$p.value,
        nobs = as.integer(gl$nobs),
        estimable = !is.na(gl$p.value)
      )
    }
  } else {
    # Multi-armed treatment: multinomial likelihood-ratio test
    joint_test <- multinomial_lr_joint_test(
      data = analysis_data,
      treatment_name = treatment_name,
      covariate_cols = expanded_names
    )
  }

  if (!is.null(study_id)) {
    covariate_tests$study_id <- study_id
    joint_test$study_id <- study_id
  }

  if (flatten) {
    joint_row <- joint_test
    joint_row$covariate <- NA_character_
    joint_row$level <- NA_character_
    result <- dplyr::bind_rows(
      cbind(test = "covariate", covariate_tests),
      cbind(test = "joint", joint_row)
    )
  } else {
    result <- list(covariate_tests = covariate_tests, joint_test = joint_test)
  }

  if (!quiet) {
    if (flatten) {
      print(result)
    } else {
      cat("Covariate-by-covariate balance tests:\n")
      print(covariate_tests)
      cat("\nJoint balance test:\n")
      print(joint_test)
    }
    return(invisible(result))
  }

  result
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
#' @family code generators
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
