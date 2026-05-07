#' Check differential attrition using double-LASSO covariate selection
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' An extension of [check_attrition()] that addresses the events-per-variable
#' (EPV) problem endemic to the fully-interacted Lin-style attrition model.
#' When the full covariate pool is used in the interacted model, df1 grows as
#' (n_arms - 1) x (1 + n_covariates), which quickly produces EPV < 10 for
#' studies with low overall missingness or many arms. Below EPV = 10, the
#' model is unreliable in both directions: too sparse to detect real effects
#' (underpowered) and capable of spurious significance from numerical
#' instability (inflated type I error).
#'
#' This function uses a double-LASSO procedure to select a parsimonious
#' covariate set before running the interacted test:
#'
#' * **LASSO 1**: regress Y on all candidate covariates (among completers).
#'   Selects covariates that predict the outcome -- the variables where
#'   differential dropout would directly bias the treatment effect estimate.
#' * **LASSO 2**: regress I(Y missing) on all candidate covariates (among
#'   assigned respondents). Selects covariates that characterize who drops out.
#' * **Union** of both selected sets enters the interacted model.
#'
#' The simple test (no covariates) is always computed and is the primary
#' criterion for detecting arm-level differential dropout. The interacted test
#' is secondary: it detects subgroup-specific dropout patterns that the simple
#' test misses even when overall rates are equal. Both are returned; the
#' caller decides which to act on.
#'
#' @section Experimental:
#' This function implements an original procedure motivated by EPV concerns.
#' It is not a reference implementation from a published paper. The double-LASSO
#' covariate selection step is adapted from Belloni, Chernozhukov, and Hansen
#' (2014) but applied to an attrition diagnostic rather than a treatment effect
#' estimator. The EPV threshold of 10 follows the rule of thumb from Peduzzi
#' et al. (1996) for logistic regression; the same concern applies to linear
#' probability models. Use with awareness that this procedure is novel.
#'
#' @param data A data frame or tibble. Should contain only assigned respondents
#'   (rows with `NA` treatment are silently dropped before all calculations).
#' @param treatment Unquoted name of the treatment variable.
#' @param outcomes Character vector of outcome variable names. If `NULL`, all
#'   columns beginning with `"Y_"` are used.
#' @param covariates Character vector of candidate covariate names (the full
#'   LASSO candidate pool, typically the `nona_covariates` vector from the
#'   study's checking script). If `NULL` or empty, only the simple test is run.
#' @param epv_threshold Minimum events-per-variable required to run the
#'   interacted test. Default `10`. Below this threshold `p_interacted` is
#'   returned as `NA` and `epv_adequate` is `FALSE`.
#' @param lasso_se `"lambda.1se"` (default, conservative -- fewer covariates,
#'   better EPV) or `"lambda.min"` (more covariates, lower in-sample error).
#' @param .method Regression function for the interacted test (default:
#'   `estimatr::lm_robust`). Must accept formula and data arguments.
#' @param quiet Logical. Suppress console output (default `TRUE`).
#' @param ... Additional arguments passed to `.method`.
#'
#' @return A tibble with one row per outcome and the following columns:
#'   \describe{
#'     \item{outcome}{Outcome variable name.}
#'     \item{n_assigned}{Number of respondents with non-missing treatment.}
#'     \item{n_missing}{Number of missing outcome observations.}
#'     \item{pct_missing}{Proportion missing.}
#'     \item{p_simple}{P-value from simple F-test: `I(missing) ~ Z`.}
#'     \item{n_lasso1}{Covariates selected by LASSO 1 (Y ~ X on completers).}
#'     \item{n_lasso2}{Covariates selected by LASSO 2 (I(missing) ~ X).}
#'     \item{n_selected}{Size of union of LASSO 1 and LASSO 2 selections.}
#'     \item{selected_covariates}{Comma-separated names of selected covariates.}
#'     \item{df1}{Degrees of freedom for the interacted test numerator:
#'       `(n_arms - 1) * (1 + n_selected)`.}
#'     \item{epv}{Events per variable: `n_missing / df1`.}
#'     \item{epv_adequate}{Logical: `epv >= epv_threshold` and `n_selected > 0`.}
#'     \item{p_interacted}{P-value from interacted F-test:
#'       `I(missing) ~ Z * (demeaned selected covariates)`, testing all Z terms
#'       jointly. `NA` when `epv_adequate` is `FALSE` or no covariates selected.}
#'     \item{flag_simple}{`p_simple < 0.05`.}
#'     \item{flag_interacted}{`epv_adequate & p_interacted < 0.05`.}
#'     \item{flag}{`flag_simple | flag_interacted`.}
#'   }
#'
#' @references
#' Belloni, A., Chernozhukov, V., and Hansen, C. (2014). Inference on treatment
#' effects after selection among high-dimensional controls. *Review of Economic
#' Studies*, 81(2), 608-650.
#'
#' Peduzzi, P., Concato, J., Kemper, E., Holford, T.R., and Feinstein, A.R.
#' (1996). A simulation study of the number of events per variable in logistic
#' regression analysis. *Journal of Clinical Epidemiology*, 49(12), 1373-1379.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 500
#' dat <- data.frame(
#'   Z       = rbinom(n, 1, 0.5),
#'   X_age   = rnorm(n, 50, 10),
#'   X_income = rnorm(n, 50000, 10000)
#' )
#' dat$Y_outcome <- 0.3 * dat$Z + 0.5 * scale(dat$X_age) + rnorm(n)
#' # Differential attrition correlated with age in treatment arm
#' p_miss <- ifelse(dat$Z == 1, 0.05 + 0.01 * scale(dat$X_age), 0.05)
#' dat$Y_outcome[rbinom(n, 1, pmax(0, pmin(1, p_miss))) == 1] <- NA
#'
#' check_attrition_lasso(dat, Z,
#'   outcomes   = "Y_outcome",
#'   covariates = c("X_age", "X_income"))
#' }
#'
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom rlang ensym as_name
#' @importFrom tibble tibble
#' @importFrom stats as.formula coef vcov df.residual pf
#' @export
check_attrition_lasso <- function(
    data,
    treatment,
    outcomes      = NULL,
    covariates    = NULL,
    epv_threshold = 10,
    lasso_se      = c("lambda.1se", "lambda.min"),
    .method       = estimatr::lm_robust,
    quiet         = TRUE,
    ...
) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop(
      "Package 'glmnet' is required for check_attrition_lasso(). ",
      "Install with: install.packages('glmnet')"
    )
  }

  lasso_se       <- match.arg(lasso_se)
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  # Resolve outcome names
  if (is.null(outcomes)) {
    y_cols <- auto_select_vars(data, prefix = "Y")
  } else {
    y_cols <- outcomes
  }
  if (length(y_cols) == 0) {
    warning("No outcomes selected.")
    return(invisible(NULL))
  }

  # Drop unassigned respondents once
  data <- data[!is.na(data[[treatment_name]]), , drop = FALSE]

  # Silently drop any covariate columns that don't exist in the data
  cov_cols <- intersect(covariates, names(data))
  dropped  <- setdiff(covariates, names(data))
  if (length(dropped) > 0) {
    warning("check_attrition_lasso: dropping covariates not found in data: ",
            paste(dropped, collapse = ", "))
  }
  has_covs    <- length(cov_cols) > 0
  n_arms      <- length(unique(data[[treatment_name]]))

  results <- lapply(y_cols, function(yc) {
    miss       <- as.integer(is.na(data[[yc]]))
    n_assigned <- nrow(data)
    n_missing  <- sum(miss)
    pct_miss   <- mean(miss)

    # ------------------------------------------------------------------ #
    # Simple test: I(missing) ~ Z                                        #
    # ------------------------------------------------------------------ #
    if (n_missing == 0L) {
      return(tibble::tibble(
        outcome             = yc,
        n_assigned          = n_assigned,
        n_missing           = 0L,
        pct_missing         = 0,
        p_simple            = 1,
        n_lasso1            = 0L,
        n_lasso2            = 0L,
        n_selected          = 0L,
        selected_covariates = "",
        df1                 = NA_integer_,
        epv                 = NA_real_,
        epv_adequate        = FALSE,
        p_interacted        = NA_real_,
        flag_simple         = FALSE,
        flag_interacted     = FALSE,
        flag                = FALSE
      ))
    }

    simple_form <- stats::as.formula(paste("miss ~", treatment_name))
    simple_df   <- cbind(data.frame(miss = miss), data[, treatment_name, drop = FALSE])
    simple_fit  <- .method(simple_form, data = simple_df, ...)
    simple_gl   <- broom::glance(simple_fit)
    p_simple    <- simple_gl$p.value

    # ------------------------------------------------------------------ #
    # LASSO covariate selection                                          #
    # ------------------------------------------------------------------ #
    lasso1_sel <- character(0)
    lasso2_sel <- character(0)

    if (has_covs && n_missing > 0) {
      X_mat <- as.matrix(data[, cov_cols, drop = FALSE])

      # LASSO 1: Y ~ X on completers (selects outcome-predictive covariates)
      completers <- !is.na(data[[yc]])
      if (sum(completers) > max(10, ncol(X_mat))) {
        cv1 <- tryCatch(
          glmnet::cv.glmnet(X_mat[completers, , drop = FALSE],
                            data[[yc]][completers],
                            alpha  = 1,
                            family = "gaussian",
                            nfolds = min(10, sum(completers))),
          error = function(e) NULL
        )
        if (!is.null(cv1)) {
          b1         <- glmnet::coef.glmnet(cv1, s = lasso_se)
          lasso1_sel <- rownames(b1)[b1[, 1] != 0 & rownames(b1) != "(Intercept)"]
          lasso1_sel <- intersect(lasso1_sel, cov_cols)
        }
      }

      # LASSO 2: I(missing) ~ X on all assigned (selects dropout-predictive covariates)
      # Use gaussian (LPM) for consistency with the interacted test and to avoid
      # convergence issues at very low event rates (1-5% missing)
      if (n_missing >= 5) {
        cv2 <- tryCatch(
          glmnet::cv.glmnet(X_mat, miss,
                            alpha  = 1,
                            family = "gaussian",
                            nfolds = min(10, n_assigned)),
          error = function(e) NULL
        )
        if (!is.null(cv2)) {
          b2         <- glmnet::coef.glmnet(cv2, s = lasso_se)
          lasso2_sel <- rownames(b2)[b2[, 1] != 0 & rownames(b2) != "(Intercept)"]
          lasso2_sel <- intersect(lasso2_sel, cov_cols)
        }
      }
    }

    selected <- union(lasso1_sel, lasso2_sel)
    n_sel    <- length(selected)

    # ------------------------------------------------------------------ #
    # EPV check                                                          #
    # ------------------------------------------------------------------ #
    df1         <- if (n_sel > 0) (n_arms - 1L) * (1L + n_sel) else NA_integer_
    epv         <- if (!is.na(df1) && df1 > 0) n_missing / df1 else NA_real_
    epv_ok      <- !is.na(epv) && epv >= epv_threshold && n_sel > 0

    # ------------------------------------------------------------------ #
    # Interacted test (only when EPV adequate)                           #
    # ------------------------------------------------------------------ #
    p_interacted <- NA_real_

    if (epv_ok) {
      # Demean selected covariates over assigned rows
      X_sel  <- data[, selected, drop = FALSE]
      X_dem  <- scale(X_sel, center = TRUE, scale = FALSE)
      colnames(X_dem) <- paste0(selected, "_dm")

      fit_df <- cbind(
        data.frame(miss = miss),
        data[, treatment_name, drop = FALSE],
        as.data.frame(X_dem)
      )

      dm_names  <- colnames(X_dem)
      covar_rhs <- paste(paste0("`", dm_names, "`"), collapse = " + ")
      fmla      <- stats::as.formula(
        paste("miss ~", treatment_name, "* (", covar_rhs, ")")
      )

      fit <- tryCatch(.method(fmla, data = fit_df, ...), error = function(e) NULL)

      if (!is.null(fit)) {
        all_terms  <- names(stats::coef(fit))
        test_terms <- all_terms[
          startsWith(all_terms, treatment_name) |
          grepl(paste0(":", treatment_name), all_terms)
        ]
        b  <- stats::coef(fit)[test_terms]
        V  <- stats::vcov(fit)[test_terms, test_terms]
        q  <- length(test_terms)
        df2 <- stats::df.residual(fit)
        W  <- tryCatch(
          as.numeric(t(b) %*% solve(V) %*% b),
          error = function(e) NULL
        )
        if (!is.null(W)) {
          p_interacted <- stats::pf(W / q, q, df2, lower.tail = FALSE)
        }
      }
    }

    tibble::tibble(
      outcome              = yc,
      n_assigned           = n_assigned,
      n_missing            = n_missing,
      pct_missing          = pct_miss,
      p_simple             = p_simple,
      n_lasso1             = length(lasso1_sel),
      n_lasso2             = length(lasso2_sel),
      n_selected           = n_sel,
      selected_covariates  = paste(selected, collapse = ", "),
      df1                  = df1,
      epv                  = epv,
      epv_adequate         = epv_ok,
      p_interacted         = p_interacted,
      flag_simple          = !is.na(p_simple)      && p_simple      < 0.05,
      flag_interacted      = epv_ok && !is.na(p_interacted) && p_interacted < 0.05,
      flag                 = (!is.na(p_simple) && p_simple < 0.05) |
                             (epv_ok && !is.na(p_interacted) && p_interacted < 0.05)
    )
  })

  out <- dplyr::bind_rows(results)
  if (!quiet) print(out)
  invisible(out)
}
