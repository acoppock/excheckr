#' Demean covariates following the Lin (2013) estimator
#'
#' Replicates the covariate demeaning procedure used in \code{estimatr::lm_lin}.
#' Expands factor/character variables to dummy indicators via \code{model.matrix},
#' then subtracts the full-sample column mean from each column.
#'
#' @param data A data frame.
#' @param covariates Character vector of covariate column names.
#'
#' @return A data frame of demeaned covariates with column names suffixed by \code{"_c"}.
#' @keywords internal
#' @noRd
#' @importFrom stats model.matrix model.frame as.formula
demean_covariates <- function(data, covariates) {
  covar_formula <- stats::as.formula(paste("~", paste(covariates, collapse = " + ")))
  covar_mat <- stats::model.matrix(
    covar_formula,
    data = stats::model.frame(covar_formula, data, na.action = na.pass)
  )
  # Drop intercept column (following lm_lin)
  covar_mat <- covar_mat[, -1, drop = FALSE]

  # Demean: subtract full-sample column means (following lm_lin)
  center <- colMeans(covar_mat, na.rm = TRUE)
  demeaned <- sweep(covar_mat, 2, center)

  # Add _c suffix to indicate centered
  colnames(demeaned) <- paste0(colnames(demeaned), "_c")

  as.data.frame(demeaned)
}


#' Auto-select variables by prefix
#'
#' Internal helper to select variables starting with a prefix and excluding
#' those ending with specified suffixes.
#'
#' @param data A data frame
#' @param prefix Character string prefix to match (default: "X_")
#' @param exclude_suffixes Character vector of suffixes to exclude (default: c("_nona", "_missing"))
#'
#' @return Character vector of variable names
#' @keywords internal
#' @noRd
auto_select_vars <- function(data, prefix = "X_", exclude_suffixes = c("_nona", "_missing")) {
  varnames <- names(data)
  pattern <- paste0("(", paste(exclude_suffixes, collapse = "|"), ")$")
  varnames[startsWith(varnames, prefix) & !grepl(pattern, varnames)]
}


#' Statistical mode
#'
#' Computes the most frequent (modal) value of a vector.
#'
#' @param x A vector (numeric, character, or factor).
#' @param na.rm Logical. Should missing values be removed before computing
#'   the mode? Defaults to TRUE.
#'
#' @return
#' The most frequent value of `x`.
#' If `x` is a factor, the result is returned as a factor with the same levels.
#' If there are ties, the first occurring mode is returned.
#' If all values are missing, returns `NA`.
#'
#' @examples
#' stat_mode(c(1, 2, 2, 3, NA))
#' stat_mode(c("a", "b", "a", "c", "c"))
#' stat_mode(factor(c("low", "high", "low", NA)))
#'
#' @export
stat_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) return(NA)

  # Handle NA explicitly if na.rm = FALSE
  if (!na.rm && any(is.na(x))) {
    na_count <- sum(is.na(x))
    tab <- table(x, useNA = "ifany")
    max_count <- max(tab)
    modes <- names(tab)[tab == max_count]

    # Return NA if NA is among the modes
    if ("NA" %in% modes) return(NA_real_)

    # Otherwise return first mode
    return(as.numeric(modes[1]))
  }

  # normal case
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  mode_val <- ux[which.max(tab)]

  # preserve factor type
  if (is.factor(x)) {
    return(factor(mode_val, levels = levels(x)))
  }
  mode_val
}


#' Multinomial likelihood-ratio joint balance test
#'
#' Fits a multinomial logistic regression of treatment on covariates via
#' \code{nnet::multinom}, compares to a null (intercept-only) model using a
#' likelihood-ratio test, and returns a chi-squared p-value.
#'
#' @param data A data frame.
#' @param treatment_name Character. Name of the treatment column (must be a factor).
#' @param covariate_cols Character vector of expanded covariate column names.
#'
#' @return A tibble with columns F_stat (LR/q), df1 (q), df2 (NA), p_value, nobs.
#' @keywords internal
#' @noRd
#' @importFrom stats as.formula logLik pchisq complete.cases
#' @importFrom tibble tibble
multinomial_lr_joint_test <- function(data, treatment_name, covariate_cols) {
  # Restrict to complete cases
  keep_cols <- c(treatment_name, covariate_cols)
  cc <- stats::complete.cases(data[, keep_cols, drop = FALSE])
  data <- data[cc, , drop = FALSE]
  n <- nrow(data)

  z_factor <- data[[treatment_name]]
  if (!is.factor(z_factor)) z_factor <- as.factor(z_factor)
  K <- length(levels(z_factor))
  p_covar <- length(covariate_cols)
  q <- (K - 1) * p_covar

  na_result <- tibble::tibble(
    F_stat  = NA_real_,
    df1     = NA_integer_,
    df2     = NA_integer_,
    p_value = NA_real_,
    nobs    = as.integer(n)
  )

  # Check for empty treatment groups after complete-case filtering
  level_counts <- table(z_factor)
  empty_groups <- names(level_counts)[level_counts == 0]
  if (length(empty_groups) > 0) {
    warning(paste(
      "Joint balance test not estimable: treatment group(s)",
      paste(empty_groups, collapse = ", "),
      "empty after removing incomplete cases."
    ))
    return(na_result)
  }

  # Check for zero-variance covariates
  for (cname in covariate_cols) {
    if (length(unique(data[[cname]])) <= 1) {
      warning("Joint balance test not estimable: constant covariate within group.")
      return(na_result)
    }
  }

  bt_cols <- paste0("`", covariate_cols, "`")
  full_formula <- stats::as.formula(
    paste(treatment_name, "~", paste(bt_cols, collapse = " + "))
  )
  null_formula <- stats::as.formula(paste(treatment_name, "~ 1"))

  fit_full <- tryCatch(
    suppressWarnings(nnet::multinom(full_formula, data = data, trace = FALSE)),
    error = function(e) NULL
  )
  if (is.null(fit_full)) {
    warning("Joint balance test not estimable: multinomial model failed to converge.")
    return(na_result)
  }

  fit_null <- suppressWarnings(nnet::multinom(null_formula, data = data, trace = FALSE))

  lr_stat <- as.numeric(-2 * (stats::logLik(fit_null) - stats::logLik(fit_full)))
  p_value <- stats::pchisq(lr_stat, df = q, lower.tail = FALSE)

  tibble::tibble(
    F_stat  = lr_stat / q,
    df1     = as.integer(q),
    df2     = NA_integer_,
    p_value = p_value,
    nobs    = as.integer(n)
  )
}


#' Randomization inference joint balance test
#'
#' Computes a multinomial likelihood-ratio test statistic and obtains a p-value
#' via randomization inference using \code{ri2::conduct_ri}.
#'
#' @param data A data frame.
#' @param treatment_name Character. Name of the treatment column.
#' @param covariate_cols Character vector of expanded covariate column names.
#' @param declaration A \code{randomizr} declaration object.
#' @param sims Integer. Number of RI simulations.
#'
#' @return A tibble with columns F_stat (observed LR), df1 (NA), df2 (NA), p_value, nobs.
#' @keywords internal
#' @noRd
#' @importFrom stats as.formula logLik
#' @importFrom tibble tibble
ri_joint_test <- function(data, treatment_name, covariate_cols, declaration, sims = 1000) {
  if (!requireNamespace("ri2", quietly = TRUE)) {
    stop("Package 'ri2' is required for randomization inference. Install it with install.packages('ri2').")
  }

  n <- nrow(data)

  bt_cols <- paste0("`", covariate_cols, "`")
  full_formula <- stats::as.formula(
    paste(treatment_name, "~", paste(bt_cols, collapse = " + "))
  )
  null_formula <- stats::as.formula(paste(treatment_name, "~ 1"))

  # Test function: multinomial LR statistic
  test_function <- function(data) {
    fit_full <- suppressMessages(
      nnet::multinom(full_formula, data = data, trace = FALSE)
    )
    fit_null <- suppressMessages(
      nnet::multinom(null_formula, data = data, trace = FALSE)
    )
    as.numeric(-2 * (stats::logLik(fit_null) - stats::logLik(fit_full)))
  }

  ri_out <- ri2::conduct_ri(
    test_function = test_function,
    declaration = declaration,
    assignment = treatment_name,
    data = data,
    sims = sims,
    p = "upper"
  )

  ri_summary <- summary(ri_out)
  p_value <- ri_summary$upper_p_value[1]

  # Compute observed statistic
  obs_stat <- test_function(data)

  tibble::tibble(
    F_stat  = obs_stat,
    df1     = NA_integer_,
    df2     = NA_integer_,
    p_value = p_value,
    nobs    = as.integer(n)
  )
}


#' Cross-equation Wald test for multi-armed balance
#'
#' Fits K-1 linear probability models (one per non-reference treatment level)
#' regressing each treatment dummy on covariates, then computes a joint Wald
#' F-test using a cross-equation cluster-robust sandwich estimator.
#'
#' @param data A data frame.
#' @param treatment_name Character. Name of the treatment column (must be a factor).
#' @param covariate_cols Character vector of expanded covariate column names.
#' @param cluster_col Character or NULL. Name of the cluster column.
#' @param .method Regression function (default: estimatr::lm_robust).
#' @param ... Additional arguments passed to .method.
#'
#' @return A tibble with columns F_stat, df1, df2, p_value, nobs.
#' @keywords internal
#' @noRd
#' @importFrom stats complete.cases model.matrix fitted.values pf
#' @importFrom tibble tibble
cross_equation_wald_test <- function(data, treatment_name, covariate_cols,
                                     cluster_col = NULL, .method = estimatr::lm_robust, ...) {
  z_factor <- data[[treatment_name]]
  if (!is.factor(z_factor)) z_factor <- as.factor(z_factor)
  z_levels <- levels(z_factor)
  K <- length(z_levels)

  # Create K-1 treatment dummies (excluding reference level)
  dummy_names <- character(K - 1)
  for (k in seq_len(K - 1)) {
    dname <- paste0(".Z_dum_", k)
    data[[dname]] <- as.integer(z_factor == z_levels[k + 1])
    dummy_names[k] <- dname
  }

  # Restrict to complete cases on covariates + treatment + clusters
  keep_cols <- c(dummy_names, covariate_cols)
  if (!is.null(cluster_col)) keep_cols <- c(keep_cols, cluster_col)
  cc <- stats::complete.cases(data[, keep_cols, drop = FALSE])
  data <- data[cc, , drop = FALSE]
  n <- nrow(data)

  # Build covariate model matrix (with intercept)
  bt_cols <- paste0("`", covariate_cols, "`")
  covar_formula <- stats::as.formula(paste("~", paste(bt_cols, collapse = " + ")))
  X <- stats::model.matrix(covar_formula, data = data)
  p <- ncol(X)  # includes intercept

  # Number of covariate parameters (excluding intercept) per equation
  p_covar <- p - 1

  # Fit K-1 regressions and collect residuals
  fits <- vector("list", K - 1)
  residual_mat <- matrix(NA_real_, nrow = n, ncol = K - 1)

  for (k in seq_len(K - 1)) {
    form <- stats::as.formula(paste(dummy_names[k], "~", paste(bt_cols, collapse = " + ")))
    fits[[k]] <- .method(form, data = data, ...)
    residual_mat[, k] <- data[[dummy_names[k]]] - stats::fitted.values(fits[[k]])
  }

  # Bread matrix for each equation: solve(X'X)
  na_result <- tibble::tibble(
    F_stat  = NA_real_,
    df1     = NA_integer_,
    df2     = NA_integer_,
    p_value = NA_real_,
    nobs    = as.integer(n)
  )
  XtX_inv <- tryCatch(
    solve(crossprod(X)),
    error = function(e) NULL
  )
  if (is.null(XtX_inv)) {
    warning("Joint balance test not estimable: singular covariate matrix (constant covariate within group).")
    return(na_result)
  }

  # Build stacked sandwich variance-covariance matrix
  # Stack order: covariate coefficients only (exclude intercept) across K-1 equations
  # Total dimension: (K-1)*p_covar x (K-1)*p_covar
  q <- (K - 1) * p_covar
  V_full <- matrix(0, nrow = q, ncol = q)

  # Indices for covariate params (rows 2:p of each equation's coefficient vector)
  covar_idx <- 2:p

  for (j in seq_len(K - 1)) {
    for (k in seq_len(K - 1)) {
      # Meat for equation pair (j, k)
      if (is.null(cluster_col)) {
        meat_jk <- crossprod(X * residual_mat[, j], X * residual_mat[, k])
        # HC1 correction: n/(n-p)
        correction <- n / (n - p)
      } else {
        clusters <- data[[cluster_col]]
        G <- length(unique(clusters))
        Xej <- rowsum(X * residual_mat[, j], clusters)
        Xek <- rowsum(X * residual_mat[, k], clusters)
        meat_jk <- crossprod(Xej, Xek)
        # CR1 correction
        correction <- (G / (G - 1)) * ((n - 1) / (n - p))
      }

      # Sandwich for this block: bread %*% meat %*% bread * correction
      sand_jk <- XtX_inv %*% meat_jk %*% XtX_inv * correction

      # Extract covariate-only submatrix
      sand_jk_covar <- sand_jk[covar_idx, covar_idx, drop = FALSE]

      # Place into stacked V
      row_start <- (j - 1) * p_covar + 1
      row_end <- j * p_covar
      col_start <- (k - 1) * p_covar + 1
      col_end <- k * p_covar
      V_full[row_start:row_end, col_start:col_end] <- sand_jk_covar
    }
  }

  # Stack covariate coefficients (exclude intercept) across equations
  beta_stacked <- numeric(q)
  for (k in seq_len(K - 1)) {
    coefs <- stats::coef(fits[[k]])
    beta_stacked[((k - 1) * p_covar + 1):(k * p_covar)] <- coefs[covar_idx]
  }

  # Wald F-test
  W <- tryCatch(
    as.numeric(t(beta_stacked) %*% solve(V_full) %*% beta_stacked),
    error = function(e) NULL
  )
  if (is.null(W)) {
    warning("Joint balance test not estimable: singular sandwich matrix.")
    return(na_result)
  }
  F_stat <- W / q

  # Degrees of freedom
  if (is.null(cluster_col)) {
    df2 <- n - p
  } else {
    df2 <- G - 1
  }
  p_value <- stats::pf(F_stat, q, df2, lower.tail = FALSE)

  tibble::tibble(
    F_stat = F_stat,
    df1 = as.integer(q),
    df2 = as.integer(df2),
    p_value = p_value,
    nobs = as.integer(n)
  )
}