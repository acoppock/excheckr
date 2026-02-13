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
  XtX_inv <- solve(crossprod(X))

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
  W <- as.numeric(t(beta_stacked) %*% solve(V_full) %*% beta_stacked)
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