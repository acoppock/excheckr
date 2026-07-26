utils::globalVariables(c(
  "var1", "var2", "var1_f", "var2_f", "fraction_missing",
  "treatment_sd", "control_sd"
))

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
    data = stats::model.frame(covar_formula, data, na.action = stats::na.pass)
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
#' For factor input, the return value is a length-1 factor with the same levels,
#' which is directly compatible with \code{tidyr::replace_na()} for
#' mode-imputation of factor columns.
#'
#' @param x A vector (numeric, character, or factor).
#' @param na.rm Logical. Should missing values be removed before computing
#'   the mode? Defaults to TRUE.
#'
#' @return
#' The most frequent value of `x`.
#' If `x` is a factor, the result is returned as a single-element factor with
#' the same levels, compatible with \code{tidyr::replace_na()}.
#' If there are ties, the first occurring mode is returned.
#' If all values are missing, returns \code{NA}.
#'
#' @examples
#' stat_mode(c(1, 2, 2, 3, NA))
#' stat_mode(c("a", "b", "a", "c", "c"))
#' stat_mode(factor(c("low", "high", "low", NA)))
#'
#' # For factor columns, stat_mode returns a length-1 factor with the same levels.
#' # The result is directly compatible with tidyr::replace_na() -- no as.character()
#' # conversion needed:
#' #   replace_na(x, stat_mode(x))
#' # The base-R equivalent works without any extra packages:
#' x <- factor(c("low", "high", "low", NA), levels = c("low", "high"))
#' x[is.na(x)] <- stat_mode(x)
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
    F_stat    = NA_real_,
    statistic = "LR/df",
    df1       = NA_integer_,
    df2       = NA_integer_,
    p_value   = NA_real_,
    nobs      = as.integer(n)
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
    F_stat    = lr_stat / q,
    statistic = "LR/df",
    df1       = as.integer(q),
    df2       = NA_integer_,
    p_value   = p_value,
    nobs      = as.integer(n)
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
    F_stat    = obs_stat,
    statistic = "LR",
    df1       = NA_integer_,
    df2       = NA_integer_,
    p_value   = p_value,
    nobs      = as.integer(n)
  )
}
