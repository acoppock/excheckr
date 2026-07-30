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


#' Weighted mean and standard deviation over a row subset
#'
#' Used by \code{check_smd} so that standardized mean differences can be computed
#' on the same weighted basis as the balance tests they sit beside. With unit
#' weights these reduce to \code{mean} and \code{sd}: the variance denominator is
#' \code{sum(w) - 1}, which equals \code{n - 1} when every weight is 1.
#'
#' @param x Numeric vector.
#' @param w Numeric weights, the same length as \code{x}.
#' @param rows Logical index selecting the rows to use.
#' @return A length-1 numeric, \code{NA} when fewer than the required number of
#'   non-missing observations remain.
#' @keywords internal
#' @noRd
weighted_mean <- function(x, w, rows) {
  ok <- rows & !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

#' @rdname weighted_mean
#' @noRd
weighted_sd <- function(x, w, rows) {
  ok <- rows & !is.na(x) & !is.na(w)
  if (sum(ok) < 2L) return(NA_real_)
  mu <- sum(x[ok] * w[ok]) / sum(w[ok])
  denom <- sum(w[ok]) - 1
  if (denom <= 0) return(NA_real_)
  sqrt(sum(w[ok] * (x[ok] - mu)^2) / denom)
}


#' Warn when covariates are aliased, and say which one to drop
#'
#' A covariate that is a linear function of the others carries no separate
#' information, so the joint test cannot estimate a coefficient for it. Base
#' \code{lm()} drops the redundant column silently and reports an F on the
#' remainder, which is how balance p-values have been computed without anyone
#' noticing a battery was redundant; \code{estimatr::lm_robust} returns \code{NA}
#' instead, which is louder but kills the test with no explanation.
#'
#' This warns and names the covariate to remove. It deliberately does \strong{not}
#' prune, because which of two redundant covariates to keep is an analysis decision
#' that belongs in the script where a reader can see it, not inside a check. Where
#' the redundancy is exact, the partner is identified so the choice is informed.
#'
#' @param data A data frame.
#' @param varnames Character vector of covariate names.
#' @param fn_name Calling function, for the message.
#' @return \code{TRUE} when aliasing was found, invisibly.
#' @keywords internal
#' @noRd
warn_aliased_covariates <- function(data, varnames, fn_name = "check_balance") {
  if (length(varnames) < 2) return(invisible(FALSE))
  form <- stats::reformulate(paste0("`", varnames, "`"))
  mm <- tryCatch(
    stats::model.matrix(form, data = stats::model.frame(form, data, na.action = stats::na.omit)),
    error = function(e) NULL
  )
  if (is.null(mm) || ncol(mm) < 3) return(invisible(FALSE))

  qr_mm <- qr(mm)
  if (qr_mm$rank == ncol(mm)) return(invisible(FALSE))

  # Map model-matrix columns to source covariates via the `assign` attribute,
  # which records it exactly; a factor expands to <covariate><level> with no
  # separator, so any name-prefix test would guess.
  keep_cols <- colnames(mm)[qr_mm$pivot[seq_len(qr_mm$rank)]]
  drop_cols <- colnames(mm)[qr_mm$pivot[-seq_len(qr_mm$rank)]]
  mm_assign <- attr(mm, "assign")
  mm_terms <- c("(Intercept)", attr(stats::terms(form, data = data), "term.labels"))
  col_source <- gsub("`", "", mm_terms[mm_assign + 1L], fixed = TRUE)
  names(col_source) <- colnames(mm)

  aliased <- unique(col_source[drop_cols])
  aliased <- aliased[aliased %in% varnames]
  if (length(aliased) == 0) return(invisible(FALSE))

  # Name the partner where the redundancy is exact: regress each dropped column on
  # the retained ones and report the covariates that determine it.
  partners <- lapply(drop_cols, function(dc) {
    others <- setdiff(keep_cols, "(Intercept)")
    if (length(others) == 0) return(character(0))
    fit <- tryCatch(stats::lm.fit(cbind(1, mm[, others, drop = FALSE]), mm[, dc]),
                    error = function(e) NULL)
    if (is.null(fit)) return(character(0))
    if (stats::var(fit$residuals) > 1e-16 * max(1, stats::var(mm[, dc]))) return(character(0))
    b <- fit$coefficients[-1]
    names(b) <- others
    unique(col_source[others[!is.na(b) & abs(b) > 1e-8]])
  })
  names(partners) <- drop_cols

  lines <- vapply(aliased, function(a) {
    cols <- drop_cols[col_source[drop_cols] == a]
    p <- unique(unlist(partners[cols]))
    p <- setdiff(p, a)
    if (length(p)) {
      paste0("    ", a, " is exactly determined by ", paste(p, collapse = " + "))
    } else {
      paste0("    ", a, " is a linear combination of the others")
    }
  }, character(1))

  keep_vars <- setdiff(varnames, aliased)
  warning(fn_name, ": the covariate set is rank deficient, so the joint test cannot ",
          "be estimated (", ncol(mm), " columns, rank ", qr_mm$rank, ").\n",
          paste(lines, collapse = "\n"), "\n",
          "  Choose which to keep at the call site rather than letting the test drop one:\n",
          "       covariates = c(", paste0('"', keep_vars, '"', collapse = ", "), ")\n",
          "  The covariate-by-covariate tests are unaffected and are all reported.",
          call. = FALSE)
  invisible(TRUE)
}


#' Resolve a \code{.by} expression to column names
#'
#' Callers need these before recursing, because a variable list resolved on the
#' full data can include a grouping column, while the stratum data has the
#' grouping columns hidden. Asking for a covariate that is no longer there is an
#' error, and \code{X_pid_3} used as a stratifier is exactly that case: it matches
#' the \code{"X_"} prefix and so is auto-selected as a covariate.
#'
#' @param data A data frame.
#' @param by_quo A quosure holding the \code{.by} tidyselect expression.
#' @return Character vector of column names.
#' @keywords internal
#' @noRd
by_column_names <- function(data, by_quo) {
  names(tidyselect::eval_select(by_quo, data))
}


#' Run a check separately within each stratum and stack the results
#'
#' Backs the \code{.by} argument of the checks that involve treatment. A check run
#' on a whole dataset when treatment was assigned within strata answers the wrong
#' question, so every real project has been writing the same
#' \code{nest} / \code{map} / \code{unnest} plumbing by hand and then bolting
#' \code{study_id} on afterwards, because the argument cannot survive the
#' \code{map}. This does that once.
#'
#' The return shape is whatever the check itself returns, with the grouping columns
#' prepended: a tibble stays a tibble, and a named list of tibbles stays a named
#' list of tibbles bound element-wise. That keeps \code{.by} orthogonal to
#' \code{flatten} and to the two shapes \code{check_attrition} can return.
#'
#' @param data A data frame.
#' @param by_quo A quosure holding the \code{.by} tidyselect expression.
#' @param fn A function of one argument, the stratum's data.
#' @return The stacked result, shaped like a single \code{fn()} call.
#' @keywords internal
#' @noRd
run_by_strata <- function(data, by_quo, fn) {
  by_cols <- names(tidyselect::eval_select(by_quo, data))
  if (length(by_cols) == 0) {
    stop("check .by: no grouping columns selected.", call. = FALSE)
  }

  keys <- data[, by_cols, drop = FALSE]
  # Group in order of first appearance, and keep NA as its own stratum rather than
  # dropping those rows, matching tidyr::nest(.by = ).
  key_str <- do.call(paste, c(lapply(keys, function(x) as.character(x)), sep = "\r"))

  # Hide the grouping columns from the check, exactly as tidyr::nest(.by = ) does.
  # They are constant within a stratum, so leaving them visible would let one be
  # picked up by prefix-based auto-selection and tested as a covariate with no
  # variation. X_pid_3 as a stratifier is precisely that case.
  stratum_data <- data[, setdiff(names(data), by_cols), drop = FALSE]

  pieces <- lapply(unique(key_str), function(k) {
    rows <- which(key_str == k)
    res <- fn(stratum_data[rows, , drop = FALSE])
    if (is.null(res)) return(NULL)
    prepend_stratum_keys(res, keys[rows[1], , drop = FALSE])
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0) return(invisible(NULL))

  if (is.data.frame(pieces[[1]])) return(dplyr::bind_rows(pieces))

  # A named list of tibbles: bind each element across strata.
  element_names <- unique(unlist(lapply(pieces, names)))
  out <- lapply(element_names, function(nm) {
    chunks <- Filter(Negate(is.null), lapply(pieces, function(p) p[[nm]]))
    if (length(chunks) == 0) tibble::tibble() else dplyr::bind_rows(chunks)
  })
  names(out) <- element_names
  out
}


#' Put a stratum's grouping columns at the front of a check's result
#'
#' @param res A tibble, or a named list of tibbles.
#' @param key_row One-row data frame of grouping values.
#' @return \code{res} with the grouping columns prepended.
#' @keywords internal
#' @noRd
prepend_stratum_keys <- function(res, key_row) {
  add <- function(tbl) {
    if (is.null(tbl) || NROW(tbl) == 0) return(tbl)
    keys <- key_row[rep(1L, NROW(tbl)), , drop = FALSE]
    rownames(keys) <- NULL
    dplyr::bind_cols(tibble::as_tibble(keys), tibble::as_tibble(tbl))
  }
  if (is.data.frame(res)) return(add(res))
  lapply(res, add)
}


#' Reciprocal condition number of a fit's slope covariance matrix
#'
#' A Wald test inverts the covariance matrix of the coefficients it tests, so its
#' statistic is only as trustworthy as that matrix's conditioning. The marginal
#' standard errors on the diagonal can look entirely reasonable while the matrix as
#' a whole is near-singular, in which case the inverse is dominated by numerical
#' noise and the statistic can come out arbitrarily large.
#'
#' The classical F statistic is computed from residual sums of squares rather than
#' by inversion, which is why it stays sane on the same fit and why a disagreement
#' between the two is the signature of this problem rather than of real imbalance.
#'
#' @param fit A fitted model.
#' @param drop_intercept Exclude the intercept row and column (default
#'   \code{TRUE}), since the intercept is not part of the joint hypothesis.
#' @return Reciprocal condition number in [0, 1], where small means badly
#'   conditioned, or \code{NA_real_} when the matrix cannot be read.
#' @keywords internal
#' @noRd
covariance_rcond <- function(fit, drop_intercept = TRUE) {
  v <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(v) || !is.matrix(v) || nrow(v) < 1) return(NA_real_)
  if (drop_intercept) {
    nm <- gsub("`", "", rownames(v), fixed = TRUE)
    keep <- is.null(nm) | nm != "(Intercept)"
    v <- v[keep, keep, drop = FALSE]
  }
  if (nrow(v) < 1 || anyNA(v)) return(NA_real_)
  out <- tryCatch(1 / kappa(v, exact = TRUE), error = function(e) NA_real_)
  if (!is.finite(out)) return(NA_real_)
  out
}


#' Classical omnibus F p-value, computed without inverting anything
#'
#' The classical F comes from a comparison of residual sums of squares, so unlike a
#' Wald statistic it never inverts the coefficient covariance matrix and stays
#' finite and sane on a fit whose covariance matrix is near-singular. That makes it
#' a free sanity check on the reported Wald p-value: the two answer the same
#' question and cannot legitimately differ by many orders of magnitude.
#'
#' Weights are ignored, so on a weighted fit this is an approximation. It is used
#' only to decide whether the Wald p-value is credible, never reported as a result.
#'
#' @param fit A fitted model, whose point estimates are assumed to be (weighted) OLS.
#' @param y The response vector the fit was computed on.
#' @return A p-value, or \code{NA_real_} when it cannot be computed.
#' @keywords internal
#' @noRd
classical_f_pvalue <- function(fit, y) {
  fv <- tryCatch(stats::fitted.values(fit), error = function(e) NULL)
  if (is.null(fv) || length(fv) == 0) return(NA_real_)
  # A fit drops incomplete rows, so align on the rows it actually used.
  if (length(fv) != length(y)) {
    idx <- suppressWarnings(as.integer(names(fv)))
    if (anyNA(idx) || length(idx) != length(fv)) return(NA_real_)
    y <- y[idx]
  }
  ok <- is.finite(fv) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  y <- y[ok]; fv <- fv[ok]
  q <- model_df1(fit)
  n <- length(y)
  df_res <- n - q - 1L
  if (is.na(q) || q < 1L || df_res < 1L) return(NA_real_)
  rss <- sum((y - fv)^2)
  tss <- sum((y - mean(y))^2)
  if (!is.finite(rss) || !is.finite(tss) || tss <= 0 || rss <= 0) return(NA_real_)
  f_stat <- ((tss - rss) / q) / (rss / df_res)
  if (!is.finite(f_stat) || f_stat < 0) return(NA_real_)
  stats::pf(f_stat, q, df_res, lower.tail = FALSE)
}


#' Numerator degrees of freedom of a fitted model's omnibus test
#'
#' The number of estimated non-intercept coefficients. Reads the coefficient
#' vector rather than \code{fit$k}, which exists only on \code{estimatr} fits: with
#' \code{.method = stats::lm}, \code{fit$k} is \code{NULL}, \code{NULL - 1L} is
#' \code{integer(0)}, and a zero-length column silently collapses the surrounding
#' \code{tibble()} to zero rows, so every test quietly disappeared rather than
#' erroring.
#'
#' Counting non-\code{NA} coefficients also handles rank deficiency correctly. An
#' aliased term carries an \code{NA} coefficient and estimates nothing, so it
#' should not be charged a degree of freedom, whereas \code{fit$k} counts it.
#'
#' @param fit A fitted model.
#' @return Integer, or \code{NA_integer_} when the coefficients cannot be read.
#' @keywords internal
#' @noRd
model_df1 <- function(fit) {
  b <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(b)) return(NA_integer_)
  nm <- names(b)
  keep <- !is.na(b)
  if (!is.null(nm)) keep <- keep & gsub("`", "", nm, fixed = TRUE) != "(Intercept)"
  as.integer(sum(keep))
}


#' Does a vector take more than one non-missing value?
#'
#' Guard for tests whose dependent variable may be constant. A missingness
#' indicator that is all zeros (nobody dropped out) or all ones (everybody did)
#' supports no test at all, and reporting such a case as a p-value of 1 would
#' put a spike at 1 into the uniform-reference diagnostics.
#'
#' @param x A vector.
#' @return \code{TRUE} when \code{x} has at least two distinct non-missing values.
#' @keywords internal
#' @noRd
has_variation <- function(x) {
  length(unique(x[!is.na(x)])) > 1L
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
    nobs      = as.integer(n),
    estimable = FALSE
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
    nobs      = as.integer(n),
    estimable = TRUE
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
    nobs      = as.integer(n),
    estimable = TRUE
  )
}
