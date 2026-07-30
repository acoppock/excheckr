#' Standardized mean differences between treatment arms
#'
#' Reports, for each covariate and each non-reference treatment arm, the
#' difference in means against the reference arm divided by the reference arm's
#' standard deviation. Standardizing by the reference (control) SD rather than
#' the pooled SD keeps the denominator fixed across arms, so the values are
#' comparable to each other and are not moved by treatment-induced changes in
#' variance.
#'
#' Standardized mean differences complement the p-values from
#' \code{\link{check_balance}}: a p-value answers "is this difference bigger
#' than chance", an SMD answers "is it big enough to matter". In a large sample
#' a trivial imbalance can be significant, and in a small one a substantial
#' imbalance can fail to reach significance.
#'
#' Factor and character covariates are expanded to one indicator per
#' non-reference level and each indicator is reported separately.
#'
#' @param data A data frame or tibble.
#' @param treatment Unquoted name of the treatment variable.
#' @param covariates Character vector of covariate names, or unquoted column
#'   names using tidyselect helpers. If omitted, all \code{"X_"} columns are
#'   used.
#' @param reference Value of \code{treatment} to use as the reference arm.
#'   Defaults to the first factor level, or the smallest value for numeric and
#'   character treatments. That default is a guess based on ordering, not on
#'   meaning: if the control arm is not the first level, every difference is
#'   computed against a treatment arm instead and every sign flips. The arm
#'   actually used is reported in the \code{reference} column of the result, so
#'   check it rather than assume it, and pass \code{reference} explicitly when
#'   the control arm is not first.
#' @param weights Optional character scalar naming a column of survey weights.
#'   When supplied, the arm means and the reference SD are computed on that
#'   weighted basis. Supply it whenever the balance tests beside this call are
#'   weighted, since an unweighted SMD and a weighted p-value describe two
#'   different samples and reading them together will mislead.
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column holding this value is appended to the result.
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
#' @param threshold Absolute SMD above which \code{flag} is \code{TRUE}
#'   (default \code{0.1}, a common rule of thumb).
#'
#' @return A tibble with one row per covariate-level-by-arm contrast and
#'   columns \code{covariate}, \code{level}, \code{arm}, \code{reference},
#'   \code{n_arm}, \code{n_reference}, \code{mean_arm}, \code{mean_reference},
#'   \code{sd_reference}, \code{smd}, and \code{flag}.
#'
#' @examples
#' set.seed(42)
#' dat <- data.frame(
#'   Z = rep(c(0L, 1L), 100),
#'   X_age = rnorm(200, 50, 10),
#'   X_party = factor(sample(c("D", "R", "I"), 200, replace = TRUE))
#' )
#' dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)
#' check_smd(dat, Z)
#'
#' @importFrom dplyr bind_rows
#' @importFrom rlang ensym as_name enquo quo_is_null eval_tidy
#' @importFrom tidyselect eval_select
#' @importFrom tibble tibble
#' @importFrom stats sd
#' @family per-study checks
#' @export
check_smd <- function(data, treatment, covariates = NULL, reference = NULL,
                      weights = NULL, study_id = NULL, threshold = 0.1,
                      .by = NULL) {
  treatment_name <- rlang::as_name(rlang::ensym(treatment))

  covariates_quo <- rlang::enquo(covariates)
  if (rlang::quo_is_null(covariates_quo)) {
    varnames <- auto_select_vars(data, prefix = "X_")
  } else {
    char_val <- tryCatch(rlang::eval_tidy(covariates_quo), error = function(e) NULL)
    if (is.character(char_val)) {
      varnames <- if (length(char_val) > 0) char_val else auto_select_vars(data, prefix = "X_")
    } else {
      varnames <- names(eval_select(covariates_quo, data))
    }
  }

  if (length(varnames) == 0) {
    warning("No covariates selected for SMD check.")
    return(invisible(NULL))
  }

  # Error rather than quietly returning one fewer row. A covariate list that has
  # drifted from the data is the common cause, and silently reporting SMDs for a
  # smaller set than the caller asked about misstates the check.
  absent <- setdiff(varnames, names(data))
  if (length(absent) > 0) {
    stop("check_smd: covariate(s) not found in ", deparse(substitute(data)), ": ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  }

  # Stratified run, with the covariate list already resolved to names.
  by_quo <- rlang::enquo(.by)
  if (!rlang::quo_is_null(by_quo)) {
    varnames <- setdiff(varnames, by_column_names(data, by_quo))
    if (length(varnames) == 0) {
      warning("No covariates left for SMD check after removing .by columns.")
      return(invisible(NULL))
    }
    return(run_by_strata(data, by_quo, function(d) {
      check_smd(d, !!treatment_name, covariates = varnames, reference = reference,
                weights = weights, study_id = study_id, threshold = threshold)
    }))
  }

  if (is.null(weights)) {
    w <- rep(1, nrow(data))
  } else {
    if (!weights %in% names(data)) {
      stop("check_smd: weights column '", weights, "' not found.", call. = FALSE)
    }
    w <- data[[weights]]
  }

  z <- data[[treatment_name]]
  arms <- if (is.factor(z)) levels(z) else sort(unique(z[!is.na(z)]))

  if (is.null(reference)) {
    reference <- arms[1]
  } else if (!reference %in% arms) {
    stop("check_smd: reference value '", reference, "' not found in ",
         treatment_name, ".")
  }
  other_arms <- setdiff(arms, reference)

  if (length(other_arms) == 0) {
    stop("check_smd: ", treatment_name, " has only one arm.")
  }

  ref_rows <- !is.na(z) & z == reference

  results <- lapply(varnames, function(v) {
    col <- data[[v]]

    if (is.numeric(col)) {
      pieces <- list(list(level = NA_character_, values = col))
    } else {
      if (!is.factor(col)) col <- as.factor(col)
      levs <- levels(col)[-1]
      pieces <- lapply(levs, function(lev) {
        list(level = lev, values = as.integer(col == lev))
      })
    }

    dplyr::bind_rows(lapply(pieces, function(piece) {
      x <- piece$values
      mean_ref <- weighted_mean(x, w, ref_rows)
      sd_ref <- weighted_sd(x, w, ref_rows)

      dplyr::bind_rows(lapply(other_arms, function(a) {
        arm_rows <- !is.na(z) & z == a
        mean_arm <- weighted_mean(x, w, arm_rows)
        smd <- if (is.na(sd_ref) || sd_ref == 0) NA_real_ else (mean_arm - mean_ref) / sd_ref
        tibble::tibble(
          covariate      = v,
          level          = piece$level,
          arm            = as.character(a),
          reference      = as.character(reference),
          n_arm          = sum(arm_rows & !is.na(x)),
          n_reference    = sum(ref_rows & !is.na(x)),
          mean_arm       = mean_arm,
          mean_reference = mean_ref,
          sd_reference   = sd_ref,
          smd            = smd,
          flag           = !is.na(smd) & abs(smd) > threshold
        )
      }))
    }))
  })

  result <- dplyr::bind_rows(results)
  if (!is.null(study_id)) result$study_id <- study_id
  result
}
