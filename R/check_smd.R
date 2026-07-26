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
#'   Defaults to the first factor level, or the smallest value for numeric
#'   treatments.
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column holding this value is appended to the result.
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
                      study_id = NULL, threshold = 0.1) {
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
      mean_ref <- mean(x[ref_rows], na.rm = TRUE)
      sd_ref <- stats::sd(x[ref_rows], na.rm = TRUE)

      dplyr::bind_rows(lapply(other_arms, function(a) {
        arm_rows <- !is.na(z) & z == a
        mean_arm <- mean(x[arm_rows], na.rm = TRUE)
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
