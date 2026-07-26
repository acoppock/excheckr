#' Check that outcome variables are within [0, 1]
#'
#' Summarises the min and max of each outcome variable and flags any that
#' fall outside [0, 1].
#'
#' When \code{outcomes} is omitted, all columns whose names start with
#' \code{prefix} are selected, excluding any that end with \code{"_missing"}
#' or \code{"_s"} (standardised versions). Supply \code{outcomes} to override
#' this default, or \code{exclude} to drop additional columns from whatever
#' was selected.
#'
#' @param data A data frame or tibble.
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column is appended to the returned tibble.
#' @param outcomes Columns to check. Supply unquoted names or tidyselect
#'   helpers (e.g., \code{starts_with("outcome_")}), or a character vector of
#'   column names. If omitted, all columns starting with \code{prefix} are
#'   used (excluding \code{"_missing"} and \code{"_s"} suffixes).
#' @param prefix Character string. Prefix used to auto-select outcome columns
#'   when \code{outcomes} is omitted (default: \code{"Y_"}).
#' @param exclude Additional columns to drop from the selection. Supply
#'   unquoted names or tidyselect helpers (e.g., \code{ends_with("_raw")}),
#'   or a character vector of exact column names. Applied after
#'   \code{outcomes} is resolved. \code{NULL} (the default) means no
#'   additional exclusions.
#'
#' @return A tibble with columns \code{variable}, \code{min}, \code{max},
#'   \code{in_bounds}, and optionally \code{study_id}.
#'
#' @examples
#' dat <- data.frame(Y_support = c(0, 0.5, 1), Y_oppose = c(0, 1.2, 0.8))
#' check_y_bounds(dat)
#' check_y_bounds(dat, study_id = "my_study")
#' check_y_bounds(dat, outcomes = "Y_support")
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @family per-study checks
#' @export
check_y_bounds <- function(data, study_id = NULL, outcomes = NULL,
                           prefix = "Y_", exclude = NULL) {
  # Resolve inclusions
  outcomes_quo <- rlang::enquo(outcomes)
  if (rlang::quo_is_null(outcomes_quo)) {
    varnames <- auto_select_vars(data, prefix = prefix,
                                 exclude_suffixes = c("_missing", "_s"))
  } else {
    val <- tryCatch(rlang::eval_tidy(outcomes_quo), error = function(e) NULL)
    if (is.character(val)) {
      varnames <- val
    } else {
      varnames <- names(eval_select(outcomes_quo, data))
    }
  }

  # Resolve exclusions
  excl_quo <- rlang::enquo(exclude)
  if (!rlang::quo_is_null(excl_quo)) {
    excl_val <- tryCatch(rlang::eval_tidy(excl_quo), error = function(e) NULL)
    if (is.character(excl_val)) {
      varnames <- setdiff(varnames, excl_val)
    } else {
      varnames <- setdiff(varnames, names(eval_select(excl_quo, data)))
    }
  }

  if (length(varnames) == 0) {
    warning("No outcome variables found.")
    return(invisible(NULL))
  }

  result <- tibble::tibble(
    variable = varnames,
    min = vapply(varnames, function(v)
      min(suppressWarnings(as.numeric(as.character(data[[v]]))), na.rm = TRUE), numeric(1)),
    max = vapply(varnames, function(v)
      max(suppressWarnings(as.numeric(as.character(data[[v]]))), na.rm = TRUE), numeric(1))
  ) |>
    dplyr::mutate(in_bounds = min >= 0 & max <= 1)

  if (!is.null(study_id)) result$study_id <- study_id

  result
}


#' Check covariate missingness and imputed-version coverage
#'
#' For each base covariate, reports the number and percentage of missing
#' values, whether an imputed companion column exists (identified by
#' \code{nona_suffix}), and whether that companion itself contains any
#' missing values.
#'
#' When \code{covariates} is omitted, all columns whose names start with
#' \code{prefix} are selected, excluding any that end with
#' \code{nona_suffix} or \code{"_missing"}. Supply \code{covariates} to
#' override this default, or \code{exclude} to drop additional columns from
#' whatever was selected.
#'
#' @param data A data frame or tibble.
#' @param study_id Optional character scalar. If provided, a \code{study_id}
#'   column is appended to the returned tibble.
#' @param covariates Columns to check. Supply unquoted names or tidyselect
#'   helpers (e.g., \code{starts_with("cov_")}), or a character vector of
#'   column names. If omitted, all columns starting with \code{prefix} are
#'   used (excluding \code{nona_suffix} and \code{"_missing"} suffixes).
#' @param prefix Character string. Prefix used to auto-select covariate
#'   columns when \code{covariates} is omitted (default: \code{"X_"}).
#' @param nona_suffix Character string. Suffix that identifies imputed
#'   companion columns (default: \code{"_nona"}). Used both to exclude
#'   companion columns from the base selection and to look them up when
#'   checking \code{nona_has_na}.
#' @param exclude Additional columns to drop from the selection. Supply
#'   unquoted names or tidyselect helpers (e.g., \code{ends_with("_old")}),
#'   or a character vector of exact column names. Applied after
#'   \code{covariates} is resolved. \code{NULL} (the default) means no
#'   additional exclusions.
#'
#' @return A tibble with columns \code{variable}, \code{n_missing},
#'   \code{pct_missing}, \code{has_nona_version}, \code{nona_has_na}, and
#'   optionally \code{study_id}. \code{nona_has_na} is \code{NA} when no
#'   companion column exists.
#'
#' @examples
#' dat <- data.frame(
#'   X_age      = c(25, NA, 30),
#'   X_age_nona = c(25, 27, 30),
#'   X_income   = c(NA, 50, 60)
#' )
#' check_missingness_nona(dat)
#' check_missingness_nona(dat, study_id = "my_study")
#' check_missingness_nona(dat, covariates = "X_age")
#' check_missingness_nona(dat, nona_suffix = "_imputed")
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @importFrom tibble tibble
#' @family per-study checks
#' @export
check_missingness_nona <- function(data, study_id = NULL, covariates = NULL,
                                   prefix = "X_", nona_suffix = "_nona",
                                   exclude = NULL) {
  all_vars <- names(data)

  # Resolve inclusions
  covariates_quo <- rlang::enquo(covariates)
  if (rlang::quo_is_null(covariates_quo)) {
    varnames <- auto_select_vars(data, prefix = prefix,
                                 exclude_suffixes = c(nona_suffix, "_missing"))
  } else {
    val <- tryCatch(rlang::eval_tidy(covariates_quo), error = function(e) NULL)
    if (is.character(val)) {
      varnames <- val
    } else {
      varnames <- names(eval_select(covariates_quo, data))
    }
  }

  # Resolve exclusions
  excl_quo <- rlang::enquo(exclude)
  if (!rlang::quo_is_null(excl_quo)) {
    excl_val <- tryCatch(rlang::eval_tidy(excl_quo), error = function(e) NULL)
    if (is.character(excl_val)) {
      varnames <- setdiff(varnames, excl_val)
    } else {
      varnames <- setdiff(varnames, names(eval_select(excl_quo, data)))
    }
  }

  if (length(varnames) == 0) {
    warning("No base covariate variables found.")
    return(invisible(NULL))
  }

  nona_has_na_safe <- function(v) {
    nona <- paste0(v, nona_suffix)
    if (nona %in% all_vars) any(is.na(data[[nona]])) else NA
  }

  result <- tibble::tibble(
    variable         = varnames,
    n_missing        = vapply(varnames, function(v) sum(is.na(data[[v]])),  integer(1)),
    pct_missing      = vapply(varnames, function(v) mean(is.na(data[[v]])), numeric(1)),
    has_nona_version = vapply(varnames, function(v) paste0(v, nona_suffix) %in% all_vars, logical(1)),
    nona_has_na      = vapply(varnames, nona_has_na_safe, logical(1))
  )

  if (!is.null(study_id)) result$study_id <- study_id

  result
}
