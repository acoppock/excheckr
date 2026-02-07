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