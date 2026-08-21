utils::globalVariables(c(
  "p_value", "p_adjusted", "adjustment", "n_tests", "study_id", "in_bounds",
  "n_missing", "has_nona_version", "nona_has_na", "variable", "outcome"
))

#' Summarize a set of design-check p-values against the uniform reference
#'
#' Under valid randomization and no differential attrition, balance and
#' attrition test p-values are distributed Uniform(0, 1). Departures from
#' uniformity, and in particular an excess of small p-values, are evidence that
#' something is wrong with the design or the analyzed sample.
#'
#' Reports the raw rejection rate, the rejection rate after a Benjamini-Hochberg
#' false-discovery-rate adjustment, and a Kolmogorov-Smirnov test of the null
#' that the p-values are uniform.
#'
#' @param x A data frame of test results, typically one element of the list
#'   returned by \code{\link{stack_checks}}.
#' @param p_col Character scalar naming the p-value column (default
#'   \code{"p_value"}).
#' @param group Optional character scalar naming a column to adjust within.
#'   When supplied, the FDR adjustment is applied separately within each level
#'   of that column (e.g. \code{group = "study_id"} for a within-study
#'   adjustment); the returned summary is still a single row across all tests.
#'   When \code{NULL} (default) the adjustment is applied across all tests at
#'   once. The two answer different questions, so the choice belongs at the
#'   call site.
#' @param alpha Rejection threshold (default \code{0.05}).
#'
#' @return A one-row tibble with columns \code{n_tests}, \code{n_dropped},
#'   \code{n_below}, \code{pct_below}, \code{expected_below}, \code{n_below_fdr},
#'   \code{pct_below_fdr}, and \code{ks_p}.
#'
#' @section Tests that could not be run:
#' Rows with a missing p-value are excluded, and \code{n_dropped} counts them.
#' Read it: a large \code{n_dropped} means \code{n_tests} describes a subsample
#' selected on estimability rather than the whole collection, and the summary
#' says nothing about the studies that dropped out. This is common in practice.
#' The joint balance test relies on \code{nnet::multinom}, which fails to
#' converge on small strata, and outcomes with no attrition at all support no
#' attrition test; in one real corpus of 1143 joint balance tests, 802 were
#' unestimable.
#'
#' @section What ks_p does and does not tell you:
#' The Kolmogorov-Smirnov test assumes the p-values are independent, and design
#' checks usually are not. Within a study, the indicators of one factor covariate
#' are mechanically dependent, correlated covariates add more, and the joint test
#' is a function of all of them. A small \code{ks_p} on a stacked collection is
#' therefore evidence about the collection's shape but not a calibrated test, and
#' dependence rather than a design problem is the first thing to suspect. The
#' comparison of \code{pct_below} against \code{expected_below} is the more robust
#' headline.
#'
#' @examples
#' set.seed(1)
#' tests <- data.frame(study_id = rep(letters[1:5], each = 20),
#'                     p_value = runif(100))
#' summarize_check_pvalues(tests)
#' summarize_check_pvalues(tests, group = "study_id")
#'
#' @importFrom tibble tibble
#' @importFrom stats p.adjust ks.test
#' @family across-study summaries
#' @export
summarize_check_pvalues <- function(x, p_col = "p_value", group = NULL, alpha = 0.05) {
  p <- extract_pvalues(x, p_col)

  if (is.null(group)) {
    p_adj <- stats::p.adjust(p$p_value, method = "BH")
  } else {
    if (!group %in% names(x)) {
      stop("summarize_check_pvalues: column '", group, "' not found in x.")
    }
    g <- x[[group]][p$keep]
    p_adj <- rep(NA_real_, length(p$p_value))
    for (lev in unique(g)) {
      idx <- which(g == lev)
      p_adj[idx] <- stats::p.adjust(p$p_value[idx], method = "BH")
    }
  }

  n <- length(p$p_value)

  tibble::tibble(
    n_tests        = n,
    n_dropped      = p$n_dropped,
    n_below        = sum(p$p_value <= alpha),
    pct_below      = 100 * mean(p$p_value <= alpha),
    expected_below = 100 * alpha,
    n_below_fdr    = sum(p_adj <= alpha),
    pct_below_fdr  = 100 * mean(p_adj <= alpha),
    ks_p           = suppressWarnings(stats::ks.test(p$p_value, "punif")$p.value)
  )
}


#' Plot design-check p-values against the uniform reference
#'
#' Histogram of test p-values with a dotted line at the Uniform(0, 1)
#' expectation and a dashed line at \code{alpha}. A flat histogram sitting on
#' the reference line, with roughly \code{alpha} of the mass below the dashed
#' line, is direct evidence that the analyzed sample is sound.
#'
#' @inheritParams summarize_check_pvalues
#' @param binwidth Histogram bin width (default \code{0.05}, giving 20 bins).
#' @param fdr Logical. When \code{TRUE}, the plot is faceted into unadjusted
#'   and Benjamini-Hochberg-adjusted panels (default \code{FALSE}).
#' @param xlab Axis label for the p-value axis.
#'
#' @return A \code{ggplot} object. Deliberately unthemed beyond
#'   \code{theme_minimal()} so that a project theme can be added to it.
#'
#' @examples
#' set.seed(1)
#' tests <- data.frame(study_id = rep(letters[1:5], each = 20),
#'                     p_value = runif(100))
#' plot_check_pvalues(tests)
#' plot_check_pvalues(tests, group = "study_id", fdr = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_hline geom_vline scale_x_continuous labs theme_minimal facet_wrap
#' @family across-study summaries
#' @export
plot_check_pvalues <- function(x, p_col = "p_value", group = NULL, alpha = 0.05,
                               binwidth = 0.05, fdr = FALSE,
                               xlab = "p-value") {
  p <- extract_pvalues(x, p_col)

  gg_df <- tibble::tibble(p_value = p$p_value, adjustment = "Unadjusted")

  if (fdr) {
    if (is.null(group)) {
      p_adj <- stats::p.adjust(p$p_value, method = "BH")
      adj_label <- "FDR-adjusted (across all tests)"
    } else {
      if (!group %in% names(x)) {
        stop("plot_check_pvalues: column '", group, "' not found in x.")
      }
      g <- x[[group]][p$keep]
      p_adj <- rep(NA_real_, length(p$p_value))
      for (lev in unique(g)) {
        idx <- which(g == lev)
        p_adj[idx] <- stats::p.adjust(p$p_value[idx], method = "BH")
      }
      adj_label <- paste0("FDR-adjusted (within ", group, ")")
    }
    gg_df <- dplyr::bind_rows(
      gg_df,
      tibble::tibble(p_value = p_adj, adjustment = adj_label)
    )
    gg_df$adjustment <- factor(gg_df$adjustment,
                               levels = c("Unadjusted", adj_label))
  }

  n_per_panel <- length(p$p_value)

  out <- ggplot2::ggplot(gg_df, ggplot2::aes(x = p_value)) +
    ggplot2::geom_histogram(binwidth = binwidth, boundary = 0,
                            fill = "grey35", color = "white", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = n_per_panel * binwidth,
                        linetype = "dotted", color = "grey45", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = alpha,
                        linetype = "dashed", color = "grey40", linewidth = 0.6) +
    ggplot2::scale_x_continuous(name = xlab, breaks = seq(0, 1, by = 0.2),
                                limits = c(-binwidth / 2, 1 + binwidth / 2)) +
    ggplot2::labs(y = "Number of tests") +
    ggplot2::theme_minimal()

  if (fdr) {
    out <- out + ggplot2::facet_wrap(~ adjustment)
  }

  out
}


#' Extract and validate a p-value column
#'
#' @param x A data frame.
#' @param p_col Name of the p-value column.
#' @return A list with \code{p_value} (non-missing p-values), \code{keep}
#'   (logical index of retained rows in \code{x}), and \code{n_dropped} (the
#'   count of missing p-values, which callers report rather than swallow).
#' @keywords internal
#' @noRd
extract_pvalues <- function(x, p_col) {
  if (!is.data.frame(x)) {
    stop("Expected a data frame of test results, got ", class(x)[1], ".")
  }
  if (!p_col %in% names(x)) {
    stop("Column '", p_col, "' not found. Available columns: ",
         paste(names(x), collapse = ", "), ".")
  }
  p <- x[[p_col]]
  keep <- !is.na(p)
  if (!any(keep)) {
    stop("Column '", p_col, "' contains no non-missing p-values.")
  }
  list(p_value = p[keep], keep = keep, n_dropped = sum(!keep))
}
