#' @keywords internal
"_PACKAGE"

#' @section Checking one study:
#' Four checks answer the questions worth asking of a single cleaned
#' experimental dataset, and each accepts a \code{study_id} so its output can be
#' stacked later:
#' \itemize{
#'   \item \code{\link{check_y_bounds}}: are the outcomes on the scale you think
#'     they are on?
#'   \item \code{\link{check_missingness_nona}}: which covariates have missing
#'     values, and do they have an imputed companion column?
#'   \item \code{\link{check_balance}}: does treatment predict the covariates,
#'     covariate by covariate and jointly? \code{\link{check_smd}} answers the
#'     companion question of whether an imbalance is large enough to matter.
#'   \item \code{\link{check_attrition}}: does treatment predict outcome
#'     missingness, on its own and allowing the pattern to differ across
#'     covariates?
#' }
#' For designs where the fully interacted attrition test runs out of degrees of
#' freedom, \code{estimatrTools::check_attrition_lasso} selects a parsimonious
#' covariate set first. It lives there rather than here because it fits an
#' estimator of its own, and this package only ever calls estimators that other
#' packages own.
#'
#' @section Checking many studies:
#' A meta-analysis or multi-study project runs those checks once per study and
#' then has to make sense of hundreds of tests at once.
#' \code{\link{stack_checks}} reads the per-study files and binds them,
#' \code{\link{report_checks}} returns only the rows that need a human, and
#' \code{\link{summarize_check_pvalues}} and \code{\link{plot_check_pvalues}}
#' compare the resulting p-values against the Uniform(0, 1) distribution they
#' should follow when nothing is wrong. See
#' \code{vignette("checking_many_studies", package = "excheckr")}.
#'
#' @section A caution about reading these checks:
#' Balance and attrition tests are diagnostics, not decisions. Under a valid
#' design their p-values are uniform, so roughly \code{alpha} of them will be
#' below \code{alpha} by construction: a handful of flags in a large collection
#' is what success looks like, not evidence of a problem. That is why
#' \code{\link{summarize_check_pvalues}} reports the whole distribution rather
#' than a count, and why dropping studies on the strength of a single flagged
#' test is a good way to introduce the bias you were checking for.
#'
#' @section Other tools:
#' \code{\link{check_schema}} and its \code{assert_*} companions check that a
#' cleaned dataset has the shape a pipeline expects, rather than that the
#' experiment behind it was sound. The \code{write_*_code} functions emit
#' copy-pasteable cleaning and checking code. \code{\link{stat_mode}} computes a
#' modal value for mode imputation, and \code{\link{scale_by_control}} puts
#' outcomes on a control-group-SD scale.
#'
#' @name excheckr-package
NULL
