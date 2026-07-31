# Triage a stacked set of design checks

Applies the standard "what needs a human look" filters to the output of
[`stack_checks`](https://alexandercoppock.com/excheckr/reference/stack_checks.md)
and returns only the rows that failed. Elements absent from `checks` are
skipped, so the same call works whether the pipeline ran
[`check_attrition`](https://alexandercoppock.com/excheckr/reference/check_attrition.md),
an `attrition_lasso` element from
[`estimatrTools::check_attrition_lasso`](https://alexandercoppock.com/estimatrTools/reference/check_attrition_lasso.html),
or neither.

## Usage

``` r
report_checks(checks, alpha = 0.05)
```

## Arguments

- checks:

  A named list of stacked check tibbles, as returned by
  [`stack_checks`](https://alexandercoppock.com/excheckr/reference/stack_checks.md).

- alpha:

  Rejection threshold for the balance and attrition filters (default
  `0.05`).

## Value

An object of class `"excheckr_report"`: a named list of tibbles holding
only the failing rows, with an `alpha` attribute. Elements with nothing
to report are present but empty. Has a `print` method that shows the
counts.

## Details

The filters are:

- out_of_bounds:

  `ybounds` rows where `in_bounds` is `FALSE`: an outcome outside the
  expected \[0, 1\] range.

- missing_no_nona:

  `missingness` rows with missing values and no imputed companion
  column: covariates that will silently drop rows from every adjusted
  model.

- nona_still_missing:

  `missingness` rows whose imputed companion column still contains `NA`:
  the imputation did not take.

- balance_joint:

  `balance_joint` rows with `p_value` at or below `alpha`. A single
  `balance` element produced by `check_balance(flatten = TRUE)` is split
  on its `test` column and handled the same way.

- balance_covariate:

  `balance_covariate` rows with `p_value` at or below `alpha`. Expect
  roughly `alpha` of these to fail by chance; use
  [`summarize_check_pvalues`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)
  to judge whether there are more than chance would give.

- attrition:

  `attrition` or `attrition_lasso` rows flagged at `alpha`. When the
  element carries a logical `flag` column, that column is used instead
  and `alpha` is ignored, because the producer already applied a
  threshold of its own.

## See also

Other across-study summaries:
[`plot_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/plot_check_pvalues.md),
[`stack_checks()`](https://alexandercoppock.com/excheckr/reference/stack_checks.md),
[`summarize_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)

## Examples

``` r
checks <- list(
  ybounds = data.frame(study_id = c("a", "b"), variable = "Y_x",
                       min = c(0, -1), max = c(1, 3),
                       in_bounds = c(TRUE, FALSE)),
  balance_joint = data.frame(study_id = c("a", "b"), p_value = c(0.4, 0.01))
)
report_checks(checks)
#> excheckr triage report (alpha = 0.05)
#>       1  outcomes outside [0, 1]
#>       1  joint balance tests flagged
#> Inspect an element with report$<name>.
```
