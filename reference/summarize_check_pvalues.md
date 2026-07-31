# Summarize a set of design-check p-values against the uniform reference

Under valid randomization and no differential attrition, balance and
attrition test p-values are distributed Uniform(0, 1). Departures from
uniformity, and in particular an excess of small p-values, are evidence
that something is wrong with the design or the analyzed sample.

## Usage

``` r
summarize_check_pvalues(x, p_col = "p_value", group = NULL, alpha = 0.05)
```

## Arguments

- x:

  A data frame of test results, typically one element of the list
  returned by
  [`stack_checks`](https://alexandercoppock.com/excheckr/reference/stack_checks.md).

- p_col:

  Character scalar naming the p-value column (default `"p_value"`).

- group:

  Optional character scalar naming a column to adjust within. When
  supplied, the FDR adjustment is applied separately within each level
  of that column (e.g. `group = "study_id"` for a within-study
  adjustment); the returned summary is still a single row across all
  tests. When `NULL` (default) the adjustment is applied across all
  tests at once. The two answer different questions, so the choice
  belongs at the call site.

- alpha:

  Rejection threshold (default `0.05`).

## Value

A one-row tibble with columns `n_tests`, `n_dropped`, `n_below`,
`pct_below`, `expected_below`, `n_below_fdr`, `pct_below_fdr`, and
`ks_p`.

## Details

Reports the raw rejection rate, the rejection rate after a
Benjamini-Hochberg false-discovery-rate adjustment, and a
Kolmogorov-Smirnov test of the null that the p-values are uniform.

## Tests that could not be run

Rows with a missing p-value are excluded, and `n_dropped` counts them.
Read it: a large `n_dropped` means `n_tests` describes a subsample
selected on estimability rather than the whole collection, and the
summary says nothing about the studies that dropped out. This is common
in practice. The joint balance test relies on
[`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html), which
fails to converge on small strata, and outcomes with no attrition at all
support no attrition test; in one real corpus of 1143 joint balance
tests, 802 were unestimable.

## What ks_p does and does not tell you

The Kolmogorov-Smirnov test assumes the p-values are independent, and
design checks usually are not. Within a study, the indicators of one
factor covariate are mechanically dependent, correlated covariates add
more, and the joint test is a function of all of them. A small `ks_p` on
a stacked collection is therefore evidence about the collection's shape
but not a calibrated test, and dependence rather than a design problem
is the first thing to suspect. The comparison of `pct_below` against
`expected_below` is the more robust headline.

## See also

Other across-study summaries:
[`plot_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/plot_check_pvalues.md),
[`report_checks()`](https://alexandercoppock.com/excheckr/reference/report_checks.md),
[`stack_checks()`](https://alexandercoppock.com/excheckr/reference/stack_checks.md)

## Examples

``` r
set.seed(1)
tests <- data.frame(study_id = rep(letters[1:5], each = 20),
                    p_value = runif(100))
summarize_check_pvalues(tests)
#> # A tibble: 1 × 8
#>   n_tests n_dropped n_below pct_below expected_below n_below_fdr pct_below_fdr
#>     <int>     <int>   <int>     <dbl>          <dbl>       <int>         <dbl>
#> 1     100         0       2         2              5           0             0
#> # ℹ 1 more variable: ks_p <dbl>
summarize_check_pvalues(tests, group = "study_id")
#> # A tibble: 1 × 8
#>   n_tests n_dropped n_below pct_below expected_below n_below_fdr pct_below_fdr
#>     <int>     <int>   <int>     <dbl>          <dbl>       <int>         <dbl>
#> 1     100         0       2         2              5           0             0
#> # ℹ 1 more variable: ks_p <dbl>
```
